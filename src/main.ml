type t_field_type =
  | VarChar
  | Date
  | Text
  | Set of string list

type t_field = 
  { f_name: string;
    f_alias: string;
    f_type: t_field_type;
    f_select: bool;
    f_autogenerate: bool }

type t_db =
  { db_name:string;
    db_alias:string;
    db_fields: t_field list }

let print_autogen_stubs out db : unit =
  let non_gen_fields = List.filter (fun f -> not f.f_autogenerate) db.db_fields in
  let params = String.concat "," (List.map (fun f -> "$"^f.f_name) non_gen_fields) in
  List.iter (fun f ->
      if f.f_autogenerate then
        Printf.fprintf out "
        private function _gen%s(%s){ return \"\"; }\n" (String.capitalize_ascii f.f_name) params
      else ()
    ) db.db_fields

let print_create_fn out db : unit =
  let non_gen_fields = List.filter (fun f -> not f.f_autogenerate) db.db_fields in
  let params = String.concat "," (List.map (fun f -> "$"^f.f_name) non_gen_fields) in
  let field_names = String.concat "," (List.map (fun f -> f.f_name) db.db_fields) in
  let qmarks = String.concat "," (List.map (fun f -> "?") db.db_fields) in
  let vars = String.concat "," (List.map (fun f -> "$"^f.f_name) db.db_fields) in
  Printf.fprintf out "
        public function create(%s){
                $sql = 'INSERT INTO `%s` (%s) VALUES (%s)';
                $stmt = $this->db->prepare($sql);"
    params db.db_name field_names qmarks;
  List.iter (fun f ->
      if f.f_autogenerate then
        Printf.fprintf out "
                $%s = _gen%s(%s);" f.f_name (String.capitalize_ascii f.f_name) params
    ) db.db_fields;
  Printf.fprintf out "
                return $stmt->execute(array(%s));
        }\n" vars

let print_update_fn out db : unit =
  let non_gen_fields = List.filter (fun f -> not f.f_autogenerate) db.db_fields in
  let params = String.concat "," (List.map (fun f -> "$"^f.f_name) non_gen_fields) in
  let field_eq_qmark = String.concat "," (List.map (fun f -> "`" ^ f.f_name ^ "` = ?" ) db.db_fields) in
  let vars = String.concat "," (List.map (fun f -> "$"^f.f_name) db.db_fields) in
  Printf.fprintf out "
        public function update($id,%s){
                $sql = 'UPDATE `%s` SET %s WHERE `id` = ?';
                $stmt = $this->db->prepare($sql);" params db.db_name field_eq_qmark;
  List.iter (fun f ->
      if f.f_autogenerate then
        Printf.fprintf out "
                $%s = _gen%s(%s);" f.f_name (String.capitalize_ascii f.f_name) vars
    ) db.db_fields;
  Printf.fprintf out "
                return $stmt->execute(array(%s,id));
        }\n" vars

let print_select_fns out db : unit =
  let aux f_select f_name =
    if f_select then
      Printf.fprintf out "
        public function select_by_%s($id){
                $sql = 'SELECT * FROM `%s` WHERE `%s` = ? LIMIT 1';
                $stmt = $this->db->prepare($sql);
                if($stmt->execute(array($id))){
                        return $stmt->fetch();
                } else {
                        return FALSE; 
                }
	}\n" f_name db.db_name f_name
    else ()
  in
  aux true "id";
  List.iter (fun f -> aux f.f_select f.f_name) db.db_fields

let print_delete_fn out db_name : unit =
  Printf.fprintf out "
        public function delete($id){
		$sql = 'DELETE FROM `%s` WHERE `id`= ?';
		$stmt = $this->db->prepare($sql);
		$stmt->execute(array($id));
	}" db_name

let print_search_fn out db : unit =
  Printf.fprintf out "
        public function search($criteria,$is_conj){;
                $kw = $is_conj? ' AND ' : ' OR ';
                $arr_col = array();
                $arr_val = array();";

  List.iter (fun f ->
      let col, vl = match f.f_type with
        | VarChar | Text -> ("'`"^f.f_name^"` LIKE ?'"), ("'%' . $criteria['"^f.f_name^"'] . '%'")
        | Date -> ("'`"^f.f_name^"` = ?'"), ("$criteria['"^f.f_name^"']")
        | Set _ -> ("'FIND_IN_SET(?,"^f.f_name^")'"), ("$criteria['"^f.f_name^"']")
      in
      Printf.fprintf out "
                if(isset($criteria['%s'])){
                        array_push($arr_col,%s);
                        array_push($arr_val,%s);
                }" f.f_name col vl
    ) db.db_fields;
  Printf.fprintf out "
                $sql = 'SELECT * FROM `%s` WHERE ' . join($kw,$arr_col) . ' ORDER BY `id` DESC';
		$stmt = $this->db->prepare($sql);
		$stmt->execute(array($arr_val));
                return $stmt->fetchAll();
        }" db.db_name

let print_model (db:t_db) : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let out = open_out (model_name ^ ".php") in
  Printf.fprintf out "<?php
class %s {
	private $db;
        private $edito;

        public function __construct(){
		$dsn = '';
		$user = '';
		$pass = '';
		try { $this->db = new PDO($dsn, $user, $pass); } 
		catch (PDOException $e) { die ('Cannot connect to the database.'); }
	}
" model_name;
  print_create_fn out db;
  print_select_fns out db;
  print_autogen_stubs out db;
  print_update_fn out db;
  print_delete_fn out db.db_name;
  print_search_fn out db;
  Printf.fprintf out "
}
?>"

let print_create_form db =
(*   let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in *)
  let out = open_out ("create_" ^ db.db_name ^ ".php") in
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Creation</title>
        <link rel=\"stylesheet\" href=\"pure/pure-min.css\" media=\"screen\">
    </head>
    <body>
        <main style=\"width:600px;margin:auto\">
        <form class=\"pure-form pure-form-stacked\">
            <fieldset>
                <legend>%s Creation</legend>" db.db_alias db.db_alias;
  List.iter (fun fd ->
      if not fd.f_autogenerate then
        match fd.f_type with
        | VarChar ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"text\" placeholder=\"%s\" required>"
            fd.f_name fd.f_alias fd.f_name fd.f_alias
        | Date ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"date\" required>"
            fd.f_name fd.f_alias fd.f_name
        | Text ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <textarea id=\"%s\" required>%s</textarea>"
            fd.f_name fd.f_alias fd.f_name fd.f_alias
        | Set elts ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <select id=\"%s\" multiple required>" fd.f_name fd.f_alias fd.f_name;
          List.iter (fun x ->
              Printf.fprintf out "
                <option>%s</option>" x
            ) elts;
          Printf.fprintf out "
            </select>"
    ) db.db_fields;
  Printf.fprintf out "
                <button type=\"submit\" class=\"pure-button pure-button-primary\">Create</button>
            </fieldset>
        </form>
        </main>
    </body>
</html>"

let print_update_form db =
  let out = open_out ("update_" ^ db.db_name ^ ".php") in
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Update</title>
        <link rel=\"stylesheet\" href=\"pure/pure-min.css\" media=\"screen\">
    </head>
    <body>
        <main style=\"width:600px;margin:auto\">
        <form class=\"pure-form pure-form-stacked\">
            <fieldset>
                <legend>%s Update</legend>
                <input id=\"id\" type=\"hidden\" placeholder=\"<?php=$arr['id'];?>\">"
    db.db_alias db.db_alias;
  List.iter (fun fd ->
      if not fd.f_autogenerate then
        match fd.f_type with
        | VarChar ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"text\" placeholder=\"<?php=$arr['%s'];?>\" required>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Date ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"date\" placeholder=\"<?php=$arr['%s'];?>\" required>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Text ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <textarea id=\"%s\" required><?php=$arr['%s'];?></textarea>"
            fd.f_name fd.f_alias fd.f_name fd.f_alias
        | Set elts ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <select id=\"%s\" multiple>
            <?php
                $tags = explode(\",\",$arr['%s']);"
            fd.f_name fd.f_alias fd.f_name fd.f_name;
          List.iter (fun x ->
              Printf.fprintf out "
                if(array_in('%s',$tags)){ echo '<option selected>%s</option>'; }
                else { echo '<option>%s</option>'; }"
                x x x
            ) elts;
          Printf.fprintf out "
            ?>
            </select>"
    ) db.db_fields;
  Printf.fprintf out "
                <button type=\"submit\" class=\"pure-button pure-button-primary\">Update</button>
            </fieldset>
        </form>
        </main>
    </body>
</html>"

let print_delete_form db =
  let out = open_out ("delete_" ^ db.db_name ^ ".php") in
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Deletion</title>
        <link rel=\"stylesheet\" href=\"pure/pure-min.css\" media=\"screen\">
    </head>
    <body>
        <main style=\"width:600px;margin:auto\">
        <form class=\"pure-form pure-form-stacked\">
            <fieldset>
                <legend>%s Deletion</legend>
                <input id=\"id\" type=\"hidden\" placeholder=\"<?php=$arr['id'];?>\">"
    db.db_alias db.db_alias;
  List.iter (fun fd ->
      if not fd.f_autogenerate then
        match fd.f_type with
        | VarChar ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"text\" placeholder=\"<?php=$arr['%s'];?>\" disabled>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Date ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"date\" placeholder=\"<?php=$arr['%s'];?>\" disabled>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Text ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <textarea id=\"%s\" disabled><?php=$arr['%s'];?></textarea>"
            fd.f_name fd.f_alias fd.f_name fd.f_alias
        | Set elts ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <select id=\"%s\" multiple disabled>
            <?php
                $tags = explode(\",\",$arr['%s']);"
            fd.f_name fd.f_alias fd.f_name fd.f_name;
          List.iter (fun x ->
              Printf.fprintf out "
                if(array_in('%s',$tags)){ echo '<option selected>%s</option>'; }
                else { echo '<option>%s</option>'; }"
                x x x
            ) elts;
          Printf.fprintf out "
            ?>
            </select>"
    ) db.db_fields;
  Printf.fprintf out "
                <button type=\"submit\" class=\"pure-button pure-button-primary\">Delete</button>
            </fieldset>
        </form>
        </main>
    </body>
</html>"


let db_test =
  { db_name = "test";
    db_alias = "Test";
    db_fields = [ 
      { f_name="url"; f_alias="Url"; f_select=true; f_autogenerate=true; f_type=VarChar };
      { f_name="album"; f_alias="Album Name"; f_select=false; f_autogenerate=false; f_type=VarChar };
      { f_name="band"; f_alias="Band name"; f_select=false; f_autogenerate=false; f_type=VarChar };
      { f_name="date"; f_alias="Date"; f_select=false; f_autogenerate=false; f_type=Date };
      { f_name="tags"; f_alias="Tags"; f_select=false; f_autogenerate=false; f_type=Set ["Live"] };
      { f_name="content"; f_alias="Content"; f_select=false; f_autogenerate=false; f_type=Text } ]
  }

let _ =
  print_model db_test;
  print_create_form db_test;
  print_update_form db_test;
  print_delete_form db_test
