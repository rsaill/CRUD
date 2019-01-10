open Types

let pp_string out s = Printf.fprintf out "%s" s

let print_create_fn out db : unit =
  let params = String.concat "," (List.map (fun f -> "$"^f.f_name) db.db_fields) in
  let field_name_list =
    (List.map (fun f -> f.f_name) db.db_fields)
    @(List.map (fun f -> f.a_name) db.db_autogen_fields)
  in
  let field_names = String.concat "," field_name_list in
  let qmarks = String.concat "," (List.map (fun _ -> "?") field_name_list) in
  let vars = String.concat "," (List.map (fun f -> "$"^f) field_name_list) in
  Printf.fprintf out "
        public function create(%s){
                $sql = 'INSERT INTO `%s` (%s) VALUES (%s)';
                $stmt = $this->db->prepare($sql);"
    params db.db_name field_names qmarks;
  List.iter (fun f ->
      let lst = List.map (fun s -> "$" ^ s) f.a_gen_fun_params in
      Printf.fprintf out "
                $%s = %s(%a);" f.a_name f.a_gen_fun_name (pp_list pp_string ",") lst
    ) db.db_autogen_fields;
  Printf.fprintf out "
                return $stmt->execute(array(%s));
        }\n" vars

let print_update_fn out db : unit =
  let params = String.concat "," (List.map (fun f -> "$"^f.f_name) db.db_fields) in
  let field_eq_qmark = String.concat "," (List.map (fun f -> "`" ^ f.f_name ^ "` = ?" ) db.db_fields) in
  let vars = String.concat "," (List.map (fun f -> "$"^f.f_name) db.db_fields) in
  Printf.fprintf out "
        public function update($id,%s){
                $sql = 'UPDATE `%s` SET %s WHERE `id` = ?';
                $stmt = $this->db->prepare($sql);" params db.db_name field_eq_qmark;
  Printf.fprintf out "
                return $stmt->execute(array(%s,$id));
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
		return $stmt->execute(array($id));
	}\n" db_name

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
        }\n" db.db_name

let print_list_fn out db : unit =
  Printf.fprintf out "
        public function enumerate($offset){
                $offset = max(0,$offset);
                $sql = 'SELECT * FROM `%s` ORDER BY `id` DESC LIMIT 20 OFFSET '.$offset;
		$stmt = $this->db->prepare($sql);
		$stmt->execute();
                return $stmt->fetchAll();
        }" db.db_name

let print dir (db:t_db) : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let out = open_out (dir ^ "/" ^ model_name ^ ".class.php") in
  Printf.fprintf out "<?php
";
  (if db.db_autogen_fields != [] then
     Printf.fprintf out "require(\"autogen.php\");

";);
Printf.fprintf out "class %s {
	private $db;

        public function __construct($db){
                $this->db = $db;
	}
" model_name;
  print_create_fn out db;
  print_select_fns out db;
  print_update_fn out db;
  print_delete_fn out db.db_name;
  print_search_fn out db;
  print_list_fn out db;
  Printf.fprintf out "
}
?>"
