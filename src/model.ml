open Types

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
                $%s = _gen%s(%s);" f.f_name (String.capitalize_ascii f.f_name) params (*FIXME*)
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
                $%s = _gen%s(%s);" f.f_name (String.capitalize_ascii f.f_name) vars (*FIXME*)
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

let print_list_fn out db : unit =
  Printf.fprintf out "
        public function list($offset){
                $sql = 'SELECT * FROM `%s` ORDER BY `id` DESC LIMIT 20 OFFSET ?';
		$stmt = $this->db->prepare($sql);
		$stmt->execute(min(0,$offset));
                return $stmt->fetchAll();
        }" db.db_name

let print (db:t_db) : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let out = open_out (model_name ^ ".class.php") in
  Printf.fprintf out "<?php
class %s {
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
