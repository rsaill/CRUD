open Types

let pp_string out s = Printf.fprintf out "%s" s

let print_create_fn out db : unit =
  let params = String.concat "," (List.map (fun f -> "$"^f.f_name) db.db_fields) in
  let field_name_list = List.map (fun f -> f.f_name) db.db_fields in
  let field_names = String.concat "," field_name_list in
  let qmarks = String.concat "," (List.map (fun _ -> "?") field_name_list) in
  let vars = String.concat "," (List.map (fun f -> "$"^f) field_name_list) in
  Printf.fprintf out "
        public function create(%s){
                $sql = 'INSERT INTO `%s` (%s) VALUES (%s)';
                $stmt = $this->db->prepare($sql);"
    params db.db_name field_names qmarks;
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


let pp_cond out (f:t_field) : unit =
  Printf.fprintf out "`%s` LIKE ?" f.f_name

let rec pp_conds out : t_field list -> unit = function
  | [] -> assert false
  | [hd] -> pp_cond out hd
  | hd::tl -> Printf.fprintf out "%a OR %a" pp_cond hd pp_conds tl 

let rec pp_qmark out = function
  | [] -> assert false
  | [_] -> Printf.fprintf out "$str"
  | _::tl -> (Printf.fprintf out "$str,"; pp_qmark out tl)

let print_search_fn out db : unit =
  let sfields = List.filter (fun f -> f.f_search) db.db_fields in
  match sfields with
  | [] -> ()
  | hd::tl ->
    Printf.fprintf out "
        public function search($str){
                $sql = 'SELECT * FROM `%s` WHERE %a ORDER BY `id` DESC';
                $stmt = $this->db->prepare($sql);
                $stmt->execute(array(%a));
                return $stmt->fetchAll();
        }\n" db.db_name pp_conds sfields pp_qmark sfields

let print_list_fn out db : unit =
  Printf.fprintf out "
        public function get_all(){
                $sql = 'SELECT * FROM `%s` ORDER BY `id` DESC';
		$stmt = $this->db->prepare($sql);
		$stmt->execute();
                return $stmt->fetchAll();
        }\n" db.db_name

let print_last_fn out db : unit =
  Printf.fprintf out "
        public function get_last($n){
                $n = (int) $n;
                $sql = 'SELECT * FROM `%s` ORDER BY `id` DESC LIMIT ' . $n;
		$stmt = $this->db->prepare($sql);
		$stmt->execute();
                return $stmt->fetchAll();
        }" db.db_name

let print dir (db:t_db) : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let out = open_out (dir ^ "/" ^ model_name ^ ".class.php") in
  Printf.fprintf out "<?php
";
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
  print_last_fn out db;
  Printf.fprintf out "
}
?>"
