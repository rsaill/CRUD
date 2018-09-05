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
  (*TODO search*)


  Printf.fprintf out "
}
?>"
(*
		public function get_articles ($que,$pag,$cat,$cri) {
		$page = ($pag>0) ? (int) $pag : 1 ;
		$category = $cat ;
		$sql_order = 'nom';
		$sql_cat = "`type` = '$cat' AND ";
		
		switch ($cat) {
		case 'Album': case 'Concert': 
			$sql_order = 'artiste'; 
			break;
		case 'Live': case 'Interview': case 'Livre': 
		case 'DVD': case 'RockFr':  case 'Groupe':
			break;
		case 'JeuneTalent': 
			$sql_cat = "`type` = 'Jeune Talent' AND ";
			break;
		case 'AlbumDuMois': 
			$sql_cat = "`type` = 'Album du Mois' AND ";
			break;
		default:
			$category = 'All';
			$sql_cat = '';
		}

		switch ($cri) {
		case 'Album':	
			$arr = array("%$que%");
			$sql_like='`nom` LIKE ?';
		       	break;
		case 'Artiste':
			$arr = array("%$que%");
			$sql_like='`artiste` LIKE ?';
			break;
		case 'Annee':	
			$arr = array("%$que%");
			$sql_like='`annee` LIKE ?';
			break;
		default: 
			$arr = array_fill(0,3,"%$que%");
			$sql_like="( `nom` LIKE ? OR `artiste` LIKE ? OR `annee` LIKE ? )";
		}

		$limit=($page-1)*20;
		$sql = "SELECT `id`,`type`,`artiste`,`nom`,`annee`,`image`,`incontournable`,`url` FROM `bdd_rock_4` WHERE 
			$sql_cat $sql_like ORDER BY `$sql_order` LIMIT $limit,20";
		
		$stmt = $this->db->prepare($sql) ;
		$stmt->execute($arr) ;
		
		$sql2 = "SELECT COUNT(`id`) FROM `bdd_rock_4` WHERE $sql_cat $sql_like";
		$stmt2 = $this->db->prepare($sql2) ;
		$stmt2->execute($arr) ;
		$count = $stmt2->fetch();
		$nb = $count[0];

		$articles = array();
		$articles['que'] = $que;
		$articles['pag'] = $page ;
		$articles['cat'] = $category ;
		$articles['cri'] = $cri ;

		$articles['nb'] = $nb; 
		$articles['max_page'] = ceil($nb/20); 
		$articles['articles'] = $stmt->fetchAll();
		return $articles;
	}
	
*)
let db_test =
  { db_name = "Test";
    db_fields = [ 
      { f_name="url"; f_alias="Url"; f_select=true; f_autogenerate=true; f_type=VarChar };
      { f_name="album"; f_alias="Album Name"; f_select=false; f_autogenerate=false; f_type=VarChar };
      { f_name="band"; f_alias="Band name"; f_select=false; f_autogenerate=false; f_type=VarChar };
      { f_name="date"; f_alias="Date"; f_select=false; f_autogenerate=false; f_type=Date };
      { f_name="tags"; f_alias="Tags"; f_select=false; f_autogenerate=false; f_type=Set ["Live"] };
      { f_name="content"; f_alias="Content"; f_select=false; f_autogenerate=false; f_type=Text } ]
  }

let _ =
  print_model db_test
