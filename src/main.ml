open Types

let out_dir = ref "."

let error msg =
  Printf.fprintf stderr "%s\n" msg;
  exit 1

let to_table (toml:TomlTypes.value) : (TomlTypes.table,string) result =
  let open TomlTypes in
  match toml with
  | TTable tb -> Ok tb
  | TString _ -> Error "expecting table, found string"
  | TBool _ -> Error "expecting table, found boolean"
  | TInt _ -> Error "expecting table, found int"
  | TFloat _  -> Error "expecting table, found float"
  | TDate _ -> Error "expecting table, found date"
  | TArray _ -> Error "expecting table, found array"

let to_string (toml:TomlTypes.value) : (string,string) result =
  let open TomlTypes in
  match toml with
  | TString s -> Ok s
  | TTable _ -> Error "expecting string, found table"
  | TBool _ -> Error "expecting string, found boolean"
  | TInt _ -> Error "expecting string, found int"
  | TFloat _  -> Error "expecting string, found float"
  | TDate _ -> Error "expecting string, found date"
  | TArray _ -> Error "expecting string, found array"

let to_bool (toml:TomlTypes.value) : (bool,string) result =
  let open TomlTypes in
  match toml with
  | TBool b -> Ok b
  | TString _ -> Error "expecting boolean, found string"
  | TTable _ -> Error "expecting boolean, found table"
  | TInt _ -> Error "expecting boolean, found int"
  | TFloat _  -> Error "expecting boolean, found float"
  | TDate _ -> Error "expecting boolean, found date"
  | TArray _ -> Error "expecting boolean, found array"

let to_table_array (toml:TomlTypes.value) : (TomlTypes.table list,string) result =
  let open TomlTypes in
  match toml with
  | TArray (NodeTable lst) -> Ok lst
  | TTable _ -> Error "expecting array of tables, found table"
  | TString _ -> Error "expecting array of tables, found string"
  | TBool _ -> Error "expecting array of tables, found boolean"
  | TInt _ -> Error "expecting array of tables, found int"
  | TFloat _  -> Error "expecting array of tables, found float"
  | TDate _ -> Error "expecting array of tables, found date"
  | TArray (NodeBool _) -> Error "expecting array of tables, found array of booleans"
  | TArray (NodeDate _) -> Error "expecting array of tables, found array of dates"
  | TArray NodeEmpty -> Error "expecting array of tables, found empty array"
  | TArray (NodeArray _) -> Error "expecting array of tables, found array of arrays"
  | TArray (NodeFloat _) -> Error "expecting array of tables, found array of floats"
  | TArray (NodeInt _) -> Error "expecting array of tables, found array of integers"
  | TArray (NodeString _) -> Error "expecting array of tables, found array of string"

let read_field db_name toml_t : t_field =
  let open TomlTypes in
  let f_name = match Table.find_opt (Toml.key "name") toml_t with
    | None -> error ("Error reading name missing for some field of '"^db_name^"'.")
    | Some v ->
      begin match to_string v with
        | Error msg -> error ("Error reading name for some field of '"^db_name^"' ("^msg^").")
        | Ok str -> str
      end
  in
  let f_alias = match Table.find_opt (Toml.key "alias") toml_t with
    | None -> f_name
    | Some v ->
      begin match to_string v with
        | Error msg -> error ("Error reading alias for field '"^db_name^"."^f_name^"' ("^msg^").")
        | Ok str -> str
      end
  in
  let f_type = match Table.find_opt (Toml.key "type") toml_t with
    | None -> VarChar
    | Some v ->
      begin match v with
        | TString s ->
          begin
            if String.equal "VarChar" s then VarChar
            else if String.equal "Date" s then Date
            else if String.equal "Text" s then Text
            else error ("Error reading type for field '"^db_name^"."^f_name^"' (unknown typ '"^s^"').")
          end
        | TArray (TomlTypes.NodeString lst) -> Set lst
        | _ -> error ("Error reading type for field '"^db_name^"."^f_name^"' (expecting a string or an array of strings).")
      end
  in
  let f_select = match Table.find_opt (Toml.key "select") toml_t with
    | None -> false
    | Some v ->
      begin match to_bool v with
        | Ok select -> select
        | Error msg -> error ("Error reading option select for field '"^db_name^"."^f_name^"' ("^msg^").")
      end
  in
  let f_display = match Table.find_opt (Toml.key "display") toml_t with
    | None -> true
    | Some v ->
      begin match to_bool v with
        | Ok display -> display
        | Error msg -> error ("Error reading option display for field '"^db_name^"."^f_name^"' ("^msg^").")
      end
  in
  let f_search = match Table.find_opt (Toml.key "search") toml_t with
    | None -> false
    | Some v ->
      begin match to_bool v with
        | Ok display -> display
        | Error msg -> error ("Error reading option search for field '"^db_name^"."^f_name^"' ("^msg^").")
      end
  in
  let f_slug = match Table.find_opt (Toml.key "slug") toml_t with
    | None -> []
    | Some v ->
      begin match v with
        | TArray (TomlTypes.NodeString lst) -> lst
        | _ -> error ("Error reading slug for field '"^db_name^"."^f_name^"' (expecting an array of strings).")
      end
  in
  { f_name; f_alias; f_type; f_select; f_display; f_search; f_slug }

let read_db (db_name:string) (toml_t:TomlTypes.table) : t_db =
  let open TomlTypes in
  let db_alias =
    match Table.find_opt (Toml.key "alias") toml_t with
    | None -> db_name
    | Some v ->
      begin match to_string v with
        | Ok alias -> alias
        | Error msg ->
          error ("Error reading alias for database '"^db_name^"' ("^msg^").")
      end
  in
  let db_fields = 
    match Table.find_opt (Toml.key "fields") toml_t with
    | None -> error ("No fields for database '"^db_name^"'.")
    | Some v ->
      begin match to_table_array v with
        | Error msg ->
          error ("Error reading fields for database '"^db_name^"' ("^msg^").")
        | Ok lst -> List.map (read_field db_name) lst
      end
  in
  { db_name; db_alias; db_fields }

let read_db_list (toml_t:TomlTypes.table) : t_db list =
  let open TomlTypes in
  Table.fold (
    fun db_name toml_v lst ->
      let db_name = Table.Key.to_string db_name in
      match to_table toml_v with
      | Ok toml_t -> (read_db db_name toml_t)::lst
      | Error msg -> error ("Error reading database '"^db_name^"' ("^msg^").")
  ) toml_t []

let print_menu out_dir (lst:t_db list) : unit =
  let out = open_out (out_dir ^ "/menu.html") in
  let aux_db db = 
    Printf.fprintf out "<a href=\"list_%s.php\" class=\"w3-bar-item w3-button\">%s</a>" db.db_name db.db_alias
  in
  Printf.fprintf out
    "<div class=\"w3-bar-block w3-cell w3-border w3-light-grey w3-card-4\">
        <h3 class=\"w3-bar-item w3-bold\">Databases</h3>";
  List.iter aux_db (List.rev lst);
  Printf.fprintf out "</div>"  

let run_on_file fn =
  match Toml.Parser.from_filename fn with
  | `Ok toml_table ->
    let lst = read_db_list toml_table in
    print_menu !out_dir lst;
    List.iter (fun db ->
        Model.print !out_dir db;
        Create.print !out_dir db;
        Request.print !out_dir db;
        Update.print !out_dir db;
        Delete.print !out_dir db
      ) lst
  | `Error (msg,_) ->
    begin
      Printf.fprintf stderr "%s.\n" msg;
      exit 1;
    end

let args = [
  ("-o", Arg.Set_string out_dir, "Output directory" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] database.toml")
