open Types

let out_dir = ref "."

let error msg =
  Printf.fprintf stderr "%s\n" msg;
  exit 1

let toml_to_string (toml:TomlTypes.value) : string =
  match toml with
  | TomlTypes.TString s -> s
  | _ -> error "Invalid string value."

let toml_to_field_type (toml:TomlTypes.value) : t_field_type =
  match toml with
  | TomlTypes.TString s ->
    begin
      if String.equal "VarChar" s then VarChar
      else if String.equal "Date" s then Date
      else if String.equal "Text" s then Text
      else error "Invalid field type." (*FIXME*)
    end
  | TomlTypes.TArray (TomlTypes.NodeString lst) ->
    Set lst
  | _ -> error "Invalid field type." (*FIXME*)

let toml_to_bool (toml:TomlTypes.value) : bool =
  match toml with
  | TomlTypes.TBool s -> s
  | _ -> error "Invalid boolean value." (*FIXME*)

let toml_to_table_list (type a) (f:TomlTypes.table->a) (toml:TomlTypes.value) : a list =
  match toml with
  | TomlTypes.TArray TomlTypes.NodeTable lst -> List.map f lst
  | _ -> error "Invalid array of tables." (*FIXME*)

let toml_to_string_list (toml:TomlTypes.value) : string list =
  match toml with
  | TomlTypes.TArray TomlTypes.NodeString lst -> lst
  | _ -> error "Invalid string array." (*FIXME*)


let find_def key table def =
  match TomlTypes.Table.find_opt (Toml.key key) table with
  | Some v -> v
  | None -> def

let find_err key table msg =
  match TomlTypes.Table.find_opt (Toml.key key) table with
  | Some v -> v
  | None -> error msg

let toml_to_field (toml_table:TomlTypes.table) : t_field =
  let open TomlTypes in
  let f_name = toml_to_string (find_err "name" toml_table "Some field name is missing.") in
    { f_name;
    f_alias = toml_to_string (find_def "alias" toml_table (TString f_name));
    f_type = toml_to_field_type (find_def "type" toml_table (TString "VarChar"));
    f_select = toml_to_bool (find_def "select" toml_table (TBool false));
    f_display = toml_to_bool (find_def "display" toml_table (TBool true));
  }

let toml_to_autogen_field (toml_table:TomlTypes.table) : t_autogen_field =
  let open TomlTypes in
  let a_name = toml_to_string (find_err "name" toml_table "Some autogen field name is missing.") in
  { a_name;
    a_alias = toml_to_string (find_def "alias" toml_table (TString a_name));
    a_select = toml_to_bool (find_def "select" toml_table (TBool false));
    a_gen_fun_name = toml_to_string (find_def "generator_name" toml_table (TString ("gen" ^ String.capitalize_ascii a_name)));
    a_gen_fun_params = toml_to_string_list (find_def "generator_parameters" toml_table (TArray (NodeString [])));
    a_display = toml_to_bool (find_def "display" toml_table (TBool true));
  }

let read_db (toml_table:TomlTypes.table) : t_db =
  let open TomlTypes in
  let db_name = toml_to_string (find_err "name" toml_table "The name of the database is missing.") in
  { db_name;
    db_alias = toml_to_string (find_def "alias" toml_table (TString db_name));
    db_fields = toml_to_table_list toml_to_field (find_err "fields" toml_table "No fields found.");
    db_autogen_fields = toml_to_table_list toml_to_autogen_field (find_def "autogen_fields" toml_table (TArray (NodeTable [])));
  }
let run_on_file fn =
  match Toml.Parser.from_filename fn with
  | `Ok toml_table ->
    begin
      let db = read_db toml_table in
      Model.print !out_dir db;
      Create.print !out_dir db;
      Request.print !out_dir db;
      Update.print !out_dir db;
      Delete.print !out_dir db
    end
  | `Error (msg,_) ->
    begin
      Printf.fprintf stderr "%s.\n" msg;
      exit 1;
    end

let args = [
  ("-o", Arg.Set_string out_dir, "Output directory" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] database.toml")

(*
let db =
  { db_name = "actu_6";
    db_alias = "Test";
    db_fields = [ 
      { f_name="album"; f_alias="Album Name"; f_select=false; f_type=VarChar; f_display=true };
      { f_name="band"; f_alias="Band name"; f_select=false; f_type=VarChar; f_display=true };
      { f_name="date"; f_alias="Date"; f_select=false; f_type=Date; f_display=false };
      { f_name="tags"; f_alias="Tags"; f_select=false; f_type=Set ["Live"]; f_display=false };
      { f_name="content"; f_alias="Content"; f_select=false; f_type=Text; f_display=false } ];
    db_autogen_fields = [
      { a_name="url"; a_alias="Url"; a_select=true; a_display=false; a_gen_fun_name="genUrl"; a_gen_fun_params=["artist";"album"]};
    ]
  }
*)
  let db =
  { db_name = "actu_6";
    db_alias = "Actu";
    db_fields = [ 
      { f_name="title"; f_alias="Titre"; f_select=false; f_type=VarChar; f_display=true };
      { f_name="date"; f_alias="Date"; f_select=false; f_type=Date; f_display=true };
      { f_name="content"; f_alias="Content"; f_select=false; f_type=Text; f_display=false } ];
    db_autogen_fields = [] 
  }

let _ =
  Model.print !out_dir db;
  Create.print !out_dir db;
  Request.print !out_dir db;
  Update.print !out_dir db;
  Delete.print !out_dir db

