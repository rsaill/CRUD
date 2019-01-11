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

type t_read_field =
  | F of t_field
  | A of (t_field*string*string list)

let read_field db_name f_name toml_t : t_read_field =
  let open TomlTypes in
  let (alias,select,typ,display,fgen_name,fgen_params) =
    Table.fold (
      fun key toml_v (alias,select,typ,display,fgen_name,fgen_params) ->
        let skey = Table.Key.to_string key in
        if String.equal "alias" skey then
          match to_string toml_v with
          | Ok alias -> (Some alias,select,typ,display,fgen_name,fgen_params)
          | Error msg -> error ("Error reading alias for field '"^db_name^"."^f_name^"' ("^msg^").")

        else if String.equal "type" skey then
          let typ = match toml_v with
            | TString s ->
              begin
                if String.equal "VarChar" s then VarChar
                else if String.equal "Date" s then Date
                else if String.equal "Text" s then Text
                else error ("Error reading type for field '"^db_name^"."^f_name^"' (unknown typ '"^s^"').")
              end
            | TArray (TomlTypes.NodeString lst) -> Set lst
            | _ -> error ("Error reading type for field '"^db_name^"."^f_name^"' (expecting a string or an array of strings).")
          in
          (alias,select,Some typ,display,fgen_name,fgen_params)

        else if String.equal "select" skey then
          match to_bool toml_v with
            | Ok select -> (alias,Some select,typ,display,fgen_name,fgen_params)
            | Error msg -> error ("Error reading option select for field '"^db_name^"."^f_name^"' ("^msg^").")

        else if String.equal "display" skey then
          match to_bool toml_v with
            | Ok display -> (alias,select,typ,Some display,fgen_name,fgen_params)
            | Error msg -> error ("Error reading option display for field '"^db_name^"."^f_name^"' ("^msg^").")

        else if String.equal "generator_name" skey then
          match to_string toml_v with
            | Ok fgen_name -> (alias,select,typ,display,Some fgen_name,fgen_params)
            | Error msg -> error ("Error reading option generator_name for field '"^db_name^"."^f_name^"' ("^msg^").")

        else if String.equal "generator_parameters" skey then
          match toml_v with
            | TArray (TomlTypes.NodeString lst) -> (alias,select,typ,display,fgen_name,Some lst)
            | _ -> error ("Error reading type for field '"^db_name^"."^f_name^"' (expecting an array of strings).")

        else
          error ("Error reading field '"^db_name^"."^f_name^"' (unknown key '"^skey^"').")
    ) toml_t (None,None,None,None,None,None)
  in
  let fd =
    { f_name;
      f_alias = (match alias with None -> f_name | Some alias -> alias);
      f_type = (match typ with None -> VarChar | Some typ -> typ);
      f_select = (match select with None -> false | Some s -> s);
      f_display = (match display with None -> true | Some d -> d);
    }
  in
  match fgen_name, fgen_params with
  | None, None -> F fd
  | Some fgen_name, Some fgen_params -> A (fd,fgen_name,fgen_params)
  | Some fgen_name, None -> A (fd,fgen_name,[])
  | None, Some fgen_params -> A(fd,"gen" ^ String.capitalize_ascii f_name,fgen_params)

let read_db (db_name:string) (toml_t:TomlTypes.table) : t_db =
  let open TomlTypes in
  let (alias,fds,afds) =
    Table.fold (
      fun key toml_v (alias,fds,afds) ->
        let skey = Table.Key.to_string key in
        if String.equal "alias" skey then
          match to_string toml_v with
          | Ok alias -> (Some alias,fds,afds)
          | Error msg -> error ("Error reading alias for database '"^db_name^"' ("^msg^").")
        else
          match to_table toml_v with
          | Ok toml_t ->
            begin match read_field db_name (Table.Key.to_string key) toml_t with
              | F fd -> (alias,fd::fds,afds)
              | A (fd,f,args) -> (alias,fds,(fd,f,args)::afds)
            end
          | Error msg -> error ("Error reading field '"^skey^"' of database '"^db_name^"' ("^msg^").")
    ) toml_t (None,[],[])
  in
  { db_name;
    db_alias = (match alias with None -> db_name | Some str -> str);
    db_fields = List.rev fds;
    db_autogen_fields = List.rev afds }

let read_db_list (toml_t:TomlTypes.table) : t_db list =
  let open TomlTypes in
  Table.fold (
    fun db_name toml_v lst ->
      let db_name = Table.Key.to_string db_name in
      match to_table toml_v with
      | Ok toml_t -> (read_db db_name toml_t)::lst
      | Error msg -> error ("Error reading database '"^db_name^"' ("^msg^").")
  ) toml_t []

let menu (lst:t_db list) : string =
  let aux_db db = 
    Printf.sprintf
    "<li class=\"pure-menu-item\">
            <a href=\"\" class=\"pure-menu-link\">%s</a>
     </li>" db.db_name
  in
  let rec aux_list () = function
    | [] -> ""
    | [hd] -> aux_db hd
    | hd::tl -> aux_db hd ^ aux_list () tl
  in
  Printf.sprintf
  "<div id=\"menu\">
    <span class=\"pure-menu-heading\">Admin</span>
    <ul class=\"pure-menu-list\">
      %a
    </ul>
  </div>" aux_list lst

let run_on_file fn =
  match Toml.Parser.from_filename fn with
  | `Ok toml_table ->
    let lst = read_db_list toml_table in
    let menu = menu lst in
    List.iter (fun db ->
        Model.print !out_dir db;
        Create.print !out_dir menu db;
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
