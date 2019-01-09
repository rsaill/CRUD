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
    f_display: bool }

type t_autogen_field = 
  { a_name: string;
    a_alias: string;
    a_gen_fun_name: string;
    a_gen_fun_params: string list;
    a_select: bool;
    a_display: bool }

type t_db =
  { db_name:string;
    db_alias:string;
    db_fields: t_field list;
    db_autogen_fields: t_autogen_field list }

let rec pp_list pp sep out = function
  | [] -> ()
  | [hd] -> Printf.fprintf out "%a" pp hd
  | hd::tl -> Printf.fprintf out "%a%s%a" pp hd sep (pp_list pp sep) tl
