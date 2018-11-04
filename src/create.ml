open Types

let print_create_php_code out db =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let non_auto_gen_fields = List.filter (fun f -> not(f.f_autogenerate)) db.db_fields in
  let pp_isset_post_var out fd = Printf.fprintf out "$_POST['%s']" fd.f_name in
  let pp_post_var out fd = Printf.fprintf out "isset($_POST['%s'])" fd.f_name in
  Printf.fprintf out "<?php
include('auth.php');
include('db.php');
include('autogen.php');
include('%s.class.php');

$model = new %s($db);
if(%a){
    if($model->create(%a)){
        $msg = '<div>Success</div>';
    } else {
        $msg = '<div>Failure</div>';
    }
}
?>" model_name model_name
    (pp_list pp_isset_post_var " && ") non_auto_gen_fields
    (pp_list pp_post_var ", ") non_auto_gen_fields

let print db =
  let out = open_out ("create_" ^ db.db_name ^ ".php") in
  print_create_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Creation</title>
        <link rel=\"stylesheet\" href=\"pure/pure-min.css\" media=\"screen\">
    </head>
    <body>
        <main style=\"width:600px;margin:auto\">
        <?php
                if(isset($msg)){ echo $msg; }
        ?>
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
