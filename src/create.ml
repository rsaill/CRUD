open Types

let print_create_php_code (out:out_channel) (db:t_db) : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let pp_post_var out fd = Printf.fprintf out "$_POST['%s']" fd.f_name in
  let pp_isset_post_var out fd = Printf.fprintf out "isset($_POST['%s'])" fd.f_name in
  Printf.fprintf out "<?php
include('db.php');
include('%s.class.php');

$model = new %s($db);
if(%a){
    if($model->create(%a)){
        $msg = '<div class=\"w3-panel w3-green\">Success</div>';
    } else {
        $msg = '<div class=\"w3-panel w3-red\">' . $model->get_error() . '</div>';
    }
}
?>" model_name model_name
    (pp_list pp_isset_post_var " && ") db.db_fields
    (pp_list pp_post_var ", ") db.db_fields

let print out_dir db =
  let out = open_out (out_dir ^ "/create_" ^ db.db_name ^ ".php") in
  let text_list =
    List.filter (fun f -> match f.f_type with Text -> true | _ -> false)
      db.db_fields
  in
  print_create_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Creation</title>
        <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\">
" db.db_alias;
 (match text_list with
    | [] -> ()
    | _::_ ->
      Printf.fprintf out "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.css\">
        <script src=\"https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.js\"></script>"
 );
  if has_slug db then
    Printf.fprintf out "<script type=\"text/javascript\">
// source https://gist.github.com/codeguy/6684588
function slug (str) {
	str = str.replace(/^\\s+|\\s+$/g, ''); // trim
	str = str.toLowerCase();

	// remove accents, swap ñ for n, etc
	var from = \"àáäâèéëêìíïîòóöôùúüûñç·/_,:;\";
	var to   = \"aaaaeeeeiiiioooouuuunc------\";
	for (var i=0, l=from.length ; i<l ; i++) {
		str = str.replace(new RegExp(from.charAt(i), 'g'), to.charAt(i));
	}

	str = str.replace(/[^a-z0-9 -]/g, '') // remove invalid chars
		.replace(/\\s+/g, '-') // collapse whitespace and replace by -
		.replace(/-+/g, '-'); // collapse dashes

	return str;
}
</script>";
  Printf.fprintf out "
    </head>
    <body>
        <?php include('menu.html'); ?>
        <div class=\"w3-cell\" style=\"width:600px;padding-left:20px;\">
        <h2>%s Creation</h2>
        <?php
                if(isset($msg)){ echo $msg; }
        ?>
        <form action=\"\" method=\"post\">" db.db_alias;
  List.iter (fun fd ->
        match fd.f_type with
          | VarChar ->
            begin
              match fd.f_slug with
              | [] -> Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input id=\"%s\" name=\"%s\" type=\"text\" class=\"w3-input w3-border\" required>
            </p>"
                        fd.f_name fd.f_alias fd.f_name fd.f_name
              | _::_ ->
                let rec aux out = function
                  | [] -> assert false
                  | [hd] -> Printf.fprintf out "document.getElementById('%s').value" hd
                  | hd::tl ->
                    Printf.fprintf out "document.getElementById('%s').value + '-' + %a" hd aux tl
                in
                Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input id=\"%s\" name=\"%s\" type=\"text\" class=\"w3-input w3-border\" pattern=\"[a-z0-9-]+\" required>
            <button type=\"button\" class=\"w3-btn w3-blue\" onclick=\"document.getElementById('%s').value = slug(%a);\">Autogénérer</button>
            </p>"
                        fd.f_name fd.f_alias fd.f_name fd.f_name fd.f_name aux fd.f_slug
                
            end
        | Date ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input id=\"%s\" name=\"%s\" type=\"date\" class=\"w3-input w3-border\" required>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Text ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <textarea id=\"%s\" name=\"%s\" class=\"w3-input w3-border\"></textarea>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Set elts ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <select id=\"%s\" name=\"%s\" class=\"w3-input w3-border\" multiple required>" fd.f_name fd.f_alias fd.f_name fd.f_name;
          List.iter (fun x ->
              Printf.fprintf out "
                <option>%s</option>" x
            ) elts;
          Printf.fprintf out "
            </select>
            </p>"
    ) db.db_fields;
  Printf.fprintf out "
               <button type=\"submit\" class=\"w3-btn w3-blue\">Create</button>
        </form>
        </div>";
  ( match text_list with
    | [] -> ()
    | _::_ ->
      Printf.fprintf out "
<script>";
      List.iter (fun f ->
          Printf.fprintf out "
var simplemde = new SimpleMDE({ element: document.getElementById(\"%s\") });" f.f_name
        ) text_list;
      Printf.fprintf out "
</script>");
  Printf.fprintf out "
    </body>
</html>"
