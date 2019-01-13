open Types

let print_update_php_code out db =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let pp_post_var out fd = Printf.fprintf out "$_POST['%s']" fd.f_name in
  let pp_isset_post_var out fd = Printf.fprintf out "isset($_POST['%s'])" fd.f_name in
  let pp_error out fd : unit = Printf.fprintf out "'%s' => 'Error'" fd.f_name in
  Printf.fprintf out "<?php
include('db.php');
include('auth.php');
include('%s.class.php');

$model = new %s($db);
if(isset($_POST['id']) && %a){
    if($model->update($_POST['id'],%a)){
        $msg = '<div class=\"w3-panel w3-green\">Success</div>';
    } else {
        $msg = '<div class=\"w3-panel w3-red\">Failure</div>';
    }
}

$error_arr = array('id'=>'-1',%a);
if(isset($_GET['id']) || isset($_POST['id'])){
        $id = isset($_GET['id'])? $_GET['id'] : $_POST['id'];
        $arr = $model->select_by_id($id);
        if($arr == FALSE){
                $arr = $error_err;
        }
} else {
        $arr = $error_err;
}
?>" model_name model_name
    (pp_list pp_isset_post_var " && ") db.db_fields
    (pp_list pp_post_var ", ") db.db_fields
    (pp_list pp_error ", ") db.db_fields

let print dir menu db =
  let out = open_out (dir ^ "/update_" ^ db.db_name ^ ".php") in
  let text_list =
    List.filter (fun f -> match f.f_type with Text -> true | _ -> false)
      db.db_fields
  in
  print_update_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Update</title>
        <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\">
    " db.db_alias;
    (match text_list with
     | [] -> ()
     | _::_ ->
       Printf.fprintf out "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.css\">
        <script src=\"https://cdn.jsdelivr.net/simplemde/latest/simplemde.min.js\"></script>"
    );
  Printf.fprintf out "
    </head>
    <body>
        %s
        <div class=\"w3-cell\" style=\"width:600px;padding-left:20px;\">
        <h2>%s Update</h2>
        <?php
                if(isset($msg)){ echo $msg; }
        ?>
        <form action=\"\" method=\"post\">
                <input name=\"id\" type=\"hidden\" value=\"<?php echo $arr['id'];?>\">"
    menu db.db_alias;
  List.iter (fun fd ->
        match fd.f_type with
        | VarChar ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input name=\"%s\" type=\"text\" class=\"w3-input w3-border\" value=\"<?php echo $arr['%s'];?>\" required>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Date ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input name=\"%s\" type=\"date\" class=\"w3-input w3-border\" value=\"<?php echo $arr['%s'];?>\" required>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Text ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <textarea id=\"%s\" name=\"%s\" class=\"w3-input w3-border\" required><?php echo $arr['%s'];?></textarea>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name fd.f_name
        | Set elts ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <select name=\"%s\" class=\"w3-input w3-border\" multiple>
            <?php
                $tags = explode(\",\",$arr['%s']);"
            fd.f_name fd.f_alias fd.f_name fd.f_name;
          List.iter (fun x ->
              Printf.fprintf out "
                if(array_in('%s',$tags)){ echo '<option selected>%s</option>'; }
                else { echo '<option>%s</option>'; }"
                x x x
            ) elts;
          Printf.fprintf out "
            ?>
            </select>
            </p>"
    ) db.db_fields;
  Printf.fprintf out "
                <button type=\"submit\" class=\"w3-btn w3-blue\">Update</button>
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
