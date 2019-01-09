open Types

let print_delete_php_code out db =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let pp_error out fd : unit = Printf.fprintf out "'%s' => 'Error'" fd.f_name in
  Printf.fprintf out "<?php
include('auth.php');
include('db.php');
include('%s.class.php');
$model = new %s(new db());

if(isset($_POST['id'])){
    if($model->delete($_POST['id'])){
        $msg = '<div>Success</div>';
    } else {
        $msg = '<div>Failure</div>';
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
?>" model_name model_name (pp_list pp_error ", ") db.db_fields

let print dir db =
  let out = open_out (dir ^ "/delete_" ^ db.db_name ^ ".php") in
  print_delete_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Deletion</title>
        <link rel=\"stylesheet\" href=\"pure/pure-min.css\" media=\"screen\">
    </head>
    <body>
        <main style=\"width:600px;margin:auto\">
        <?php
                if(isset($msg)){ echo $msg; }
        ?>

        <form class=\"pure-form pure-form-stacked\">
            <fieldset>
                <legend>%s Deletion</legend>
                <input id=\"id\" type=\"hidden\" placeholder=\"<?php=$arr['id'];?>\">"
    db.db_alias db.db_alias;
  List.iter (fun fd ->
        match fd.f_type with
        | VarChar ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"text\" placeholder=\"<?php=$arr['%s'];?>\" disabled>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Date ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <input id=\"%s\" type=\"date\" placeholder=\"<?php=$arr['%s'];?>\" disabled>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Text ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <textarea id=\"%s\" disabled><?php=$arr['%s'];?></textarea>"
            fd.f_name fd.f_alias fd.f_name fd.f_alias
        | Set elts ->
          Printf.fprintf out "
            <label for=\"%s\">%s</label>
            <select id=\"%s\" multiple disabled>
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
            </select>"
    ) db.db_fields;
  Printf.fprintf out "
                <button type=\"submit\" class=\"pure-button pure-button-primary\">Delete</button>
            </fieldset>
        </form>
        </main>
    </body>
</html>"
