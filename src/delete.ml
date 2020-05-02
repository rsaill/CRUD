open Types

let print_delete_php_code out db =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  let pp_error out fd : unit = Printf.fprintf out "'%s' => 'Error'" fd.f_name in
  Printf.fprintf out "<?php
include('db.php');
include('%s.class.php');

$model = new %s($db);

if(isset($_POST['id'])){
    if($model->delete($_POST['id'])){
        $msg = '<div class=\"w3-panel w3-green\">Success</div>';
    } else {
        $msg = '<div class=\"w3-panel w3-red\">' . $model->get_error() . '</div>';
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

let print out_dir db =
  let out = open_out (out_dir ^ "/delete_" ^ db.db_name ^ ".php") in
  print_delete_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Deletion</title>
        <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\">
    </head>
    <body>
        <?php include('menu.html'); ?>
        <div class=\"w3-cell\" style=\"width:600px;padding-left:20px;\">
        <h2>%s Deletion</h2>
        <?php
                if(isset($msg)){ echo $msg; }
        ?>
        <form action=\"\" method=\"post\">
                <input name=\"id\" type=\"hidden\" value=\"<?php echo $arr['id'];?>\">"
    db.db_alias db.db_alias;
  List.iter (fun fd ->
        match fd.f_type with
        | VarChar ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input name=\"%s\" type=\"text\" class=\"w3-input w3-border\" value=\"<?php echo $arr['%s'];?>\" disabled>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Date ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <input name=\"%s\" type=\"date\" class=\"w3-input w3-border\" value=\"<?php echo $arr['%s'];?>\" disabled>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Text ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <textarea name=\"%s\" class=\"w3-input w3-border\" disabled><?php echo $arr['%s'];?></textarea>
            </p>"
            fd.f_name fd.f_alias fd.f_name fd.f_name
        | Set elts ->
          Printf.fprintf out "
            <p>
            <label for=\"%s\">%s</label>
            <select name=\"%s\" class=\"w3-input w3-border\" multiple disabled>
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
                <button type=\"submit\" class=\"w3-btn w3-blue\">Delete</button>
        </form>
        </div>
    </body>
</html>"
