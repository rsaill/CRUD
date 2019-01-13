open Types

let print_list_php_code out db : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  Printf.fprintf out "<?php
include('db.php');
include('auth.php');
include('%s.class.php');

$model = new %s($db);
$p = isset($_GET['p'])? max(1,$_GET['p']) : 1;
$result = $model->get_all();
?>" model_name model_name

let print dir menu db : unit =
  let out = open_out (dir ^ "/list_" ^ db.db_name ^ ".php") in
  print_list_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Management</title>
        <link rel=\"stylesheet\" href=\"https://www.w3schools.com/w3css/4/w3.css\">
    </head>
    <body>
        %s
        <div class=\"w3-cell\" style=\"padding-left:20px;\">
        <h2>%s</h2>
    <p><a class=\"w3-button w3-blue\" href=\"create_%s.php\">Create a new entry</a></p>
        <table class=\"w3-table-all\">
            <thead>
                <tr>
                    <th>Id</th>" db.db_alias menu db.db_alias db.db_name;
  List.iter (fun fd ->
      if fd.f_display then Printf.fprintf out "
                    <th>%s</th>" fd.f_alias
    ) db.db_fields;
  Printf.fprintf out "
                    <th>Update</th>
                    <th>Delete</th>
                </tr>
            </thead>
            <tbody>
<?
    $offset = ($p-1)*20;
    $nb = count($result);
    for($i=$offset;$i<min($nb,$offset+20);$i++){
        $line = $result[$i];
        echo '<tr>';
        echo '<td>',$line['id'],'</td>';";
  List.iter (fun fd -> (*FIXME autogen*)
      if fd.f_display then
        Printf.fprintf out "
echo '<td>',$line['%s'],'</td>';" fd.f_name
    ) db.db_fields;
  Printf.fprintf out "
echo '<td><a class=\"w3-button w3-blue\" href=\"update_%s.php?id=',$line['id'],'\">Update</a></td>';
echo '<td><a class=\"w3-button w3-blue\" href=\"delete_%s.php?id=',$line['id'],'\">Delete</a></td>';"
    db.db_name db.db_name;
  Printf.fprintf out "
        echo '</tr>';
    }
?>
        </tbody>
    </table>
    <p class=\"w3-bar\">
    <?php
    for($i=1;$i<=ceil($nb/20);$i++){
        if($i == $p){
            echo '<a href=\"list_%s.php?p=',$i,'\" class=\"w3-button w3-blue\">',$i,'</a>';
        } else {
            echo '<a href=\"list_%s.php?p=',$i,'\" class=\"w3-button\">',$i,'</a>';
        }
    }
    ?>
    </p>
        </div>
    </body>
</html>" db.db_name db.db_name
