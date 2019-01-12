open Types

let print_list_php_code out db : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  Printf.fprintf out "<?php
include('db.php');
include('auth.php');
include('%s.class.php');

$model = new %s($db);
$offset = isset($_GET['offset'])? $_GET['offset'] : 0;
$result = $model->enumerate($offset);
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
        <div class=\"w3-cell\" style=\"width:600px;padding-left:20px;\">
        <h2>List</h2>
    <a href=\"create_%s.php\">Create a new entry</a>
        <table class=\"w3-table\">
            <thead>
                <tr>
                    <th>Id</th>" db.db_alias menu db.db_name;
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
    foreach($result as $line){
        echo '<tr>';
        echo '<td>',$line['id'],'</td>';";
  List.iter (fun fd -> (*FIXME autogen*)
      if fd.f_display then
        Printf.fprintf out "
echo '<td>',$line['%s'],'</td>';" fd.f_name
    ) db.db_fields;
  Printf.fprintf out "
echo '<td><a href=\"update_%s.php?id=',$line['id'],'\">Update</a></td>';
echo '<td><a href=\"delete_%s.php?id=',$line['id'],'\">Delete</a></td>';"
    db.db_name db.db_name;
  Printf.fprintf out "
        echo '</tr>';
    }
?>
        </tbody>
    </table>
    <a href=\"list_%s.php?p=<? echo min($offset-20,0); ?>\">Next</a>
    -
    <a href=\"list_%s.php?p=<? echo ($offset+20); ?>\">Previous</a>
        </div>
    </body>
</html>" db.db_name db.db_name
