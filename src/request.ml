open Types

let print_list_php_code out db : unit =
  let model_name = "Model" ^ (String.capitalize_ascii db.db_name) in
  Printf.fprintf out "<?php
include('db.php');
include('auth.php');
include('%s.class.php');

$model = new %s($db);
$offset = isset($_GET['p'])? $_GET['offset'] : 0;
$result = $model->list($offset);
?>" model_name model_name

let print dir db : unit =
  let out = open_out (dir ^ "/list_" ^ db.db_name ^ ".php") in
  print_list_php_code out db;
  Printf.fprintf out "<!DOCTYPE html>
<html lang=\"fr\">
    <head>
        <meta charset=\"utf-8\">
        <title>%s Management</title>
        <link rel=\"stylesheet\" href=\"pure/pure-min.css\" media=\"screen\">
    </head>
    <body>
        <main style=\"width:600px;margin:auto\">
    <a href=\"create_%s.php\">Create a new entry</a>
        <table class=\"pure-table-striped\">
            <thead>
                <tr>
                    <th>Id</th>" db.db_alias db.db_name;
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
        echo '<tr>';";
  List.iter (fun fd ->
      if fd.f_display then
        Printf.fprintf out "
echo '<td>',$line['%s'],'</td>';
echo '<td><a href=\"update_%s.php?id=',$line['id'],'\">Update</a></td>';
echo '<td><a href=\"delete_%s.php?id=',$line['id'],'\">Delete</a></td>';"
        db.db_name db.db_name fd.f_name
    ) db.db_fields;
  Printf.fprintf out "
    }
?>
        </tbody>
    </table>
    <a href=\"list_%s.php?p=<? echo min($offset-20,0); ?>\">Next</a>
    -
    <a href=\"list_%s.php?p=<? echo ($offset+20); ?>\">Previous</a>
        </main>
    </body>
</html>" db.db_name db.db_name
