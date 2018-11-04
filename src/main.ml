open Types

let gen_all db =
  Model.print db;
  Create.print db;
  Request.print db;
  Update.print db;
  Delete.print db

let db_test =
  { db_name = "test";
    db_alias = "Test";
    db_fields = [ 
      { f_name="url"; f_alias="Url"; f_select=true; f_autogenerate=true; f_type=VarChar; f_display=false };
      { f_name="album"; f_alias="Album Name"; f_select=false; f_autogenerate=false; f_type=VarChar; f_display=true };
      { f_name="band"; f_alias="Band name"; f_select=false; f_autogenerate=false; f_type=VarChar; f_display=true };
      { f_name="date"; f_alias="Date"; f_select=false; f_autogenerate=false; f_type=Date; f_display=false };
      { f_name="tags"; f_alias="Tags"; f_select=false; f_autogenerate=false; f_type=Set ["Live"]; f_display=false };
      { f_name="content"; f_alias="Content"; f_select=false; f_autogenerate=false; f_type=Text; f_display=false } ]
  }

let db_actu =
  { db_name = "actu_6";
    db_alias = "Actu";
    db_fields = [ 
      { f_name="title"; f_alias="Titre"; f_select=false; f_autogenerate=false; f_type=VarChar; f_display=true };
      { f_name="date"; f_alias="Date"; f_select=false; f_autogenerate=false; f_type=Date; f_display=true };
      { f_name="content"; f_alias="Content"; f_select=false; f_autogenerate=false; f_type=Text; f_display=false } ]
  }

let _ = gen_all db_actu
