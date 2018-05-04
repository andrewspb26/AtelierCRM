# creating tables

create table clients (

  id integer PRIMARY KEY AUTOINCREMENT,
  name text,
  address text,
  email text
  
);

create table measurements (

  client_name integer, 
  height integer,
  chest_girth integer,
  sleeve_length integer,
  neck_girth integer,
  waist integer,
  biceps_girth integer,
  head_girth integer,
  wrist_girth integer,
  shoulder integer,
  notes text
  
);

create table orders (

  order_id integer PRIMARY KEY AUTOINCREMENT,
  user_name integer,
  started_at text,
  finished_at text,
  tracking_code text,
  item text,
  material text,
  color text,
  price integer
  
);