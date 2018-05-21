# creating tables

create table clients (

  id integer PRIMARY KEY AUTOINCREMENT,
  name text,
  address text,
  email text,
  created_at datetime default current_timestamp
  
);

create table measurements (

  client_name text, 
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
  user_name text,
  item text,
  material text,
  color text,
  quantity integer,
  price integer,
  status text,
  notes text,
  created_at datetime default current_date
  
);