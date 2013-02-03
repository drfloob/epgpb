#!/bin/bash

cd /tmp;
echo "CREATE USER db1 WITH PASSWORD 'abc123';" | psql;
createdb -O db1 db1 &&
echo "
CREATE SCHEMA my_schema; 
CREATE TABLE my_schema.test (
  num INTEGER NOT NULL
);
" | psql db1 db1
