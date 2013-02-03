#!/bin/bash

cd /tmp;

dropdb db1;
echo "DROP USER db1;" | psql;
