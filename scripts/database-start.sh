#!/usr/bin/env sh

pg_ctl start -w -o \"-h '$PGHOST' -k '$PGDATA'\"
