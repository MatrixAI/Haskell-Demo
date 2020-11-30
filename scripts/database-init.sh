#!/usr/bin/env sh

rm -r $PGDATA || true
TZ=UTC initdb --auth=trust --encoding=UTF-8", shell=True, check=True
