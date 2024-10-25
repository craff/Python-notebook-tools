#!/bin/bash

dune exec -- server/main.exe -z -36000 -i python-eval --log-exceptions 4 --log-requests 4 --ssl _build/default/server/domain.crt _build/default/server/domain.key --restart-file ./server.restart
