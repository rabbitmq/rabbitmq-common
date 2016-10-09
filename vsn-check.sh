#!/bin/bash -e

PROJECT_VERSION="$1"

APP_VSN=$(erl -noshell -eval '{ok, [{_,_,L}]} = file:consult("src/supervisor3.app.src"), {vsn, Vsn} = lists:keyfind(vsn, 1, L), io:format("~s", [Vsn]), halt(0).')

if [ "$PROJECT_VERSION" != "$APP_VSN" ]; then
  echo "Version discrepancy, PROJECT_VERSION=$PROJECT_VERSION, APP_VSN=$APP_VSN"
  exit 1
fi

