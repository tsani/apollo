#!/bin/bash

set -e

PASSWORD="$(echo "$MPD_HOST" | cut -d "@" -f 1)"

if [ "$PASSWORD" = "$MPD_HOST" ] ; then
  PASSWORD=
  HOSTPORT="$MPD_HOST"
else
  HOSTPORT="$(echo "$MPD_HOST" | cut -d "@" -f 2)"
fi

HOST="$(echo "$HOSTPORT" | cut -d ":" -f 1)"

if [ "$HOST" = "$HOSTPORT" ] ; then
  PORT=6600
else
  PORT="$(echo "$HOSTPORT" | cut -d ":" -f 2)"
fi

DBDIR="$1"
shift
HTTP_PORT="$1"
shift

echo $HOST $PORT $PASSWORD

D="$PWD"
mkdir -p $DBDIR/{music,transcoded,archives}
(cd $DBDIR ; exec "$D/dist/build/apollo/apollo" "$HTTP_PORT" "$HOST" "$PORT" "$PASSWORD" $@)
