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

# note to self: ARGS !!!
#  1. api scheme
#  2. api domain
#  3. api port
#  4. static scheme
#  5. static domain
#  6. static port

D="$PWD"
mkdir -p $DBDIR/{music,transcoded,archives}
(cd $DBDIR ; exec "$D/dist/build/apollo/apollo" "$HTTP_PORT" "$HOST" "$PORT" "$PASSWORD" $@)
