#!/bin/bash

set -e

source .secrets

DBDIR="$1"
if test -z "$DBDIR" ; then
  DBDIR=db
fi

D="$PWD"
mkdir -p $DBDIR/{music,transcoded,archives}
(cd $DBDIR ; exec "$D/dist/build/apollo/apollo" "$HOST" "$PORT" "$PASSWORD")

