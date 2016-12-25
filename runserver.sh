#!/bin/bash

set -e

source .secrets

MUSICDIR="$1"
if test -z "$MUSICDIR" ; then
  MUSICDIR=music
fi

D="$PWD"
mkdir -p $MUSICDIR
(cd $MUSICDIR ; exec "$D/dist/build/apollo/apollo" "$HOST" "$PORT" "$PASSWORD")

