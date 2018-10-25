#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cat $DIR/../original/stugan2/stuga1.sve $DIR/../original/stugan3/stuga2.sve \
    $DIR/../original/stugan4/stuga3.sve | $DIR/stugan_parse.pl \
    | iconv -f CP437 -t UTF-8 > $DIR/../src/stuga.bas
$DIR/ibmgraph.py < $DIR/../original/stugan1/solhot.sve > $DIR/../src/solhot.txt
