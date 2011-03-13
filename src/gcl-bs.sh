#!/bin/sh
if test "x$3" == "x" ; then
    echo "usage: $0 <gcl> <target> <multiplier>"
fi;
rm -f $2

$1 <<EOF
(setf si::*multiply-stacks* $3)
(si::save-system "$2")
EOF