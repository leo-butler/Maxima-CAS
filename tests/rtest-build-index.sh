#!/bin/sh
## Copyright Leo Butler 2011
## Released under the terms of GPLv3
##
## This code allows visual inspection of build-index.lisp's
## output.
##
function vecho
{
    if [ "x$VERBOSE" != "x" ]; then
	echo $*
    fi
}
function killme_and_my_children
{
    vecho -n "Killing "
    for p in $(ps -o pid --no-heading --ppid $PID) ; do
	vecho -n "$p "
	kill -9 $p 2>/dev/null  #I don't want to hear the screams
    done
    kill -9 $PID 2>/dev/null
    vecho ". Done."
}
function directory
{
    echo $1 | awk -F'/' -v ORS='/' 'for(i=1;i<NF;i++){print $i}'
}
PID=$$
VERBOSE=${VERBOSE:-}
INFO_DIR=${INFO_DIR:-../doc/info/}
INFO_SUBDIRS=${INFO_SUBDIRS:-$(find $INFO_DIR -name 'maxima.info' -execdir pwd \;)}
XTERM=${XTERM:-xterm}
XTERM_OPTS=${XTERM_OPTS:-"-exec"}
XTERM_CMD=${XTERM_CMD:-"$XTERM $XTERM_OPTS"}
LISPS=${LISPS:-"sbcl clisp cmucl"}
MAXIMA_OPTS=${MAXIMA_OPTS:-"--init=/dev/null --very-quiet"}
MAXIMA_CMD=${MAXIMA_CMD:-"$PWD/../maxima-local"}
#MAXIMA_BATCH_STRING=${MAXIMA_BATCH_STRING:-"setup_help_database();print_help_database(\"maxima-index.lisp\");describe(\"expand\");read(\"Quit?\");"}
MAXIMA_BATCH_STRING=${MAXIMA_BATCH_STRING:-"describe(\"expand\");read(\"Quit\?\");"}
BATCH_FILE=$(mktemp)

RUN_RTEST_BUILD_INDEX=${RUN_RTEST_BUILD_INDEX:-1}
RTEST_BUILD_INDEX_BS=${RTEST_BUILD_INDEX_BS:-"load(\"rtest-run.lisp\");"}
vecho $INFO_DIR
vecho $INFO_SUBDIRS
vecho "This pid: $$"

echo "$RTEST_BUILD_INDEX_BS" > $BATCH_FILE
if test "x$RUN_RTEST_BUILD_INDEX" == "x1"; then
    for lisp in $LISPS; do
	$MAXIMA_CMD $MAXIMA_OPTS -l $lisp --batch=$BATCH_FILE
    done
fi

echo "$MAXIMA_BATCH_STRING" > $BATCH_FILE
for dir in $INFO_SUBDIRS ; do
    vecho "Testing in $dir"
    for lisp in $LISPS ; do
	(cd $dir ;
	$XTERM_CMD "$MAXIMA_CMD $MAXIMA_OPTS -l $lisp --batch=$BATCH_FILE")
    done
done
rm -f $BATCH_FILE
trap killme_and_my_children INT QUIT HUP
exit 0;