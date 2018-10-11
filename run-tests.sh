#!/usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS="emacs"
fi


$EMACS -batch \
       -l ert \
       -l bm.el \
       -l bm-tests.el \
       -f ert-run-tests-batch-and-exit


$EMACS -Q --batch \
           --eval '(setq byte-compile-error-on-warn t)' \
           -f batch-byte-compile bm.el
