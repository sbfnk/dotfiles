#!/bin/sh

/usr/local/bin/mbsync $1
/opt/homebrew/bin/notmuch new
