#!/usr/bin/env ksh

if [ ! -r "autodeploy.sh" -a ! -w "autodeploy.sh" ]; then
	echo "Yes"
else
	echo "No"
fi
