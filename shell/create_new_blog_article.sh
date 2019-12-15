#!/usr/bin/env bash
set -e
set -u
set -o pipefail

entry_name=`date '+%Y_%m_%d'`".article"

while getopts 'n:' OPTION; do
    case "$OPTION" in
	n)
	    entry_name="$OPTARG"
	    if [ -z $entry_name ] ; then
		echo "File name expected when supplying the -n option"
		exit 1;
	    fi;
	    ;;
	?)
	echo "Only supported option is -n"
	exit 1;
	;;
    esac
done

current_year=`date '+%Y'`

# If the blog entry does not exist for the day create one.
if [ ! -f $WEBSITE/blog/$current_year/${entry_name} ]; then
    cat > $WEBSITE/blog/$current_year/${entry_name} <<EOF
* TITLE

# no-navbeta

# no-toc

EOF
fi

nohup emacsclient $WEBSITE/blog/$current_year/${entry_name} &

