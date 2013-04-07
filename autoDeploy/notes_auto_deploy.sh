#!/usr/bin/env ksh

### Purpose
# The purpose of this script to deploy the latest files from the
# $webpage/org-notes directory to the remote web site folder at freeshell

# The reason this is separate from the whole website is because the
# content in set of pages will change frequently.


ORG_NOTES_ARCHIVE="org-notes.tar.gz"
FREESHELL_ACCOUNT="rfcornel@tty.freeshell.org"
ORG_NOTES_DIR="org-notes"



cd $WEBSITE

#Check if we have any files that need to be deployed.

if [ `find $ORG_NOTES_DIR -name '*.html'| wc -l` -eq 0 ];then
    # if dont have anything to deploy exit
    exit 1;
fi


rm -f $ORG_NOTES_ARCHIVE
tar -czf $ORG_NOTES_ARCHIVE $ORG_NOTES_DIR
sftp $FREESHELL_ACCOUNT <<EOF
put $ORG_NOTES_ARCHIVE
exit
EOF
ssh $FREESHELL_ACCOUNT <<EOF
tar -xzf $ORG_NOTES_ARCHIVE
cp -R $ORG_NOTES_DIR/* ~/html
rm -f $ORG_NOTES_ARCHIVE
rm -f $ORG_NOTES_DIR/src/*
rmdir $ORG_NOTES_DIR/src
rm -f $ORG_NOTES_DIR/*
rmdir $ORG_NOTES_DIR
EOF

cd $ORG_NOTES_DIR
rm *.html
cd src
rm *
exit 0