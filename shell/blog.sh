#!/usr/bin/env bash

temp_pwd=`pwd`
cd $WEBSITE/blog

mkdir $WEBSITE/website/blog 2>/dev/null

latest_article=""

for year in `ls -d 20*`;do
    cd $year

    article_count=`ls *.article 2>/dev/null | wc -l`
    if [ $article_count -gt 0 ] ; then

	echo "Generating articles for $year"

	articleGen *.article
	cp *.html *.jpg $WEBSITE/website &> /dev/null
	latest_article=`ls *.article | tail -1 | cut -d '.' -f1`
	rm *.html
    fi
    cd ..
done

# Generate the blog.html
# Create a file called blog.temp because we don't want it to be found when we look for article files
cat > blog.temp <<EOF
* Blog	

# no-toc

# no-navbeta

EOF

# Find all articles, find the titles in every file.
# Emit a row of the format <FILENAME>.article|FILENAME|Title
# Use awk to 1. replace all "_" wit "-" and emit a line of the format <a href="FILENAME.html">Date Title</a>
find . -name '*.article' | sort | xargs grep "\*\ " | cut -d '/' -f3- | sed 's/:\*/|/' | sed 's/\(.*\)\.article\(.*\)/\1.html|\1\2/' |  awk  'BEGIN{FS="|"; print("\n<ul>")} 
{date=$2; gsub(/_/,"-", date); print("<li><a href=\"" $1 "\">"date $3"</a>")}
END{print("</UL>")}' >> blog.temp

mv blog.temp blog.article
articleGen blog.article
cp blog.html $WEBSITE/website
rm blog.article

cd $temp_pwd
