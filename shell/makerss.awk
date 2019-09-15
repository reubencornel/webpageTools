BEGIN{
  RS="#";
  FS="|";
  URL="http://rfcornel.freeshell.org";

  print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  print "<rss version=\"2.0\">";
  print "<channel>";
  print "<title>Reuben's web page</title>";
  print "<link>" URL "</link>";
  print "<description>Syndication for Reuben's web page</description>";
  
}


END{
    print "</channel>";
    print "</rss>";
}

/<>/{print " "}

{
  print "<item>"
  print "<title>" $3 "</title>";
  print "<link>" URL "/" $4 "</link>";
  print "<description>" $2 "</description>";
  print "</item>";
}



