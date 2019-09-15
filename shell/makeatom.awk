BEGIN{
  RS="#";
  FS="|";
  URL="http://rfcornel.freeshell.org";

  print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  print "<feed xmlns=\"http://www.w3.org/2005/Atom\">";
  print "<title>Reuben's web page</title>";
  print "<link href=\"" URL "\"/>";
  
}


END{
    print "</feed>";
}

/<>/{print " "}

{
  print "<entry>"
  print "<title>" $3 "</title>";
  print "<link href=\"" URL "/" $4 "\"/>";
  print "<summary>" $2 "</summary>";
  print "</entry>";
}



