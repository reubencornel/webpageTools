BEGIN{
  RS="#";
  FS="|";
  print "* Log"
  print ""
  print "# no-toc"
  print ""
  print "# no-navbeta"
  print ""
}

END{
}


{
  print "<b>"$1"</b>";
  print "";
  print $2;
}



