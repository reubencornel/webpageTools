#!/usr/bin/perl

open (FILE1,"<$ARGV[0]") or die 'Could not open file';
#open (FILE1,"temp.html") ;




while($line=<FILE1>){
    print $line if $line !~ /NAVALPHA/;
    print $line if $line !~ /NAVBETA/;
    if ($line=~/^NAVALPHA$/){
	open (FILE,'</home/reuben/webpage/navAlpha') or open (FILE,'<navAlpha') or die 'Could not open file NAVALPHA';

	print "<div id=\"navAlpha\">\n";
	while(<FILE>){
	    chomp();
	    @menuEntries = split(/\|/);
	    print "<a href=\"$menuEntries[1]\">$menuEntries[0]</a><br /><br />\n " if !/^\#/;
	}
	print "</div>";
	print $1;
    }

    if($line=~/^NAVBETA$/){
	open (FILE2,'<navBeta') or die 'Could Not open file NAVBETA';
	print "<div id=\"navBeta\">\n";
	while(<FILE2>){
	    chomp();
	    @menuEntries = split(/\|/);
	    print "<a href=\"@menuEntries[1]\">@menuEntries[0]</a><br /><br />\n "  if !/^\#/;
	}
	print "</div>";
    }
}




