#!/usr/bin/perl
$count=0;
$inside_code=0;
$indent=4;
$line_number=0;
while(<>){

    $inside_code=1 if /<CODE-TAG>/;
    $inside_code=0 if /<\/CODE-TAG>/;


    print "<div id=\"navCode\">" if /<CODE-TAG>/;
    print "</div>" if /<\/CODE-TAG>/;
   
    if($inside_code){
	next if /<CODE-TAG>/;
	$count=$count-$indent if  /\}/;
	print $line_number . "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" if !/<CODE-TAG>/;
	$line_number++ if !/<CODE-TAG>/;
	
	for ($i=0;$i<$count;$i=$i+1){
	    print "&nbsp;";
	}
	while(/(.*)&&(.*)/ && !/&amp/){
	    $_=$1 . "&amp;&amp;" .$2;
	}

	while(/(.*)\<(.*)/){
	    $_= $1 . "&lt;" .$2 ;

	}
	while(/(.*)\>(.*)/){
	    $_= $1 . "&gt;" .$2 ;

	}
	print $_ if !/<CODE-TAG>/ && !/\</ && !/\>/;
	print "<br />" if !/<CODE-TAG>/;
	print "\n";
	
	$count+=$indent if  /\{/;

    }
    print if !$inside_code && !/<\/CODE-TAG/;

}
