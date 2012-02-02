#!/usr/bin/perl
use strict;
no strict 'refs';

use File::Basename;
use File::Path;

my $SKIP_EXISTING=1;

for my $func (qw/circle_stone  square_stone triangle_stone label_stone/) {
    &$func("bmoku-0","white");
    &$func("empty-0","black");
    &$func("wmoku-0","black");
}


sub circle_stone {

     my $base   = shift;
     my $color  = shift;
     my $infile ="$base.xpm";
     my $outfile="$base-CR.xpm";
     system "convert -strokewidth 2 -fill none -stroke $color -draw 'circle 17,17  24,24' $infile $outfile";
 }

sub square_stone {
     my $base   = shift;
     my $color  = shift;
     my $prop ="SQ";
     my $draw_command="rectangle 9,9  26,26";

     my $infile ="$base.xpm";
     my $outfile="$base-$prop.xpm";
     system "convert -strokewidth 2 -fill none -stroke $color -draw '$draw_command' $infile $outfile";
 }

sub triangle_stone {
     my $base   = shift;
     my $color  = shift;
     my $prop ="TR";
     my $draw_command="line 17,6 6,25 line 6,25 29,25 line 29,25 17,6";

     my $infile ="$base.xpm";
     my $outfile="$base-$prop.xpm";
     system "convert -strokewidth 2 -fill none -stroke $color -draw '$draw_command' $infile $outfile";
 }



sub label_stone {
    my $base=shift;
    my $color=shift;
    my $infile ="$base.xpm";
    my @label= ( (map {chr($_+ord("A"))}  0..25) , (map {chr($_+ord("a"))}  0..25) ,"1/2","1/3","2/3" ,0..361 );
    for my $l (@label) {
	my $outfile="$base-LB-$l.xpm";
	next if -f $outfile and $SKIP_EXISTING;
	if ($outfile=~/\//) {
	    eval {mkpath (dirname $outfile)};
	    if ($@) {
		print "Couldn't create $outfile: $@";
	    }
	}
	system "convert -gravity Center -pointsize 19  -fill $color -stroke $color  -draw 'text -1,-2  \"$l\"'  $infile $outfile ";
    }
}

