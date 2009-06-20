#!/usr/bin/perl -w

use strict;
use diagnostics;
use File::Basename;

BEGIN {
    use File::Basename;
    push @INC, dirname($0);
};

use JavaScript::Minifier qw(minify);

my $inputfile = shift @ARGV;
open FH, "< $inputfile" or die "Couldn't open input file: $!";

while (<FH>) {
    if (/^(.*)<link\s[^>]*\brel="stylesheet"[^>]*\bhref="([^"]+)"[^>]*>(.*)/) {
        my $cssfile = $2;
        my $csspath = dirname($inputfile) . "/" . $cssfile;
        my $head = $1;
        my $tail = $3;
        print "$head<style type=\"text/css\">/* <![CDATA[ */\n";
        open CSS, $csspath or die "Couldn't open CSS file $csspath: $!";
        while (<CSS>) { print; }
        close CSS;
        print "\n/* ]]> */</style>$tail\n";
    } elsif (/^(.*<script\s[^>]*?)\s*src="([^"]+)"([^>]*)>(<.*)/) {
        my $scriptfile = $2;
        my $scriptpath = dirname($inputfile) . "/" . $scriptfile;
        open SCRIPT, $scriptpath or die "Couldn't open script $scriptpath: $!";
        my $content = minify(input => *SCRIPT);
        close SCRIPT;
        my $head = $1;
        my $mid = $3;
        my $tail = $4;
        $content =~ s:^\s+::;
        print "$head$mid>// $scriptfile <![CDATA[\n$content\n// ]]>$tail\n";
    } else {
        print;
    }
}

close FH;
