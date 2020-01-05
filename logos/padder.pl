#!/usr/bin/perl
use strict;
use warnings;

my $file = $ARGV[0];

my @acc;

open(my $fh , "<", $file) or die "can't open $file";

while (<$fh>) {
  chomp;
  push @acc, $_;
}

close($fh);

foreach my $str(@acc) {
  my $len = 88 - (length $str);
  my $pad = ' ' x int($len / 2);
  $str = $pad . $str . $pad;
}

my $len = 38 - (scalar @acc);
$len = ($len,0)[$len<0];
my @top_pad = (' ' x 88) x int($len/2);
my @final;
push(@final, @top_pad);
push(@final, @acc);
push(@final, @top_pad);

open(my $oh, ">", "testing.txt") or die "can't output";

map {print $oh ($_ . "\n")} @final;
