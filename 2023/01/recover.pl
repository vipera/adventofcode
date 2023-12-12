#!/usr/bin/env perl

use warnings;
use strict;

my $total = 0;
while (<>) {
  my ($first_digit) = m/^[^\d]*?(\d)/;
  my ($last_digit) = m/(\d)[^\d]*?$/;
  my $number = int("${first_digit}${last_digit}");
  $total += $number;
}

print "Total: $total\n";
