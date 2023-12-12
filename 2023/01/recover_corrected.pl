#!/usr/bin/env perl

use warnings;
use strict;

my %number_lookup = (
  one => 1,
  two => 2,
  three => 3,
  four => 4,
  five => 5,
  six => 6,
  seven => 7,
  eight => 8,
  nine => 9,
);
my $number_regex = join '|', keys %number_lookup;

my $total = 0;

while (<>) {
  my ($first_digit_text) = m/^[^\d]*?(\d|$number_regex)/;
  my ($last_digit_text) = m/.*(\d|$number_regex)[^\d]*$/;
  my $first_digit = $number_lookup{$first_digit_text} // $first_digit_text;
  my $last_digit = $number_lookup{$last_digit_text} // $last_digit_text;
  my $number = int("${first_digit}${last_digit}");
  $total += $number;
}

print "Total: $total\n";
