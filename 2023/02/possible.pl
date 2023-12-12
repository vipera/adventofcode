#!/usr/bin/env perl

use warnings;
use strict;

use constant MAX_RED => 12;
use constant MAX_GREEN => 13;
use constant MAX_BLUE => 14;

my $id_total = 0;
my $power_total = 0;

while (<>) {
  my ($game_id, $game_outcomes) = m/^Game (\d+):(.*)$/;
  my $possible_specifications = 0;
  my @game_specifications = split /;/, $game_outcomes;
  my %min_color = (red => 0, green => 0, blue => 0);

  for my $game_specification (@game_specifications) {
    my %games = reverse $game_specification =~ m/(?: (\d+) (red|green|blue)(?:,|$))/g;
    my %by_color = (red => 0, green => 0, blue => 0);

    for my $color (keys %games) {
      $by_color{$color} += $games{$color};
      $min_color{$color} = $games{$color} if $games{$color} > $min_color{$color};
    }

    if ($by_color{red} <= MAX_RED && $by_color{green} <= MAX_GREEN && $by_color{blue} <= MAX_BLUE) {
      $possible_specifications++;
    }
  }

  if ($possible_specifications == @game_specifications) {
    $id_total += $game_id;
  }

  $power_total += $min_color{red} * $min_color{green} * $min_color{blue};
}

print "Total of game IDs: $id_total\n";
print "Total power of sets is: $power_total\n";
