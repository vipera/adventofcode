use std::cmp::Ordering;
use std::collections::HashSet;

use super::Card;
use super::{Hand, OrderedHand, CamelOrdering, compare_hands, base_card_strength};

/// Newtype for representing a hand with "standard" ordering.
#[derive(PartialEq, Eq)]
pub struct StandardOrdering<'a>(&'a Hand);

impl<'a> OrderedHand for StandardOrdering<'a> {
  type CardOrdering = u32;

  fn hand(&self) -> &Hand { self.0 }

  fn hand_strength(hand: &Hand) -> CamelOrdering {
    let count_map = hand.as_count_map();
    let distinct_keys = count_map.keys().len();
    let count_set: HashSet<_> = count_map.values().copied().collect();

    if count_set.contains(&5) {
      CamelOrdering::FiveOfAKind
    } else if count_set.contains(&4) {
      CamelOrdering::FourOfAKind
    } else if count_set.contains(&3) && count_set.contains(&2) {
      CamelOrdering::FullHouse
    } else if count_set.contains(&3) {
      CamelOrdering::ThreeOfAKind
    } else if count_set.contains(&2) && distinct_keys <= 3 {
      CamelOrdering::TwoPairs
    } else if count_set.contains(&2) {
      CamelOrdering::OnePair
    } else {
      CamelOrdering::HighCard
    }
  }

  fn card_strength(card: Card) -> Self::CardOrdering {
    base_card_strength(card)
  }
}

impl<'a> From<&'a Hand> for StandardOrdering<'a> {
  fn from(hand: &'a Hand) -> Self {
    StandardOrdering(hand)
  }
}

impl<'a> PartialOrd for StandardOrdering<'a> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a> Ord for StandardOrdering<'a> {
  fn cmp(&self, other: &Self) -> Ordering {
    compare_hands(self, other)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::camel_cards::hand::test_mocks::*;

  #[test]
  fn test_hand_ordering() {
    for (hand, other_hand) in [
      (five_of_a_kind(), four_of_a_kind()),
      (four_of_a_kind(), full_house()),
      (full_house(), three_of_a_kind()),
      (three_of_a_kind(), two_pair()),
      (two_pair(), one_pair()),
      (one_pair(), high_card()),

      (four_of_a_kind_stronger(), four_of_a_kind_weaker()),
      (full_house_stronger(), full_house_weaker()),
    ] {
      let ordering = StandardOrdering::from(&hand);
      let other_ordering = StandardOrdering::from(&other_hand);
      assert!(ordering > other_ordering);
    }
  }

  #[test]
  fn test_hand_strength() {
    for (hand, other_hand) in [
      (five_of_a_kind(), four_of_a_kind()),
      (four_of_a_kind(), full_house()),
      (full_house(), three_of_a_kind()),
      (three_of_a_kind(), two_pair()),
      (two_pair(), one_pair()),
      (one_pair(), high_card()),
    ] {
      assert!(StandardOrdering::hand_strength(&hand) > StandardOrdering::hand_strength(&other_hand));
    }
  }
}
