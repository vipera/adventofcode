use std::cmp::Ordering;
use std::fmt;

use super::Card;
use super::Hand;
pub use standard::StandardOrdering;
pub use joker::JokerOrdering;

pub mod joker;
pub mod standard;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CamelOrdering {
  HighCard,
  OnePair,
  TwoPairs,
  ThreeOfAKind,
  FullHouse,
  FourOfAKind,
  FiveOfAKind,
}

impl From<&CamelOrdering> for &'static str {
  fn from(camel_ordering: &CamelOrdering) -> Self {
    match *camel_ordering {
      CamelOrdering::HighCard => "High card",
      CamelOrdering::OnePair => "One pair",
      CamelOrdering::TwoPairs => "Two pairs",
      CamelOrdering::ThreeOfAKind => "Three of a kind",
      CamelOrdering::FullHouse => "Full house",
      CamelOrdering::FourOfAKind => "Four of a kind",
      CamelOrdering::FiveOfAKind => "Five of a kind",
    }
  }
}

impl From<&CamelOrdering> for String {
  fn from(camel_ordering: &CamelOrdering) -> Self {
    let camel_ordering_str: &str = camel_ordering.into();
    camel_ordering_str.to_string()
  }
}

impl fmt::Display for CamelOrdering {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", String::from(self))
  }
}

pub trait OrderedHand {
  type CardOrdering: Ord;

  fn hand(&self) -> &Hand;

  /// Returns strength of a hand based on Camel Cards rules
  fn hand_strength(hand: &Hand) -> CamelOrdering;

  /// Returns the strength of a single card
  fn card_strength(card: Card) -> Self::CardOrdering;
}

pub fn compare_hands<T>(ordered_hand: &T, other_ordered_hand: &T) -> Ordering
where
  T: OrderedHand
{
  // first try matching by hand strength
  let hand_strength = T::hand_strength(ordered_hand.hand());
  let other_hand_strength = T::hand_strength(other_ordered_hand.hand());
  let ordering = match hand_strength.cmp(&other_hand_strength) {
    // otherwise, the hand with the largest card first wins
    Ordering::Equal => ordered_hand.hand().cards()
      .iter()
      .zip(other_ordered_hand.hand().cards())
      .fold(Ordering::Equal, |r, (c1, c2)|
        if r == Ordering::Equal {
          T::card_strength(*c1).cmp(&T::card_strength(*c2))
        } else {
          r
        }
      ),
    // hands are different
    gt_lt => gt_lt,
  };
  ordering
}

fn base_card_strength(card: Card) -> u32 {
  match card {
    Card::A => 14,
    Card::K => 13,
    Card::Q => 12,
    Card::J => 11,
    Card::T => 10,
    Card::N9 => 9,
    Card::N8 => 8,
    Card::N7 => 7,
    Card::N6 => 6,
    Card::N5 => 5,
    Card::N4 => 4,
    Card::N3 => 3,
    Card::N2 => 2,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_base_card_strength() {
    for (card, other_card) in [
      (Card::A, Card::K),
      (Card::K, Card::Q),
      (Card::J, Card::N5),
      (Card::A, Card::N5),
    ] {
      assert!(base_card_strength(card) > base_card_strength(other_card));
    }
  }
}
