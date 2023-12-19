use nom::{
  IResult,
  multi::count,
};
use std::fmt;
use std::collections::HashMap;

use crate::errors::ParseError;
use super::card::{Card, parse_card};

pub mod ordering;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hand {
  cards: Vec<Card>,
}

impl Hand {
  pub fn from_str(input: &str) -> Result<Self, ParseError> {
    parse_hand(input)
      .map(|(_rest, hand)| hand)
      .map_err(|_| ParseError::Hand { hand: input.to_string() })
  }

  pub fn cards(&self) -> &[Card] {
    &self.cards
  }

  fn as_count_map(&self) -> HashMap<Card, u32> {
    self.cards.iter().fold(
      HashMap::new(),
      |mut acc, card| {
        *acc.entry(*card).or_insert(0) += 1;
        acc
      }
    )
  }
}

impl From<&[Card; 5]> for Hand {
  fn from(slice: &[Card; 5]) -> Self {
    Hand { cards: slice.into() }
  }
}

impl From<&Hand> for String {
  fn from(hand: &Hand) -> Self {
    let mut hand_str = String::with_capacity(5);
    for card in hand.cards() {
      hand_str.push_str(card.into());
    }
    hand_str
  }
}

impl fmt::Display for Hand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", String::from(self))
  }
}

pub(super) fn parse_hand(input: &str) -> IResult<&str, Hand> {
  let (i, cards) = count(parse_card, 5)(input)?;
  Ok((i, Hand { cards }))
}

#[cfg(test)]
pub(crate) mod test_mocks {
  use super::*;

  pub(crate) fn five_of_a_kind() -> Hand { Hand::from(&[Card::A, Card::A, Card::A, Card::A, Card::A]) }
  pub(crate) fn four_of_a_kind() -> Hand { Hand::from(&[Card::A, Card::A, Card::N8, Card::A, Card::A]) }
  pub(crate) fn full_house() -> Hand { Hand::from(&[Card::N2, Card::N3, Card::N3, Card::N3, Card::N2]) }
  pub(crate) fn three_of_a_kind() -> Hand { Hand::from(&[Card::T, Card::T, Card::T, Card::N9, Card::N8]) }
  pub(crate) fn two_pair() -> Hand { Hand::from(&[Card::N2, Card::N3, Card::N4, Card::N3, Card::N2]) }
  pub(crate) fn one_pair() -> Hand { Hand::from(&[Card::A, Card::N2, Card::N3, Card::A, Card::N4]) }
  pub(crate) fn high_card() -> Hand { Hand::from(&[Card::N2, Card::N3, Card::N4, Card::N5, Card::N6]) }

  pub(crate) fn four_of_a_kind_stronger() -> Hand { Hand::from(&[Card::N3, Card::N3, Card::N3, Card::N3, Card::N2]) }
  pub(crate) fn four_of_a_kind_weaker() -> Hand { Hand::from(&[Card::N2, Card::A, Card::A, Card::A, Card::A]) }

  pub(crate) fn full_house_stronger() -> Hand { Hand::from(&[Card::N7, Card::N7, Card::N8, Card::N8, Card::N8]) }
  pub(crate) fn full_house_weaker() -> Hand { Hand::from(&[Card::N7, Card::N7, Card::N7, Card::N8, Card::N8]) }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_hand_as_count_map() {
    let hand = Hand {
      cards: vec![
        Card::A,
        Card::A,
        Card::Q,
        Card::Q,
        Card::Q,
      ]
    };
    let count_map = hand.as_count_map();
    assert_eq!(*count_map.get(&Card::A).expect("Has key"), 2);
    assert_eq!(*count_map.get(&Card::Q).expect("Has key"), 3);
    assert!(count_map.get(&Card::K).is_none());
  }

  #[test]
  fn test_format_hand() {
    assert_eq!("AA2AA".to_string(), String::from(&Hand::from(&[Card::A, Card::A, Card::N2, Card::A, Card::A])));
  }

  #[test]
  fn test_parse_hand() {
    let (_rest, hand) = parse_hand("T55J5").expect("Parses ok");
    assert_eq!(hand.cards().len(), 5);
    assert_eq!(hand.cards()[0], Card::T);
    assert_eq!(hand.cards()[1], Card::N5);
    assert_eq!(hand.cards()[2], Card::N5);
    assert_eq!(hand.cards()[3], Card::J);
    assert_eq!(hand.cards()[4], Card::N5);
  }
}
