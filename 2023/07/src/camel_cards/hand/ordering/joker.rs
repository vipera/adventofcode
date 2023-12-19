use std::cmp::Ordering;
use std::collections::BinaryHeap;

use super::Card;
use super::{Hand, OrderedHand, CamelOrdering, compare_hands, base_card_strength};

/// Newtype for representing a hand with jokers taking the place of cards
#[derive(PartialEq, Eq)]
pub struct JokerOrdering<'a>(&'a Hand);

impl<'a> OrderedHand for JokerOrdering<'a> {
  type CardOrdering = u32;

  fn hand(&self) -> &Hand { self.0 }

  fn hand_strength(hand: &Hand) -> CamelOrdering {
    let mut count_map = hand.as_count_map();
    let joker_count = count_map.remove(&Card::J).unwrap_or(0);
    let mut counts: Vec<_> = count_map.values().copied().collect();

    let mut heap = BinaryHeap::new();
    heap.extend(counts.drain(..));

    let mut heap_fullhouse = heap.clone();
    let mut heap_twopairs = heap.clone();

    if heap.is_empty() || heap.peek() == Some(&(5 - joker_count)) {
      CamelOrdering::FiveOfAKind
    } else if heap.peek() == Some(&(4 - joker_count)) {
      CamelOrdering::FourOfAKind
    } else if heap_fullhouse.pop() == Some(3 - joker_count) && heap_fullhouse.pop() == Some(2) {
      CamelOrdering::FullHouse
    } else if heap.peek() == Some(&(3 - joker_count)) {
      CamelOrdering::ThreeOfAKind
    } else if heap_twopairs.pop() == Some(2) && heap_twopairs.pop() == Some(2) {
      CamelOrdering::TwoPairs
    } else if heap.peek() == Some(&(2 - joker_count)) {
      CamelOrdering::OnePair
    } else {
      // won't actually happen
      CamelOrdering::HighCard
    }
  }

  fn card_strength(card: Card) -> Self::CardOrdering {
    match card {
      Card::J => 0,
      card => base_card_strength(card),
    }
  }
}

impl<'a> From<&'a Hand> for JokerOrdering<'a> {
  fn from(hand: &'a Hand) -> Self {
    JokerOrdering(hand)
  }
}

impl<'a> PartialOrd for JokerOrdering<'a> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a> Ord for JokerOrdering<'a> {
  fn cmp(&self, other: &Self) -> Ordering {
    compare_hands(self, other)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_hand_ordering() {
    for (hand, other_hand) in [
      (Hand::from_str("JKKK2").unwrap(), Hand::from_str("QQQ23").unwrap()),
      (Hand::from_str("QQQQ2").unwrap(), Hand::from_str("JKKK2").unwrap()),
      (Hand::from_str("KTJJT").unwrap(), Hand::from_str("T55J5").unwrap()),
    ] {
      let ordering = JokerOrdering::from(&hand);
      let other_ordering = JokerOrdering::from(&other_hand);
      assert!(ordering > other_ordering);
    }
  }

  #[test]
  fn test_hand_strength() {
    let five_of_a_kind = Hand::from_str("QQQQQ").unwrap();
    assert_eq!(JokerOrdering::hand_strength(&five_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QQQQJ").unwrap()));
    assert_eq!(JokerOrdering::hand_strength(&five_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QQQJJ").unwrap()));
    assert_eq!(JokerOrdering::hand_strength(&five_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QQJJJ").unwrap()));
    assert_eq!(JokerOrdering::hand_strength(&five_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QJJJJ").unwrap()));
    assert_eq!(JokerOrdering::hand_strength(&five_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("JJJJJ").unwrap()));

    let four_of_a_kind = Hand::from_str("KKKKQ").unwrap();
    assert_eq!(JokerOrdering::hand_strength(&four_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QQQJ2").unwrap()));
    assert_eq!(JokerOrdering::hand_strength(&four_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QQJJ2").unwrap()));
    assert_eq!(JokerOrdering::hand_strength(&four_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("QJJJ2").unwrap()));

    let full_house = Hand::from_str("KKQQQ").unwrap();
    assert_eq!(JokerOrdering::hand_strength(&full_house), JokerOrdering::hand_strength(&Hand::from_str("KKQQJ").unwrap()));

    let three_of_a_kind = Hand::from_str("KKK23").unwrap();
    assert_eq!(JokerOrdering::hand_strength(&three_of_a_kind), JokerOrdering::hand_strength(&Hand::from_str("KKJ23").unwrap()));

    let one_pair = Hand::from_str("KK234").unwrap();
    assert_eq!(JokerOrdering::hand_strength(&one_pair), JokerOrdering::hand_strength(&Hand::from_str("KJ234").unwrap()));
  }

  #[test]
  fn test_card_strength() {
    // every card is stronger than the joker card individually
    for (card, other_card) in [
      (Card::A, Card::J),
      (Card::K, Card::J),
      (Card::N9, Card::J),
      (Card::N2, Card::J),
    ] {
      assert!(JokerOrdering::card_strength(card) > JokerOrdering::card_strength(other_card));
    }
  }
}
