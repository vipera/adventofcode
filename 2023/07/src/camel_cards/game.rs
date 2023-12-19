use super::bid_hand::BidHand;
use super::hand::ordering::{
  compare_hands,
  OrderedHand,
  JokerOrdering,
  StandardOrdering,
};

#[derive(Debug)]
pub struct Game {
  bid_hands: Vec<BidHand>,
  jokers: bool,
}

impl Game {
  pub fn from_lines(lines: impl Iterator<Item = String>) -> Self {
    let bid_hands = lines.filter_map(|line| {
      match BidHand::from_str(&line) {
        Ok(bid_hand) => Some(bid_hand),
        Err(hand_err) => {
          println!("Error parsing hand: {}", hand_err);
          None
        }
      }
    }).collect();

    Game { bid_hands, jokers: false }
  }

  pub fn set_jokers(&mut self, jokers: bool) {
    self.jokers = jokers;
  }

  pub fn play(&self) {
    let mut sorted_bid_hands = self.bid_hands.clone();
    sorted_bid_hands.sort_by(|h1, h2| {
      if self.jokers {
        compare_hands(&JokerOrdering::from(h1.hand()), &JokerOrdering::from(h2.hand()))
      } else {
        compare_hands(&StandardOrdering::from(h1.hand()), &StandardOrdering::from(h2.hand()))
      }
    });

    let total_score = sorted_bid_hands.into_iter()
      .enumerate()
      .fold(0, |sum, (i, bid_hand)| {
        let js = JokerOrdering::hand_strength(bid_hand.hand());
        println!("Hand: {} [{}], bid: {}", bid_hand.hand(), js, bid_hand.bid());
        sum + (u32::try_from(i).unwrap() + 1) * bid_hand.bid()
      });

    println!("Total score of bids: {}", total_score);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_game_from_lines() {
    let lines = vec![
      "KKKKK 1".to_string(),
      "QQQQQ 2".to_string(),
    ];
    let game = Game::from_lines(lines.into_iter());
    assert_eq!(game.bid_hands[0].bid(), 1);
    assert_eq!(game.bid_hands[1].bid(), 2);
  }
}
