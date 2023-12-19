use nom::{
  IResult,
  bytes::complete::tag,
  character::complete::digit1,
  combinator::map_res,
};

use crate::errors::ParseError;
use super::hand::{Hand, parse_hand};

#[derive(Debug, Clone)]
pub struct BidHand {
  hand: Hand,
  bid: u32,
}

impl BidHand {
  pub fn from_str(input: &str) -> Result<Self, ParseError> {
    parse_bid_hand(input)
      .map(|(_rest, bid_hand)| bid_hand)
      .map_err(|_| ParseError::BidHand { bid_hand: input.to_string() })
  }

  pub fn hand(&self) -> &Hand {
    &self.hand
  }

  pub fn bid(&self) -> u32 {
    self.bid
  }
}

fn parse_bid_hand(input: &str) -> IResult<&str, BidHand> {
  let (i, hand) = parse_hand(input)?;
  let (i, _) = tag(" ")(i)?;
  let (i, bid) = map_res(digit1, str::parse)(i)?;
  Ok((i, BidHand { hand, bid }))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_bid_hand() {
    let (_rest, bid_hand) = parse_bid_hand("QQQJA 483").expect("Parses ok");
    assert_eq!(bid_hand.hand().cards().len(), 5);
    assert_eq!(bid_hand.bid(), 483);
  }
}
