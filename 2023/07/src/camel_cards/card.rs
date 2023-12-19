use nom::{
  IResult,
  branch::alt,
  bytes::complete::tag,
  error::context,
};
use std::fmt;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Card {
  A,
  K,
  Q,
  J,
  T,
  N9,
  N8,
  N7,
  N6,
  N5,
  N4,
  N3,
  N2,
}

impl From<&str> for Card {
  fn from(input: &str) -> Self {
    match input {
      "A" => Self::A,
      "K" => Self::K,
      "Q" => Self::Q,
      "J" => Self::J,
      "T" => Self::T,
      "9" => Self::N9,
      "8" => Self::N8,
      "7" => Self::N7,
      "6" => Self::N6,
      "5" => Self::N5,
      "4" => Self::N4,
      "3" => Self::N3,
      "2" => Self::N2,
      _ => unimplemented!("No other cards known"),
    }
  }
}

impl From<&Card> for &'static str {
  fn from(card: &Card) -> Self {
    match *card {
      Card::A => "A",
      Card::K => "K",
      Card::Q => "Q",
      Card::J => "J",
      Card::T => "T",
      Card::N9 => "9",
      Card::N8 => "8",
      Card::N7 => "7",
      Card::N6 => "6",
      Card::N5 => "5",
      Card::N4 => "4",
      Card::N3 => "3",
      Card::N2 => "2",
    }
  }
}

impl From<&Card> for String {
  fn from(card: &Card) -> Self {
    let card_str: &str = card.into();
    card_str.to_string()
  }
}

impl fmt::Display for Card {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", String::from(self))
  }
}

pub(super) fn parse_card(input: &str) -> IResult<&str, Card> {
  context("card", alt(
    (tag("A"), tag("K"), tag("Q"), tag("J"), tag("T"), tag("9"), tag("8"),
      tag("7"), tag("6"), tag("5"), tag("4"), tag("3"), tag("2"))
  ))(input)
    .map(|(next_input, res)| (next_input, res.into()))
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_format_card() {
    assert_eq!("A".to_string(), String::from(&Card::A));
    assert_eq!("5".to_string(), String::from(&Card::N5));
  }

  #[test]
  fn test_parse_card() {
    assert_eq!(parse_card("A").expect("Parses ok").1, Card::A);
    assert_eq!(parse_card("K").expect("Parses ok").1, Card::K);
    assert_eq!(parse_card("5").expect("Parses ok").1, Card::N5);
  }
}
