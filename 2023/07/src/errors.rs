use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
  #[error("invalid hand: {hand:?}")]
  Hand { hand: String },

  #[error("invalid bid and hand: {bid_hand:?}")]
  BidHand { bid_hand: String }
}
