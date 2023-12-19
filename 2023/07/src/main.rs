use clap::Parser;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

mod camel_cards;
mod errors;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
  /// Config file
  #[arg(
    short = 'f',
    long = "filename",
    value_name = "FILENAME",
    help = "Puzzle file to load",
  )]
  filename: String,

  #[arg(
    short = 'j',
    long = "jokers",
    help = "Interpret J as jokers",
  )]
  jokers: bool,
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
  let file = File::open(filename)?;
  Ok(io::BufReader::new(file).lines())
}

fn main() {
  let args = Args::parse();
  if let Ok(game_lines) = read_lines(&args.filename) {
    let game_lines_str = game_lines.filter_map(|line| {
      match line {
        Ok(str) => Some(str),
        Err(e) => {
          println!("Error reading lines: {}", e);
          None
        }
      }
    });
    let mut game = camel_cards::Game::from_lines(game_lines_str);
    game.set_jokers(args.jokers);
    game.play();
  }
}
