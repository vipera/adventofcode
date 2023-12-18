require 'race'

class RaceSpecification

  def initialize(races)
    @races = races
  end

  def self.from_file(filename, **options)
    races = parse_from_file(filename, **options)
    self.new(races)
  end

  def solve
    @races.map(&:better_times_count).inject(:*)
  end

  private

    def self.parse_from_file(filename, **options)
      lines = File.readlines(filename)
      parse_specification(lines, **options)
    end

    def self.parse_specification(lines, ignore_kerning: false)
      times, distances = *lines.map do |line|
        if ignore_kerning
          parse_race_line_ignore_spaces(line)
        else
          parse_race_line(line)
        end
      end
      times.zip(distances).map do |time, distance|
        Race.new(time: time, winning_distance: distance)
      end
    end

    def self.parse_race_line(line)
      number_matches = match_numbers(line)
      numbers = number_matches.scan(/\d+/).map(&:to_i)
    end

    def self.parse_race_line_ignore_spaces(line)
      number_matches = match_numbers(line)
      without_spaces = number_matches.gsub(/\s/, '')
      without_spaces.scan(/\d+/).map(&:to_i)
    end

    def self.match_numbers(line)
      _, number_matches = *line.match(/^\w+:\s*(.*)$/)
      number_matches
    end
end