require 'spec_helper'
require 'race_specification'

RSpec.describe RaceSpecification do
  describe '#solve' do
    where(:times, :distances, :score) do
      [
        [[7, 15, 30], [9, 40, 200], 288],
      ]
    end

    with_them do
      it 'finds the correct solution' do
        races = times.zip(distances).map {|t, d| Race.new(time: t, winning_distance: d)}
        specification = RaceSpecification.new(races)
        expect(specification.solve).to eq(score)
      end
    end
  end

  describe '::parse_race_line' do
    where(:line, :values) do
      [
        ['Time:      7  15   30', [7, 15, 30]],
        ['Distance:  9  40  200', [9, 40, 200]],
        ['X:1 2 3',               [1, 2, 3]],
      ]
    end

    with_them do
      it 'parses a valid list of races' do
        expect(RaceSpecification.send(:parse_race_line, line)).to contain_exactly(*values)
      end
    end
  end

  describe '::parse_race_line_ignore_spaces' do
  end

  describe '::parse_specification' do
    it 'parses a valid race specification' do
      race_spec = ["Time:      7  15   30\n", 'Distance:  9  40  200']
      races = RaceSpecification.send(:parse_specification, race_spec)
      expect(races).to match_array([
        have_attributes(time: 7, winning_distance: 9),
        have_attributes(time: 15, winning_distance: 40),
        have_attributes(time: 30, winning_distance: 200),
      ])
    end
  end
end