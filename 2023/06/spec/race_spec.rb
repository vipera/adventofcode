require 'spec_helper'
require 'race'

RSpec.describe Race do
  describe '#better_times' do
    it 'returns' do
      race = Race.new(time: 7, winning_distance: 9)
      expect(race.better_times_count).to eq(4)
    end
  end
end