class Race
  attr_reader :time, :winning_distance

  def initialize(time:, winning_distance:)
    @time = time
    @winning_distance = winning_distance
  end

  def better_times_count
    # Distance increases by 1 milimeter/ms every ms the button is held
    # distance = (@time - hold_time) * hold_time
    # distance = (@time * hold_time) - (hold_time ** 2)
    #
    # So if for distance = current best distance, it's solved for = 0, the roots
    # are boundaries where distance is exceeded:
    #
    # (hold_time ** 2) - (@time * hold_time) + distance = 0
    #
    # a1 = (@time + sqrt(@time ** 2 - 4 * distance)) / 2
    # a2 = (@time - sqrt(@time ** 2 - 4 * distance)) / 2
    #
    # For distance = winning distance + 1, the roots represent when an
    # increasing button hold time reaches a new record before starting to lose
    # again due to not having enough time to grow.
    better_distance = @winning_distance + 1
    quadratic_root = Math.sqrt(@time ** 2 - 4 * better_distance)
    a1 = (@time + quadratic_root) / 2
    a2 = (@time - quadratic_root) / 2

    # Only interested in integers as button hold time is specified in whole
    # milliseconds (plus one because the range is inclusive).
    a1.floor - a2.ceil + 1
  end

end
