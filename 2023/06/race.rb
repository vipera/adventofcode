#!/usr/bin/env ruby -I lib

require 'optparse'
require 'race_specification'

@options = {
  filename: nil,
  ignore_kerning: false,
}

option_parser = OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} -f FILENAME"

  opts.on '-f FILENAME', '--filename FILENAME', String, 'Race specification input filename' do |o|
    @options[:filename] = o
  end

  opts.on '-i', '--ignore-kerning', 'Ignore bad kerning in race specification' do |o|
    @options[:ignore_kerning] = true
  end
end

option_parser.parse!

unless @options[:filename]
  puts option_parser.help
  exit 1
end

specification = RaceSpecification.from_file(@options[:filename], ignore_kerning: @options[:ignore_kerning])
solution = specification.solve

if @options[:ignore_kerning]
  puts "Solution (ignoring kerning): #{solution}"
else
  puts "Solution: #{solution}"
end
