#!/usr/bin/env ruby

filename = ARGV[0] or abort("No file provided")
map_spec = File.read(filename)

puts map_spec