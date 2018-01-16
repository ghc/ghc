#!/usr/bin/env ruby

require './utils.rb'

def strip_tags(filename)
  File.open(filename, 'r:utf-8') do |file|
    str = file.read

    d = 0

    str.each_char do |c|
      d += 1 if c == '<'
      putc(if d > 0 then ' ' else c end)
      d -= 1 if c == '>'
    end
  end
end

ARGV.each do |f|
  t = benchmark { strip_tags(f) }
  STDERR.puts "#{f}: #{t}"
end
