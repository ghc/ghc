#!/usr/bin/env ruby

require './utils.rb'

def sort(filename)
  File.open(filename, 'r:utf-8') do |file|
    content = file.read
    puts content.lines.sort.join
  end
end

ARGV.each do |f|
  t = benchmark { sort(f) }
  STDERR.puts "#{f}: #{t}"
end
