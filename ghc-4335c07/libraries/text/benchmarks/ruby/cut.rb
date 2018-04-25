#!/usr/bin/env ruby

require './utils.rb'

def cut(filename, l, r)
  File.open(filename, 'r:utf-8') do |file|
    file.each_line do |line|
      puts line[l, r - l]
    end
  end
end

ARGV.each do |f|
  t = benchmark { cut(f, 20, 40) }
  STDERR.puts "#{f}: #{t}"
end
