#!/usr/bin/env ruby

require './utils.rb'

def fold(filename, max_width)
  File.open(filename, 'r:utf-8') do |file|
    # Words in this paragraph
    paragraph = []

    file.each_line do |line|
      # If we encounter an empty line, we reformat and dump the current
      # paragraph
      if line.strip.empty?
        puts fold_paragraph(paragraph, max_width)
        puts
        paragraph = []
      # Otherwise, we append the words found in the line to the paragraph
      else
        paragraph.concat line.split
      end
    end

    # Last paragraph
    puts fold_paragraph(paragraph, max_width) unless paragraph.empty?
  end
end

# Fold a single paragraph to the desired width
def fold_paragraph(paragraph, max_width)
  # Gradually build our output
  str, *rest = paragraph
  width = str.length

  rest.each do |word|
    if width + word.length + 1 <= max_width
      str << ' ' << word
      width += word.length + 1
    else
      str << "\n" << word
      width = word.length
    end
  end

  str
end

ARGV.each do |f|
  t = benchmark { fold(f, 80) }
  STDERR.puts "#{f}: #{t}"
end
