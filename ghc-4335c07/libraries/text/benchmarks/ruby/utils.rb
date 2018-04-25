require 'benchmark'

def benchmark(&block)
  runs = 100
  total = 0

  runs.times do |i|
    result = Benchmark.measure(&block).total
    $stderr.puts "Run #{i}: #{result}"
    total += result
  end

  total / runs 
end
