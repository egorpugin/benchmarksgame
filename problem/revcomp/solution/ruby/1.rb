# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Aaron Tavistock

def process_segment(segment)
  (header, *body, tail) = segment.split("\n")

  # This is an ugly way to handle the first row, but returning nil causes the fo
rk to implode
  return '' if !tail
  body << tail if tail != '>'

  sequence = body.join
  sequence.reverse!
  sequence.tr!(
    'wsatugcyrkmbdhvnATUGCYRKMBDHVN',
    'WSTAACGRYMKVHDBNTAACGRYMKVHDBN'
  )

  results = [">#{header}"]

  idx = 0
  length = sequence.length
  while idx < length
    results << sequence[idx,60]
    idx += 60
  end

  results.join("\n")
end

def forking_worker(segment)
  reader, writer = IO.pipe

  pid = Process.fork do
    begin
      reader.close
      results = process_segment(segment)
      Marshal.dump(results, writer) if results
    ensure
      writer.close
    end
  end

  writer.close
  begin
    results = Marshal.load(reader)
  ensure
    reader.close
  end
  Process.waitpid(pid)

  results
end

threads = []
$stdin.each_line('>') do |segment|
  threads << Thread.new do
      Thread.current[:output] = forking_worker(segment)
  end
end
threads.each(&:join)
threads.each do |thread|
  if !thread[:output].empty?  # Artifact of non-empty results from forking worke
r
    puts thread[:output]
  end
end

