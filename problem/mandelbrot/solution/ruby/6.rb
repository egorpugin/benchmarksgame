# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Aaron Tavistock

require 'thread'

WORKER_COUNT = begin
  cpu_count = if File.readable?('/proc/cpuinfo') # Linux
    %x(cat /proc/cpuinfo | grep -c processor).chomp.to_i
  elsif File.executable?('/usr/sbin/sysctl')  #OS/X
    %x(/usr/sbin/sysctl -n hw.ncpu).chomp.to_i
  else
    1
  end
  [(cpu_count * 2.0).to_i, 2].max
end

class WorkerPool

  def initialize
    @work = Queue.new
    @pool = Array.new(WORKER_COUNT) do |i|
      Thread.new do
        Thread.current[:id] = i
        catch(:exit) do
          while(true) do
            work, args = @work.pop
            work.call(*args)
          end
        end
      end
    end
  end

  def schedule(*args, &block)
    @work << [block, args]
  end

  def shutdown
    @pool.size.times do
      schedule { throw :exit }
    end
    @pool.each do |t|
      t.join
    end
  end

end

class Mandel

  attr_reader :output

  def self.render(size)
    m = Mandel.new(size)
    m.process
    print "#{m.header}\n#{m.output.join}"
  end

  def initialize(size)
    @size = size.to_i
    @output = Array.new(@size)
    @two_over_size = 2.0 / @size.to_f
  end

  def process
    workers = WorkerPool.new  # Using a thread pool to manage processes
    @size.times do |row|
      workers.schedule(row) do |y|
        read, write = IO.pipe
        Process.fork do
          read.close
          ci = (@two_over_size * y.to_f) - 1.0
          write.print( render_row(ci) )
        end
        Process.wait
        write.close
        @output[y] = read.read
      end
    end
    workers.shutdown
  end

  def header
    "P4\n#{@size} #{@size}"
  end

  private

  def render_row(ci)
    row_bits = Array.new(@size) do |col|
      cr = (@two_over_size * col.to_f) - 1.5
      get_bit(cr, ci)
    end

    row = ''
    row_bits.each_slice(8) do |byte|
      if byte.size < 8
        byte = byte.fill(0b0, byte.size, 8 - byte.size)
      end
      row << byte.join.to_i(2).chr
    end

    row
  end

  def get_bit(cr, ci)
    zrzr = 0.0
    zizi = 0.0
    zrzi = 0.0

    count = 50
    while count > 0

      zr = zrzr - zizi + cr
      zi = 2.0 * zrzi + ci

      zrzr = zr * zr
      zizi = zi * zi
      zrzi = zr * zi

      return 0b0 if zrzr + zizi > 4.0

      count -= 1
    end

    0b1
  end

end

Mandel.render(ARGV.shift)

