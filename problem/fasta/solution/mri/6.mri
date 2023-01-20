# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Aaron Tavistock

ALU = ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCA
GGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGC
GCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGA
GATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ.freeze

IUB = [
  [ʼaʼ, 0.27],
  [ʼcʼ, 0.12],
  [ʼgʼ, 0.12],
  [ʼtʼ, 0.27],
  [ʼBʼ, 0.02],
  [ʼDʼ, 0.02],
  [ʼHʼ, 0.02],
  [ʼKʼ, 0.02],
  [ʼMʼ, 0.02],
  [ʼNʼ, 0.02],
  [ʼRʼ, 0.02],
  [ʼSʼ, 0.02],
  [ʼVʼ, 0.02],
  [ʼWʼ, 0.02],
  [ʼYʼ, 0.02],
].freeze

HOMOSAPIENS = [
  [ʼaʼ, 0.3029549426680],
  [ʼcʼ, 0.1979883004921],
  [ʼgʼ, 0.1975473066391],
  [ʼtʼ, 0.3015094502008],
].freeze

class RandomSequence

  MULTIPLIER = 3877
  INCREMENT = 29573
  MODULUS = 139968
  FLOAT_MODULUS = MODULUS.to_f

  ROW_SIZE = 60

  attr_reader :value

  def initialize(seed_value, map, size)
    @size = size
    @value = seed_value
    @output = ʼ ʼ * ROW_SIZE
    generate_map_value_method(map)
  end

  def render(label)
    puts ">#{label}"
    full_row_count, last_row_size = @size.divmod(60)
    while (full_row_count > 0)
      puts output_row(ROW_SIZE)
      full_row_count -= 1
    end
    if last_row_size > 0
      @output = ʼ ʼ*last_row_size
      puts output_row(last_row_size)
    end
  end

  private

  def generate_map_value_method(map)
    accum_percentage = 0.0

    conditions = []
    map.each do |letter, percentage|
      accum_percentage += percentage
      conditions << %[(value <= #{accum_percentage} ? #{letter.ord} : ]
    end
    conditions[-1] = "#{map.last.first.ord}" # Substitute last condition for fix
ed value
    conditions << ʼ)ʼ * (map.size - 1)

    instance_eval %[def map_value(value); #{conditions.join}; end]
  end

  def next_item
    @value = (@value * MULTIPLIER + INCREMENT) % MODULUS
    @value.to_f / FLOAT_MODULUS
  end

  def output_row(size)
    idx = 0
    while idx < size
      @output.setbyte(idx, map_value(next_item))
      idx += 1
    end
    @output
  end

end

class RepeatSequence

  def initialize(seed_sequence, size)
    repeats = (size / seed_sequence.size).to_i + 1
    seq = seed_sequence * repeats
    @sequence = seq[0,size]
  end

  def render(label)
    puts ">#{label}"
    seq_size = @sequence.size - 1
    0.step(seq_size, 60) do |x|
      puts @sequence[x, 60]
    end
  end

end

size = (ARGV[0] || 27).to_i

one = RepeatSequence.new(ALU, size*2)
one.render(ʼONE Homo sapiens aluʼ)

two = RandomSequence.new(42, IUB, size*3)
two.render(ʼTWO IUB ambiguity codesʼ)

three = RandomSequence.new(two.value, HOMOSAPIENS, size*5)
three.render(ʼTHREE Homo sapiens frequencyʼ)

