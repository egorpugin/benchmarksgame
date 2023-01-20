# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Aaron Tavistock

ALU = ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCA
GGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGC
GCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGA
GATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ

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
]

HOMOSAPIENS = [
  [ʼaʼ, 0.3029549426680],
  [ʼcʼ, 0.1979883004921],
  [ʼgʼ, 0.1975473066391],
  [ʼtʼ, 0.3015094502008],
]

class RandomSequence

  GR_IM = 139968.0
  GR_IA = 3877.0
  GR_IC = 29573.0

  attr_reader :value

  def initialize(seed_value, map, size)
    @size = size
    @value = seed_value
    @output_buffer = ʼʼ
    generate_map_value_method(map)
  end

  def render(label)
    puts ">#{label}"
    full_row_count, last_row_size = @size.divmod(60)
    while (full_row_count > 0)
      puts output_row(60)
      full_row_count -= 1
    end
    puts output_row(last_row_size) if last_row_size > 0
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
    @value = (@value * GR_IA + GR_IC) % GR_IM
    @value / GR_IM
  end

  def output_row(size)
    @output_buffer.clear
    while (size > 0)
      @output_buffer << map_value(next_item)
      size -= 1
    end
    @output_buffer
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

