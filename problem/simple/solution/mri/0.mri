# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

# line-by-line from Greg Buchholzʼs C program







w, h, x, y, bit_num = 0, 0, 0, 0, 0
byte_acc = 0
i, iter = 0, 50
limit = 2.0
zr, zi, cr, ci, tr, ti = 0.0, 0.0, 0.0, 0.0, 0.0, 0.0

w = ARGV[0].to_i
h = w

puts "P4\n#{w} #{h}"

h.times do |y|

    w.times do |x|

        zr, zi = 0.0, 0.0
        cr = 2.0*x/w - 1.5; ci = 2.0*y/h - 1.0

        iter.times do

            tr = zr*zr - zi*zi + cr
            ti = 2.0*zr*zi + ci
            zr = tr; zi = ti
            if zr*zr+zi*zi > limit*limit
                break
            end
        end
        if zr*zr+zi*zi > limit*limit
            byte_acc = (byte_acc << 1) | 0x00
        else
            byte_acc = (byte_acc << 1) | 0x01
        end
        bit_num += 1

        if bit_num == 8

            print byte_acc.chr
            byte_acc = 0;
            bit_num = 0

        elsif x == w-1

            byte_acc = byte_acc << (8-w%8)
            print byte_acc.chr
            byte_acc = 0;
            bit_num = 0
        end

   end
end


