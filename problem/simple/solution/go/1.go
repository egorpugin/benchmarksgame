/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   line-by-line from Greg Buchholz's C program
*/

package main
import ("bufio"; "fmt"; "os"; "strconv")

func main () {
    bufout := bufio.NewWriter(os.Stdout); defer bufout.Flush()
    var w, h, x, y, i, bit_num int
    var byte_acc byte
    iter := 50
    limit := 2.0
    var Zr, Zi, Cr, Ci, Tr, Ti float64

    w,_ = strconv.Atoi(os.Args[1])
    h = w;

    fmt.Printf("P4\n%d %d\n",w, h)

    for y=0; y<h; y++ {

        for x=0; x<w; x++ {

            Zr = 0.0; Zi = 0.0
            Cr = 2*float64(x)/float64(w) - 1.5; Ci = 2*float64(y)/float64(h) - 1

            for i=0; i<iter; i++ {

                Tr = Zr*Zr - Zi*Zi + Cr
                Ti = 2*Zr*Zi + Ci
                Zr = Tr; Zi = Ti
                if Zr*Zr+Zi*Zi > limit*limit {
                    break
                }
            }
            if Zr*Zr+Zi*Zi > limit*limit {
                byte_acc = (byte_acc << 1) | 0x00
            } else {
                byte_acc = (byte_acc << 1) | 0x01
            }
            bit_num++

            if bit_num == 8 {

                bufout.WriteByte(byte_acc)
                byte_acc = 0
                bit_num = 0

            } else if x == w-1 {

                byte_acc = byte_acc << (8-w%8)
                bufout.WriteByte(byte_acc)
                byte_acc = 0
                bit_num = 0
            }

        }
    }
}

