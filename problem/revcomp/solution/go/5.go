/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Сергей Фролкин.
 */

package main

import (
   "bufio"
   "bytes"
   "container/list"
   "io"
   "os"
)

const (
   BUF_SIZE = 24 * 1024 * 1024
   BEG_SEQ  = ʼ>ʼ
   EOL      = ʼ\nʼ
   WORKERS  = 100
)

var complement = [256]byte{
   ʼAʼ: ʼTʼ, ʼaʼ: ʼTʼ,
   ʼCʼ: ʼGʼ, ʼcʼ: ʼGʼ,
   ʼGʼ: ʼCʼ, ʼgʼ: ʼCʼ,
   ʼTʼ: ʼAʼ, ʼtʼ: ʼAʼ,
   ʼUʼ: ʼAʼ, ʼuʼ: ʼAʼ,
   ʼMʼ: ʼKʼ, ʼmʼ: ʼKʼ,
   ʼRʼ: ʼYʼ, ʼrʼ: ʼYʼ,
   ʼWʼ: ʼWʼ, ʼwʼ: ʼWʼ,
   ʼSʼ: ʼSʼ, ʼsʼ: ʼSʼ,
   ʼYʼ: ʼRʼ, ʼyʼ: ʼRʼ,
   ʼKʼ: ʼMʼ, ʼkʼ: ʼMʼ,
   ʼVʼ: ʼBʼ, ʼvʼ: ʼBʼ,
   ʼHʼ: ʼDʼ, ʼhʼ: ʼDʼ,
   ʼDʼ: ʼHʼ, ʼdʼ: ʼHʼ,
   ʼBʼ: ʼVʼ, ʼbʼ: ʼVʼ,
   ʼNʼ: ʼNʼ, ʼnʼ: ʼNʼ,
}

type Task struct {
   buf  []byte
   num  int
   last bool
}

var (
   code_chan  = make(chan Task)
   print_chan = make(chan Task)
   done       = make(chan bool)
)

func print_worker(wr io.Writer) {
   expect_num := 0
   buffer := list.New()
   print := func(task Task) {
      wr.Write(task.buf)
      expect_num++
      if task.last {
         done <- true
      }
   }
TaskLoop:
   for task := range print_chan {
      if task.num == expect_num {
         print(task)
         var next *list.Element
         for b := buffer.Front(); b != nil; b = next {
            if b.Value.(Task).num == expect_num {
               print(b.Value.(Task))
               next = b.Next()
               buffer.Remove(b)
            } else {
               break
            }
         }
      } else {
         for b := buffer.Front(); b != nil; b = b.Next() {
            if task.num < b.Value.(Task).num {
               buffer.InsertBefore(task, b)
               continue TaskLoop
            }
         }
         buffer.PushBack(task)
      }
   }
}

func Reverse(buf []byte, i, j int) {
   for i < j {
      ci := buf[i]
      if ci == EOL {
         i++
         ci = buf[i]
      }
      cj := buf[j]
      if cj == EOL {
         j--
         cj = buf[j]
      }
      buf[i] = complement[cj]
      buf[j] = complement[ci]
      i++
      j--
   }
}

func code_worker() {
   for task := range code_chan {
      start := bytes.IndexByte(task.buf, EOL)
      if start >= 0 {
         end := len(task.buf)
         if !task.last {
            end--
         }
         Reverse(task.buf, start+1, end-1)
      }
      print_chan <- task
   }
}

func main() {
   for i := 0; i < WORKERS; i++ {
      go code_worker()
   }
   go print_worker(os.Stdout)

   brd := bufio.NewReaderSize(os.Stdin, BUF_SIZE)
   num := 0
   for {
      buf, err := brd.ReadBytes(BEG_SEQ)
      if err != nil && err != io.EOF {
         panic(err)
      }
      code_chan <- Task{buf, num, err == io.EOF}
      num++
      if err == io.EOF {
         break
      }
   }

   <-done
}

