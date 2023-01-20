# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Joerg Baumann

from sys import stdin, stdout
from os import cpu_count

reverse_translation = bytes.maketrans(
   bʼABCDGHKMNRSTUVWYabcdghkmnrstuvwyʼ,
   bʼTVGHCDMKNYSAABWRTVGHCDMKNYSAABWRʼ)

def reverse_complement(header, sequence):
   t = sequence.translate(reverse_translation, bʼ\n\r ʼ)
   output = bytearray()
   trailing_length = len(t) % 60
   if trailing_length: output += bʼ\nʼ + t[:trailing_length]
   for i in range(trailing_length, len(t), 60):
      output += bʼ\nʼ+ t[i:i+60]
   return header, output[::-1]

def read_sequences(file):
   for line in file:
      if line[0] == ord(ʼ>ʼ):
         header = line
         sequence = bytearray()
         for line in file:
            if line[0] == ord(ʼ>ʼ):
               yield header, sequence
               header = line
               sequence = bytearray()
            else:
               sequence += line
         yield header, sequence
         break

def reverse_and_print_task(q, c, v):
   while True:
      i = q.get()
      if i == None: break
      h, r = reverse_complement(*data[i])
      with c:
         while i != v.value:
            c.wait()
      write(h); write(r); flush()
      with c:
         v.value = i + 1
         c.notify_all()

if __name__==ʼ__main__ʼ :
   write = stdout.buffer.write
   flush = stdout.buffer.flush

   s = read_sequences(stdin.buffer)
   data = next(s)
   if cpu_count() == 1 or len(data[1]) < 1000000:
      from itertools import starmap
      def merge(v, g):
         yield v; yield from g
      for h, r in starmap(reverse_complement, merge(data, s)):
         write(h); write(r)
   else:
      from multiprocessing import Process, Queue, Value, Condition
      from ctypes import c_int

      data = [data] + list(s)
      q, c, v = (Queue(), Condition(), Value(c_int, 0))
      processes = [Process(target=reverse_and_print_task, args=(q, c, v))
         for _ in range(min(len(data), cpu_count()))]

      for p in processes: p.start()
      for i in range(len(data)): q.put(i)
      for p in processes: q.put(None)
      for p in processes: p.join()

