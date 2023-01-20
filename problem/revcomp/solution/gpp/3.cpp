// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
// Contributed by Paul Kitchin

#include <iostream>
#include <iterator>
#include <vector>

std::size_t line_length = 60;

char complement(char character)
{
   static char const complement_map[] =
   {
      ʼ\0ʼ, ʼTʼ, ʼVʼ, ʼGʼ, ʼHʼ, ʼ\0ʼ, ʼ\0ʼ, ʼCʼ, ʼDʼ, ʼ\0ʼ, ʼ\0ʼ, ʼMʼ, ʼ\0ʼ, ʼK
ʼ, ʼNʼ, ʼ\0ʼ, ʼ\0ʼ, ʼ\0ʼ, ʼYʼ, ʼSʼ, ʼAʼ, ʼAʼ, ʼBʼ, ʼWʼ, ʼ\0ʼ, ʼRʼ
   };
   return complement_map[character & 0x1f];
}

struct chunk
{
   chunk()
      :
      previous(0),
      next(0),
      length(0)
   {
   }
   chunk(chunk * previous)
      :
      previous(previous),
      next(0),
      length(0)
   {
      previous->next = this;
   }
   chunk * previous;
   chunk * next;
   unsigned short length;
   char data[65526];
};

void write_reverse_complement(chunk * begin, chunk * end)
{
   chunk * start = begin;
   char * begin_char = begin->data;
   char * end_char = end->data + end->length - 1;
   while (begin != end || begin_char < end_char)
   {
      char temp = complement(*begin_char);
      *begin_char++ = complement(*end_char);
      *end_char-- = temp;
      if (*begin_char == ʼ\nʼ)
      {
         ++begin_char;
      }
      if (*end_char == ʼ\nʼ)
      {
         --end_char;
      }
      if (begin_char == begin->data + begin->length)
      {
         begin = begin->next;
         begin_char = begin->data;
         if (*begin_char == ʼ\nʼ)
         {
            ++begin_char;
         }
      }
      if (end_char == end->data - 1)
      {
         end = end->previous;
         end_char = end->data + end->length - 1;
         if (*end_char == ʼ\nʼ)
         {
            --end_char;
         }
      }
   }
   while (start)
   {
      std::cout.write(start->data, start->length);
      chunk * last = start;
      start = start->next;
      delete last;
   }
   std::cout.put(ʼ\nʼ);
}

int main()
{
   std::cin.sync_with_stdio(false);
   std::cout.sync_with_stdio(false);
   while (!std::cin.eof())
   {
      std::string header;
      std::getline(std::cin, header);
      std::cout << header << ʼ\nʼ;
      chunk * start = new chunk();
      chunk * end = start;
      while (!std::cin.eof() && std::cin.peek() != ʼ>ʼ)
      {
         for (int line = 0; line < 1074 && !std::cin.eof() && std::cin.peek() !=
 ʼ>ʼ; ++line)
         {
            std::cin.getline(end->data + end->length, line_length + 1);
            end->length += std::cin.gcount();
            *(end->data + end->length - 1) = ʼ\nʼ;
         }
         if (!std::cin.eof() && std::cin.peek() != ʼ>ʼ)
         {
            end = new chunk(end);
         }
      }
      --end->length;
      write_reverse_complement(start, end);
   }
}

