# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Ziong

const revcompdata = Dict(
   ʼAʼ=> ʼTʼ, ʼaʼ=> ʼTʼ,
   ʼCʼ=> ʼGʼ, ʼcʼ=> ʼGʼ,
   ʼGʼ=> ʼCʼ, ʼgʼ=> ʼCʼ,
   ʼTʼ=> ʼAʼ, ʼtʼ=> ʼAʼ,
   ʼUʼ=> ʼAʼ, ʼuʼ=> ʼAʼ,
   ʼMʼ=> ʼKʼ, ʼmʼ=> ʼKʼ,
   ʼRʼ=> ʼYʼ, ʼrʼ=> ʼYʼ,
   ʼWʼ=> ʼWʼ, ʼwʼ=> ʼWʼ,
   ʼSʼ=> ʼSʼ, ʼsʼ=> ʼSʼ,
   ʼYʼ=> ʼRʼ, ʼyʼ=> ʼRʼ,
   ʼKʼ=> ʼMʼ, ʼkʼ=> ʼMʼ,
   ʼVʼ=> ʼBʼ, ʼvʼ=> ʼBʼ,
   ʼHʼ=> ʼDʼ, ʼhʼ=> ʼDʼ,
   ʼDʼ=> ʼHʼ, ʼdʼ=> ʼHʼ,
   ʼBʼ=> ʼVʼ, ʼbʼ=> ʼVʼ,
   ʼNʼ=> ʼNʼ, ʼnʼ=> ʼNʼ,
)


function print_buff(buff)
   out = String(take!(buff))
   l = length(out)
   n = 1
   outbuff = IOBuffer()
   for i in l:-1:1
      write(outbuff, revcompdata[out[i]])
      n % 60 == 0 ?  write(outbuff, ʼ\nʼ) : false
      n += 1
   end
   n % 60 > 1 ? write(outbuff, ʼ\nʼ) : false
   print(String(take!(outbuff)))
end

function main()
   buff = IOBuffer()
   line = readline()
   while !isempty(line)
      if line[1] == ʼ>ʼ
         buff.size > 0 ? print_buff(buff) : false
         println(line)
         buff = IOBuffer()
      else
         write(buff, line)
      end
      line = readline()
   end
   print_buff(buff)
end

main()

