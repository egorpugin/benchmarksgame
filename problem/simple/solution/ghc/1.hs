-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

-- contributed by Greg Buchholz
-- import bitrot fixed by Isaac Gouy
-- overflowing Chars fixed by Artem Pelenitsyn
-- memory exhaustion fixed by Noughtmare

import qualified Data.ByteString.Lazy as B
import Data.Complex
import Data.Word
import System.Environment

main = do   [arg] <- getArgs
            let width = read arg
            let pts = points width width
            putStr $ "P4\n" ++ arg ++ " " ++ arg ++ "\n"
            B.putStr . B.pack . makePBM 0 0 $ fractal pts

limit  = 2
iter   = 50+1 -- add one to compensate for the 'iterate' function

points width height = [(2*x/w - 1.5) :+ (2*y/h - 1) | y<-[0..h-1], x<-[0..w-1]]
                    where w = fromIntegral width
                          h = fromIntegral height

mandel c z = z * z + c

fractal = map ((\f-> length (takeIter (iterate f (0:+0)))) . mandel)
        where takeIter a = take iter (takeWhile (\x-> magnitude x < limit) a)

makePBM i acc []     = [acc * 2^(8-i)]
makePBM i acc (x:xs) | i==8      = acc : makePBM 0 0 (x:xs)
                     | otherwise = makePBM (i+1) n xs
                                    where
                                      n = if x==iter then acc*2+1 else acc*2

