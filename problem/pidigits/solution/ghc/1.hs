-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
-- contributed by Einar Karttunen
-- adapted from the OCaml version.

import System.Environment

floor_ev (q, r, s, t) x = (q*x + r) `div` (s*x + t)
comp (q,r,s,t) (qʼ,rʼ,sʼ,tʼ) = (q*qʼ + r*sʼ, q*rʼ + r*tʼ, s*qʼ + t*sʼ, s*rʼ + t*
tʼ)
next z = floor_ev z 3
safe z n = n == floor_ev z 4
prod z n = comp (10,-10 * n, 0, 1) z
cons z k = let den = 2*k+1 in comp z (fromIntegral k, fromIntegral (2*den), 0, f
romIntegral den)

digit :: Int -> (Integer,Integer,Integer,Integer) -> Int -> Int -> Int -> IO ()
digit k z 0 row col = putStrLn (take (10-col) "               "++"\t:"++show (ro
w+col))
digit k z n row col =
   if safe z y
      then if col == 10
         then do
            let rowʼ = row + 10
            putStr ("\t:"++show rowʼ++"\n"++show y)
            digit k (prod z y) (n-1) rowʼ 1
         else putStr (show y) >> digit k (prod z y) (n-1) row (col+1)
      else digit (k+1) (cons z k) n row col
   where y = next z

main = do
   [n] <- getArgs
   digit 1 (1,0,0,1) (read n) 0 0

