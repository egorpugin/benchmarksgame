-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Roman Kashitsyn

import qualified Data.ByteString.Char8 as BS
import           System.Environment    (getArgs)
import           System.IO             (stdout)

alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

type DistF = Double -> Char

-- These cumulative distribution functions are not very pretty, but they
-- give ~10% speedup in execution time comparing to a list of pairs.
iubF, homoF :: DistF
iubF f
  | f < 0.27 = ʼaʼ
  | f < 0.39 = ʼcʼ
  | f < 0.51 = ʼgʼ
  | f < 0.78 = ʼtʼ
  | f < 0.80 = ʼBʼ
  | f < 0.82 = ʼDʼ
  | f < 0.84 = ʼHʼ
  | f < 0.86 = ʼKʼ
  | f < 0.88 = ʼMʼ
  | f < 0.90 = ʼNʼ
  | f < 0.92 = ʼRʼ
  | f < 0.94 = ʼSʼ
  | f < 0.96 = ʼVʼ
  | f < 0.98 = ʼWʼ
  | otherwise = ʼYʼ

homoF f
  | f < 0.302954942668  = ʼaʼ
  | f < 0.5009432431601 = ʼcʼ
  | f < 0.6984905497992 = ʼgʼ
  | otherwise = ʼtʼ

lineWidth, modulo :: Int
lineWidth = 60
modulo = 139968

nextSeed :: Int -> Int
nextSeed seed = (3877 * seed + 29573) `rem` modulo

printRepeatedFasta :: BS.ByteString -> Int -> IO ()
printRepeatedFasta s = go lineWidth n
  where
    !n = BS.length s
    go 0 sn left = BS.putStrLn "" >> go lineWidth sn left
    go w  0 left = go w n left
    go w  _ 0    = if w == lineWidth then return () else BS.putStrLn ""
    go w sn left = do
      let toTake = w `min` sn `min` left
      BS.putStr (BS.take toTake $ BS.drop (n - sn) s)
      go (w - toTake) (sn - toTake) (left - toTake)

printRandomFasta :: DistF -> Int -> Int -> IO Int
printRandomFasta dist seed n = go n seed
  where
    genChar seed = Just (dist f, seedʼ)
      where !seedʼ = nextSeed seed
            !f = fromIntegral seedʼ / (fromIntegral modulo)

    go 0     !seed = return seed
    go total !seed = do
      let toTake = total `min` lineWidth
          (!b, Just seedʼ) = BS.unfoldrN toTake genChar seed
      BS.putStrLn b
      go (total - toTake) seedʼ

main :: IO ()
main = do
  n <- getArgs >>= readIO . head

  BS.putStrLn ">ONE Homo sapiens alu"
  printRepeatedFasta alu (2 * n)

  BS.putStrLn ">TWO IUB ambiguity codes"
  seedʼ <- printRandomFasta iubF 42 (3 * n)

  BS.putStrLn ">THREE Homo sapiens frequency"
  printRandomFasta homoF seedʼ (5 * n)
  return ()

