-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Roman Kashitsyn
-- Optimizations by Vassil Keremidchiev

import qualified Data.ByteString.Char8 as BS
import           System.Environment    (getArgs)
import           System.IO             (stdout, hSetBuffering, BufferMode(..))
import           Data.ByteString.Builder

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

printRepeatedFasta :: BS.ByteString -> Int -> Builder
printRepeatedFasta s x = go lineWidth n x mempty
  where
    !n = BS.length s
    nl = char8 ʼ\nʼ

    go 0 sn left before = go lineWidth sn left (before <> nl)
    go w 0  left before = go w n left before
    go w _  0    before = if w == lineWidth then before else before <> nl
    go w sn left before = go (w - toTake) (sn - toTake) (left - toTake) (before
<> (byteString newone)) where
      toTake = w `min` sn `min` left
      newone = BS.take toTake $ BS.drop (n - sn) s

printRandomFasta :: DistF -> Int -> Int -> (Builder, Int)
printRandomFasta dist seed n = go n seed mempty
  where
    nl = char8 ʼ\nʼ
    genChar seed = Just (dist f, seedʼ)
      where !seedʼ = nextSeed seed
            !f = fromIntegral seedʼ / (fromIntegral modulo)

    go 0     !seed before = (before, seed)
    go total !seed before = go (total - toTake) seedʼ (before <> (byteString b)
<> nl) where
      toTake = total `min` lineWidth
      (!b, Just seedʼ) = BS.unfoldrN toTake genChar seed

main :: IO ()
main = do
  n <- getArgs >>= readIO . head

  let a = ">ONE Homo sapiens alu\n"
      res = printRepeatedFasta alu (2 * n)

      b = ">TWO IUB ambiguity codes\n"
      (resʼ, seedʼ) = printRandomFasta iubF 42 (3 * n)

      c = ">THREE Homo sapiens frequency\n"
      (resʼʼ, _) = printRandomFasta homoF seedʼ (5 * n)

  hPutBuilder stdout $ a <> res <> b <> resʼ <> c <> resʼʼ

