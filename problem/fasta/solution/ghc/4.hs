-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Jaro Reinders

module Main where

import Data.Word
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable.Mutable as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent
import Control.Monad
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Char
import Data.Maybe
import Data.IORef
import System.Environment
import Data.Int
import Data.List

-- First some constants

lineLength :: Int
lineLength = 60

workers :: Int
workers = 4

ia, ic, im :: Word32
ia = 3877
ic = 29573
im = 139968

alu :: B.ByteString
alu =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGG\
  \AGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGG\
  \CGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAG\
  \TGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

-- An mvar that is used to ensure that no two threads will print to stdout at
-- the same time.
output :: MVar ()
output = unsafePerformIO (newMVar ())

-- Make sure that the program does not exit before all child processes are
-- done.

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> putMVar children []
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

-- Firstly the repeated fasta

-- Helper function that cuts a lazy bytestring into a list of equal size
-- chunks.
toChunksOf :: Int64 -> BL.ByteString -> [BL.ByteString]
toChunksOf !n = unfoldr (Just . BL.splitAt n)

-- Find the smallest multiple of the alu that aligns with the line length.
-- Then print that repeatedly. Finally, print the remainder.
printRepeatedFasta :: Int -> Int -> B.ByteString -> IO ()
printRepeatedFasta !n !l !s = do
  replicateM_ q (B.putStr buffer)
  B.putStr $ B.take (r + r `quot` lineLength) buffer
  B.putStr "\n"
  where
  m = l * gcd l lineLength
  (q,r) = n `quotRem` (lineLength * m)
  buffer = BL.toStrict
    $ BL.unlines
    $ take m
    $ toChunksOf (fromIntegral lineLength)
    $ BL.cycle
    $ BL.fromStrict s

-- Secondly the random fasta

-- fill a vector with pseudorandom values from the LCG.
genSeeds :: Word32 -> M.IOVector Word32 -> Word32 -> IO Word32
genSeeds !bufferSize !v !s = go 0 s where
  go :: Word32 -> Word32 -> IO Word32
  go !n !s
    | n < bufferSize = do
      M.unsafeWrite v (fromIntegral n) sʼ
      go (n + 1) sʼ
    | otherwise = return s
    where
      -- This is the LCG formula
      !sʼ = (ia * s + ic) `rem` im

-- The worker is designed to run in a separate thread. It first generates a
-- block of random values, then it consumes that block. This is done repeatedly
-- until no more characters need to be generated.
worker
  :: Int
  -> V.Vector Word32
  -> V.Vector Word8
  -> MVar (Word32, MVar ())
  -> IORef Int
  -> IO ()
worker !bufferSize !p0 !p1 !mvar !ref = do
  m <- M.new bufferSize
  -- We freeze it unsafely here, but we expect it to be changed in the loop.
  v <- V.unsafeFreeze m
  s <- S.new (bufferSize + bufferSize `quot` lineLength)
  let
    loop = do
      -- Find out how many characters still need to be done.
      n <- atomicModifyIORefʼ ref $ \x ->
        if x > 0
          then (max 0 (x - bufferSize), min x bufferSize)
          else (0, 0)
      when (n > 0) $ do
        next <- newEmptyMVar
        (rnd, prev) <- takeMVar mvar
        rndʼ <- genSeeds (fromIntegral n) m rnd
        putMVar mvar (rndʼ, next)
        consume n p0 p1 s v prev next
        loop
  loop

-- Do the cumulative probability sampling and print the resulting string.
-- It turns out that using two separate vectors for the lookup table is faster.
consume
  :: Int
  -> V.Vector Word32
  -> V.Vector Word8
  -> S.IOVector Word8
  -> V.Vector Word32
  -> MVar ()
  -> MVar ()
  -> IO ()
consume !bufferSize !p0 !p1 !s !v !prev !next = do
  loop lineLength 0 0
  takeMVar prev
  S.unsafeWith s $ \ptr ->
    hPutBuf stdout ptr (bufferSize + bufferSize `quot` lineLength)
  putMVar next ()
  where
    loop :: Int -> Int -> Int -> IO ()
    loop 0 !si !vi = S.unsafeWrite s si 10 *> loop lineLength (si + 1) vi
    loop !l !si !vi
      | vi < bufferSize = do
        S.unsafeWrite s si (lookupCumul (V.unsafeIndex v (fromIntegral vi)))
        loop (l - 1) (si + 1) (vi + 1)
      | otherwise = return ()

    lookupCumul !c = V.unsafeIndex p1 (fromJust (V.findIndex (c <=) p0))

-- Convert a list of characters with probabilities to a cumulative lookup
-- table.
cumulativeProbabilities :: [(Char, Float)] -> V.Vector (Word32, Word8)
cumulativeProbabilities = V.unfoldr uncons . (0,) where
  uncons (_   , []) = Nothing
  uncons (prev, ((c,p):xs)) = let pʼ = p + prev in Just
    ((floor (fromIntegral im * pʼ), fromIntegral (ord c)), (pʼ, xs))

printRandomFasta
  :: V.Vector Word32
  -> V.Vector Word8
  -> Int
  -> Word32
  -> IO Word32
printRandomFasta !p0 !p1 !n !s = do
  ref <- newIORef n
  prev <- newMVar ()
  var <- newMVar (s, prev)
  replicateM_ workers (forkChild (worker bufferSize p0 p1 var ref))
  waitForChildren
  when (n `rem` lineLength /= 0) (putStrLn "")
  fst <$> takeMVar var
  where
    bufferSize = (max (n `quot` (100 * lineLength)) 100) * lineLength

main :: IO ()
main = do
  n <- readIO . head =<< getArgs

  let
    p0 = cumulativeProbabilities
      [(ʼaʼ, 0.27), (ʼcʼ, 0.12), (ʼgʼ, 0.12), (ʼtʼ, 0.27), (ʼBʼ, 0.02),
       (ʼDʼ, 0.02), (ʼHʼ, 0.02), (ʼKʼ, 0.02), (ʼMʼ, 0.02), (ʼNʼ, 0.02),
       (ʼRʼ, 0.02), (ʼSʼ, 0.02), (ʼVʼ, 0.02), (ʼWʼ, 0.02), (ʼYʼ, 0.02)]
    p00 = V.map fst p0
    p01 = V.map snd p0

  let
    p1 = cumulativeProbabilities
      [(ʼaʼ, 0.3029549426680), (ʼcʼ, 0.1979883004921),
       (ʼgʼ, 0.1975473066391), (ʼtʼ, 0.3015094502008)]
    p10 = V.map fst p1
    p11 = V.map snd p1

  putStrLn ">ONE Homo sapiens alu"
  printRepeatedFasta (2 * n) (B.length alu) alu

  putStrLn ">TWO IUB ambiguity codes"
  sʼ <- printRandomFasta p00 p01 (3 * n) 42

  putStrLn ">THREE Homo sapiens frequency"
  printRandomFasta p10 p11 (5 * n) sʼ

  return ()

