--
-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- Contributed by Sterling Clover
-- Heavily inspired by contribution from Don Stewart
-- Inlining improvements by Don Stewart.
-- GHC 7.8.1 fix by Ersin Er
-- GHC 9.0.1, 9.2.1 fixes by Artem Pelenitsyn
--

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Foreign
import Control.Arrow
import GHC.Base
import GHC.Ptr
import GHC.IO

main = uncurry proc =<< clines `fmap` S.getContents

proc [] _ = return ()
proc (h:hs) (b:bs) = S.putStrLn h >> revcomp b >> writeFasta b >> proc hs bs

writeFasta t
    | S.null t  = return ()
    | otherwise = S.putStrLn l >> writeFasta r
    where (l,r) = S.splitAt 60 t

clines :: ByteString -> ([ByteString],[ByteString])
clines ps = clinesʼ ps ([],[])
    where
      {-# INLINE clinesʼ #-}
      clinesʼ ps accum@(f,s)
          | otherwise = case S.elemIndex ʼ\nʼ ps of
                          Just n  -> clinesʼʼ (S.drop (n+1) ps) (f++[S.take n ps
],s)
      clinesʼʼ ps accum@(f,s)
          | otherwise = case S.elemIndex ʼ>ʼ ps of
                      Nothing -> (f,s++[S.filter (/=ʼ\nʼ) ps])
                      Just n  -> clinesʼ (S.drop n ps) (f,s++[S.filter (/=ʼ\nʼ)
. S.take n $ ps])

{-# INLINE comps #-}
comps = map (ord *** c2w) [
    (ʼAʼ , ʼTʼ), ( ʼaʼ , ʼTʼ), ( ʼCʼ , ʼGʼ), ( ʼcʼ , ʼGʼ), ( ʼGʼ , ʼCʼ),
    (ʼgʼ , ʼCʼ), ( ʼTʼ , ʼAʼ), ( ʼtʼ , ʼAʼ), ( ʼUʼ , ʼAʼ), ( ʼuʼ , ʼAʼ),
    (ʼMʼ , ʼKʼ), ( ʼmʼ , ʼKʼ), ( ʼRʼ , ʼYʼ), ( ʼrʼ , ʼYʼ), ( ʼYʼ , ʼRʼ),
    (ʼyʼ , ʼRʼ), ( ʼKʼ , ʼMʼ), ( ʼkʼ , ʼMʼ), ( ʼVʼ , ʼBʼ), ( ʼvʼ , ʼBʼ),
    (ʼHʼ , ʼDʼ), ( ʼhʼ , ʼDʼ), ( ʼDʼ , ʼHʼ), ( ʼdʼ , ʼHʼ), ( ʼBʼ , ʼVʼ), ( ʼbʼ ,
 ʼVʼ)]

ca :: Ptr Word8
ca = unsafeDupablePerformIO $ do
       a <- mallocArray 200
       mapM_ (uncurry (pokeByteOff a)) $ zip [0..199::Int] [0..199::Word8]
       mapM_ (uncurry (pokeByteOff a)) comps
       return a

comp :: Word# -> Word#
comp c = rw8 ca (word2Int# c)

revcomp (PS fp s (I# l)) = withForeignPtr fp $ \p -> rc (p `plusPtr` s) 0# (l -#
 1#)
  where
    rc :: Ptr Word8 -> Int# -> Int# -> IO ()
    rc p i j  = rcʼ i j
        where
          rcʼ i j
              | isTrue# (i <# j) = do
                          let x = rw8 p i
                          ww8 p i (comp (rw8 p j))
                          ww8 p j (comp x)
                          rcʼ (i +# 1#) (j -# 1#)
              | isTrue# (i ==# j) = ww8 p i (comp (rw8 p i))
              | otherwise =  return ()

rw8 :: Ptr Word8 -> Int# -> Word#
rw8 (Ptr a) i = case readWord8OffAddr# a i realWorld#  of (# _, x #) ->  word8To
Word# x
{-# INLINE rw8 #-}

ww8 :: Ptr Word8 -> Int# -> Word# -> IO ()
ww8 (Ptr a) i x  = IO $ \s -> case writeWord8OffAddr# a i (wordToWord8# x) s of
s2 -> (# s2, () #)


