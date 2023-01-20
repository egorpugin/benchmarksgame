{-
The Computer Language Benchmarks Game
https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

contributed by Louis Wasserman
GHC 9.0, 9.2 fix by Artem Pelenitsyn
-}

import Control.Monad
import Foreign
import Data.ByteString.Internal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)

data Buf = Buf !Int !Int !(Ptr Word8)

withBuf run = run . Buf 0 ini =<< mallocBytes ini
  where ini = 1024

newSize len sz
  | len <= sz  = sz
  | otherwise  = newSize len (2 * sz)

{-# INLINE putBuf #-}
putBuf pS lS (Buf lD szD pD) run
  | lDʼ > szD  = do
    let szDʼ = newSize lDʼ szD
    pDʼ <- reallocBytes pD szDʼ
    copyArray (pDʼ +* lD) pS lS
    run (Buf lDʼ szDʼ pDʼ)
  | otherwise  = do
    copyArray (pD +* lD) pS lS
    run (Buf lDʼ szD pD)
  where lDʼ = lD + lS

findChar p n c zero one = do
    q <- memchr p c (fromIntegral (n :: Int))
    if q == nullPtr then zero else one $! q `minusPtr` p

clearBuf (Buf _ lB pB) = Buf 0 lB pB

main = allocaArray 82 $ \ line ->
  let go !buf = do
        !m <- hGetBuf stdin line 82
        if m == 0 then revcomp buf else do
          findChar line m (c2w ʼ>ʼ)
            (putBuf line m buf go)
            (\ end -> do
              putBuf line end buf revcomp
              putBuf (line +* end) (m - end) (clearBuf buf)
                go)
    in withBuf go

(+*) = advancePtr

{-# INLINE comps #-}
comps = Prelude.zipWith (\ a b -> (fromEnum a, c2w b)) "AaCcGgTtUuMmRrYyKkVvHhDd
Bb"
  "TTGGCCAAAAKKYYRRMMBBDDHHVV"

ca :: Ptr Word8
ca = unsafeDupablePerformIO $ do
       !a <- mallocArray 200
       mapM_ (\ i -> pokeByteOff a (fromIntegral i) i ) [0..199::Word8]
       mapM_ (uncurry (pokeByteOff a)) comps
       return a

revcomp (Buf lBuf _ pBuf) = when (lBuf > 0) $ ca `seq`
  findChar pBuf lBuf (c2w ʼ\nʼ) undefined $ \ begin -> let
    beginʼ = begin + 1
    rc :: Ptr Word8 -> Ptr Word8 -> IO ()
    rc !i !j | i < j = do
      x <- peek i
      if x == c2w ʼ\nʼ then let !iʼ = i +* 1 in rc1 j iʼ =<< peek iʼ
        else rc1 j i x
    rc i j = when (i == j) (poke i =<< comp =<< peek i)

    rc1 !j !i !xi = do
      y <- peek j
      if y == c2w ʼ\nʼ then let !jʼ = j +* (-1) in rc2 i xi jʼ =<< peek jʼ
        else rc2 i xi j y

    comp = peekElemOff ca . fromIntegral

    rc2 !i !xi !j !xj = do
      poke j =<< comp xi
      poke i =<< comp xj
      rc (i +* 1) (j +* (-1))
    in do
      hPutBuf stdout pBuf beginʼ
      rc (pBuf +* beginʼ) (pBuf +* (lBuf - 1))
      hPutBuf stdout (pBuf +* beginʼ) (lBuf - begin - 1)

