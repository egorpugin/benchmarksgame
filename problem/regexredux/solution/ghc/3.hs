-- The Computer Language Benchmarks Game
-- https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
--
-- contributed by Jaro Reinders
--
-- adapted from the C implementation that was:
--   contributed by Jeremy Zerfas
--   modified by Zoltan Herczeg

import Foreign.C.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.Word
import Control.Monad
import Foreign.Storable
import System.IO
import Control.Concurrent
import Data.Foldable
import Data.Char

-- first some foreign imports (Haskell does not have a proper pcre2 binding!)

foreign import capi "pcre2.h value PCRE2_JIT_COMPLETE"
  c_PCRE2_JIT_COMPLETE :: CUInt

foreign import ccall "pcre2.h pcre2_get_ovector_pointer_8"
  c_pcre2_get_ovector_pointer :: Ptr MatchData -> IO (Ptr CSize)

foreign import ccall "pcre2.h pcre2_compile_8"
  c_pcre2_compile :: Ptr CChar -> CSize -> CInt -> Ptr CInt -> Ptr CSize
    -> Ptr () -> IO (Ptr Code)

foreign import ccall "pcre2.h pcre2_jit_compile_8"
  c_pcre2_jit_compile :: Ptr Code -> CUInt -> IO ()

-- This one is marked unsafe for extra performance.
-- See https://wiki.haskell.org/Performance/FFI
foreign import ccall unsafe "pcre2.h pcre2_jit_match_8"
  c_pcre2_jit_match :: Ptr Code -> Ptr CChar -> CSize -> CSize -> CUInt
    -> Ptr MatchData -> Ptr MatchContext -> IO CInt

foreign import ccall "pcre2.h pcre2_code_free_8"
  c_pcre2_code_free :: Ptr Code -> IO ()

foreign import ccall "pcre2.h pcre2_match_context_create_8"
  c_pcre2_match_context_create :: Ptr GeneralContext -> IO (Ptr MatchContext)

foreign import ccall "pcre2.h pcre2_jit_stack_create_8"
  c_pcre2_jit_stack_create :: CSize -> CSize -> Ptr () -> IO (Ptr JitStack)

foreign import ccall "pcre2.h pcre2_jit_stack_assign_8"
  c_pcre2_jit_stack_assign :: Ptr MatchContext -> Ptr () -> Ptr JitStack
    -> IO ()

foreign import ccall "pcre2.h pcre2_match_data_create_8"
  c_pcre2_match_data_create :: CUInt -> Ptr GeneralContext
    -> IO (Ptr MatchData)

foreign import ccall "pcre2.h pcre2_match_context_free_8"
  c_pcre2_match_context_free :: Ptr MatchContext -> IO ()

foreign import ccall "pcre2.h pcre2_jit_stack_free_8"
  c_pcre2_jit_stack_free :: Ptr JitStack -> IO ()

foreign import ccall "pcre2.h pcre2_match_data_free_8"
  c_pcre2_match_data_free :: Ptr MatchData -> IO ()

data MatchData
data MatchContext
data Code
data GeneralContext
data JitStack

data GrowString = GrowString
  {-# UNPACK #-} !(M.IOVector Word8)
  {-# UNPACK #-} !Int

-- Freeze and trim
freezeGrowString :: GrowString -> IO (V.Vector Word8)
freezeGrowString (GrowString dat siz) = V.unsafeFreeze (M.slice 0 siz dat)

-- Function for searching a srcString for a pattern, replacing it with some
-- specified replacement, and storing the result in dstString.
--
-- dstString might be reallocated so this function returns a new GrowString.
-- For optimal performance you should not use the old GrowString anymore.
replace :: V.Vector Word8 -> V.Vector Word8 -> V.Vector Word8
  -> GrowString -> Ptr MatchContext -> Ptr MatchData -> IO GrowString
replace !pattern !replacement !srcString !dstString !mcontext !mdata =
  alloca $ \errorCode -> alloca $ \errorOffset -> do
    match <- c_pcre2_get_ovector_pointer mdata

    -- Compile and study pattern.
    regex <- V.unsafeWith pattern $ \patternPtr -> c_pcre2_compile
      (castPtr patternPtr) (fromIntegral (V.length pattern)) 0 errorCode
      errorOffset nullPtr
    c_pcre2_jit_compile regex c_PCRE2_JIT_COMPLETE

    -- Find each match of the pattern in srcString and append the characters
    -- preceding each match and the replacement text to dstString.
    let
      go !pos !dstString = do
        !x <- V.unsafeWith srcString $ \srcStringPtr ->
          c_pcre2_jit_match regex (castPtr srcStringPtr)
            (fromIntegral srcStringLen) (fromIntegral pos) 0 mdata mcontext
        if (x >= 0) then do
          !match0 <- fromIntegral <$> peekElemOff match 0

          -- Allocate more memory for dstString if there is not enough space
          -- for the characters preceding the match and the replacement text.
          let
            growLoop str@(GrowString !dat !siz)
              | siz + match0 - pos + replacementSize > M.length dat = do
                !datʼ <- M.grow dat (M.length dat) :: IO (M.IOVector Word8)
                growLoop (GrowString datʼ siz)
              | otherwise = return str
          (GrowString dat siz) <- growLoop dstString

          -- Append the characters preceding the match and the replacement text
          -- to dstString and update the size of dstString.
          let
            !sizʼ = siz + match0 - pos
            !sizʼʼ = sizʼ + replacementSize
          V.copy (M.slice siz  (match0 - pos)  dat)
            (V.slice pos (match0 - pos) srcString)
          V.copy (M.slice sizʼ replacementSize dat) replacement

          -- Find the new pos to continue after the current match.
          !posʼ <- fromIntegral <$> peekElemOff match 1

          go posʼ (GrowString dat sizʼʼ)
        else return (pos, dstString)
    (!pos, !dstStringʼ) <- go 0 dstString

    c_pcre2_code_free regex

    -- Allocate more memory for dstString if there is not enough space for the
    -- characters following the last match (or the entire srcString if there
    -- was no match).
    let
      growLoop str@(GrowString !dat !siz)
        | siz + srcStringLen - pos > M.length dat = do
          datʼ <- M.grow dat (M.length dat) :: IO (M.IOVector Word8)
          growLoop (GrowString datʼ siz)
        | otherwise = return str
    (GrowString dat siz) <- growLoop dstStringʼ

    -- Append the characters following the last match (or the entire srcString
    -- if there was no match) to dstString and update the size of dstString.
    V.copy (M.slice siz (srcStringLen - pos) dat)
      (V.slice pos (srcStringLen - pos) srcString)

    return (GrowString dat (siz + srcStringLen - pos))
  where
    srcStringLen = V.length srcString
    replacementSize = V.length replacement

main :: IO ()
main = do
  let
    f = V.fromList . map (fromIntegral . ord)
    countInfo = map f
      [ "agggtaaa|tttaccct"
      , "[cgt]gggtaaa|tttaccc[acg]"
      , "a[act]ggtaaa|tttacc[agt]t"
      , "ag[act]gtaaa|tttac[agt]ct"
      , "agg[act]taaa|ttta[agt]cct"
      , "aggg[acg]aaa|ttt[cgt]ccct"
      , "agggt[cgt]aa|tt[acg]accct"
      , "agggta[cgt]a|t[acg]taccct"
      , "agggtaa[cgt]|[acg]ttaccct"
      ]
    replaceInfo = map (\(a,b) -> (f a, f b))
      [ ("tHa[Nt]", "<4>")
      , ("aND|caN|Ha[DS]|WaS", "<3>")
      , ("a[NSt]|BY", "<2>")
      , ("<[^>]*>", "|")
      , ("\\|[^|][^|]*\\|", "-")
      ]

  input <- (\x -> GrowString x 0) <$> M.new 16384
  sequences <- (\x -> GrowString x 0) <$> M.new 16384

  -- Read in input from stdin until we reach the end or encounter an error.
  let
    readLoop (GrowString !dat !siz) = do
      bytesRead <- M.unsafeWith (M.slice siz (M.length dat - siz) dat)
        $ \inputPtr -> hGetBuf stdin inputPtr (M.length dat - siz)
      if bytesRead > 0 then do
        -- update the size of input to reflect the newly read input and if
        -- weʼve reached the full capacity of the input string then also double
        -- its size.
        datʼ <- if (siz + bytesRead == M.length dat)
          then M.grow dat (M.length dat) :: IO (M.IOVector Word8)
          else return dat
        readLoop (GrowString datʼ (siz + bytesRead))
      else return (GrowString dat siz)
  inputʼ <- freezeGrowString =<< readLoop input
  let !inputSiz = V.length inputʼ

  let
    threadInit = do
      mcontext <- c_pcre2_match_context_create nullPtr
      stack <- c_pcre2_jit_stack_create 16384 16384 nullPtr
      c_pcre2_jit_stack_assign mcontext nullPtr stack
      mdata <- c_pcre2_match_data_create 16 nullPtr
      return (mcontext, stack, mdata)
  (mcontext, stack, mdata) <- threadInit

  -- Find all sequence descriptions and new lines in input, replace them with
  -- empty strings, and store the result in the sequences string.
  sequencesʼ@(GrowString seqDat seqSiz) <-
    replace (f ">.*\\n|\\n") (f "") inputʼ sequences mcontext mdata

  -- Work on performing all the replacements serially.
  replaceVar <- newEmptyMVar
  -- Fork this thread explicitely to capability 0 to discourage the scheduler
  -- from interrupting this thread.
  forkOn 0 $ do
    -- Weʼll use two strings when doing all the replacements, searching for
    -- patterns in prereplaceString and using postreplaceString to store the
    -- string after the replacements have been made. After each iteration these
    -- two then get swapped. Start out with both strings having the same
    -- capacity as the sequences string and also copy the sequences string into
    -- prereplaceString for the initial iteration.
    prereplaceString <- (\x -> GrowString x seqSiz) <$> M.clone seqDat
    postreplaceString <- (\x -> GrowString x 0) <$> M.new (M.length seqDat)

    -- Iterate through all the replacement patterns and their replacements in
    -- replaceInfo
    let
      cons (pre@(GrowString dat _),post) (a,b) = do
        datʼ <- freezeGrowString pre
        postʼ <- replace a b datʼ post mcontext mdata
        let preʼ = (GrowString dat 0)
        -- Swap pre and post in the next iteration.
        return (postʼ, preʼ)
    -- If any replacements were made, theyʼll be in the fst element of the
    -- tuple instead of the second because of the swap done at the end of each
    -- iteration.
    (GrowString _ !siz, _) <- foldlM cons
      (prereplaceString, postreplaceString) replaceInfo

    c_pcre2_match_context_free mcontext
    c_pcre2_jit_stack_free stack
    c_pcre2_match_data_free mdata

    putMVar replaceVar siz

  -- Iterate through all the count patterns in countInfo and perform the
  -- counting for each one on a different thread if available.
  first <- newMVar ()
  rest <- replicateM (length countInfo) newEmptyMVar

  for_ (zip3 countInfo (first : rest) rest) $ \(pattern, prev, next) ->
    forkIO $ do
      (mcontext, stack, mdata) <- threadInit
      match <- c_pcre2_get_ovector_pointer mdata

      -- Compile and study pattern.
      regex <- alloca $ \errorCode -> alloca $ \errorOffset ->
        V.unsafeWith pattern $ \patternPtr -> c_pcre2_compile
          (castPtr patternPtr) (fromIntegral (V.length pattern)) 0 errorCode
          errorOffset nullPtr
      c_pcre2_jit_compile regex c_PCRE2_JIT_COMPLETE

      -- Find each match of the pattern in the sequences string and increment
      -- count for each match.
      let
        go !count !pos = do
          x <- M.unsafeWith dat $ \datPtr -> c_pcre2_jit_match regex
            (castPtr datPtr) (fromIntegral siz) pos 0 mdata mcontext
          if x >= 0 then do
            -- Find the new pos to continue searching after the current match.
            posʼ <- peekElemOff match 1
            go (count + 1) posʼ
          else return count
          where
            (GrowString dat siz) = sequencesʼ
      count <- go 0 0

      c_pcre2_code_free regex

      -- Print the count for each pattern in the correct order.
      takeMVar prev
      V.unsafeWith pattern $ \patternPtr ->
        hPutBuf stdout patternPtr (V.length pattern)
      putStr " "
      print count
      putMVar next ()

      c_pcre2_match_context_free mcontext
      c_pcre2_jit_stack_free stack
      c_pcre2_match_data_free mdata

  siz <- takeMVar (replaceVar)

  takeMVar (last rest)

  -- Print the size of the original input, the size of the input without the
  -- sequence descriptions & new lines, and the size after having made all the
  -- replacements.
  putStrLn ""
  print inputSiz
  print seqSiz
  print siz

