module OnyxEdit.Audio (loadStereo16WAV, loadMono16WAV) where

import Sound.OpenAL
import Foreign.Marshal.Alloc
import Data.Bits
import Foreign.Storable
import System.IO
import Data.Word
import Foreign.Ptr
import Control.Monad

-- | Gets the WAV data size and leaves the handle at the beginning of the data.
getWAVSize :: Handle -> IO Int
getWAVSize h = do
  w32 <- mallocBytes 4 :: IO (Ptr Word8)
  hSeek h AbsoluteSeek 40
  4 <- hGetBuf h w32 4
  [a, b, c, d] <- mapM (fmap fromIntegral . peekElemOff w32) [0..3]
  return $ a + shiftL b 8 + shiftL c 16 + shiftL d 24

hLoadMono16WAV :: Handle -> IO Source
hLoadMono16WAV h = do
  chanSize <- getWAVSize h
  mem <- mallocBytes chanSize
  sz <- hGetBuf h mem chanSize
  when (sz /= chanSize) $ error "loadMono16WAV: EOF"
  [buf] <- genObjectNames 1
  bufferData buf $=
    BufferData (MemoryRegion mem $ fromIntegral chanSize) Mono16 44100
  [src] <- genObjectNames 1
  buffer src $= Just buf
  sourcePosition src $= Vertex3 0 0 0 -- probably not necessary
  return src

hLoadStereo16WAV :: Handle -> IO (Source, Source)
hLoadStereo16WAV h = do
  wavSize <- getWAVSize h
  let chanSize = shiftR wavSize 1
      nSamples = shiftR chanSize 1
  memL <- mallocBytes chanSize
  memR <- mallocBytes chanSize

  let go n ml mr = unless (n == 0) $ do
        2 <- hGetBuf h ml 2
        2 <- hGetBuf h mr 2
        go (n - 1) (plusPtr ml 2) (plusPtr mr 2)
  go nSamples memL memR

  [bufL, bufR] <- genObjectNames 2
  bufferData bufL $=
    BufferData (MemoryRegion memL $ fromIntegral chanSize) Mono16 44100
  bufferData bufR $=
    BufferData (MemoryRegion memR $ fromIntegral chanSize) Mono16 44100
  [srcL, srcR] <- genObjectNames 2
  buffer srcL $= Just bufL
  buffer srcR $= Just bufR
  sourcePosition srcL $= Vertex3 (-1) 0 0
  sourcePosition srcR $= Vertex3 1    0 0
  return (srcL, srcR)

loadStereo16WAV :: FilePath -> IO (Source, Source)
loadStereo16WAV fp = withBinaryFile fp ReadMode hLoadStereo16WAV

loadMono16WAV :: FilePath -> IO Source
loadMono16WAV fp = withBinaryFile fp ReadMode hLoadMono16WAV

{-
main :: IO ()
main = do
  Just dev <- openDevice Nothing
  Just ctxt <- createContext dev []
  currentContext $= Just ctxt
  (srcL, srcR) <- withBinaryFile "drums2.wav" ReadMode loadStereo16WAV
  play [srcL, srcR]
  let waitPlay = do
        st <- get $ sourceState srcL
        case st of
          Playing -> waitPlay
          _       -> print st
  waitPlay
  _ <- closeDevice dev
  return ()
-}
