-- Copyright © 2012 Bart Massey
-- Based on Tests.hs in the package nbt on Hackage
module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8 ( fromString, toString )
import Data.Int
import Data.NBT
import Data.Serialize
import Data.Word
import System.Environment

eitherErr :: Either String a -> a
eitherErr = either error id

data ChunkIndex = ChunkIndex {
  ciRelX, ciRelZ :: Word,
  ciFileOffset :: Word32,
  ciApproxLength :: Word32,
  ciTimeStamp :: Word32
} deriving Show

parseRegionHeader :: BS.ByteString -> [ChunkIndex]
parseRegionHeader regionFile =
  eitherErr $ runGet parseHeader regionFile
  where
    parseHeader = do
      locBlocks <- replicateM 1024 getWord32be
      timeStampBlocks <- replicateM 1024 getWord32be
      let raw = zip [0..] $ zip locBlocks timeStampBlocks
      let filtered = filter ((/= 0) . fst . snd) raw
      return $ map makeChunkIndex filtered
      where
        makeChunkIndex (seq, (block, ts)) =
          ChunkIndex {
            ciRelX = seq `div` 32,
            ciRelZ = seq `mod` 32,
            ciFileOffset = 4096 * (block `shiftR` 8),
            ciApproxLength = 4096 * (block .&. 0xff),
            ciTimeStamp = ts }

data ChunkData = ChunkData {
  cdLength :: Word32,
  cdCompressionType :: Word8,
  cdChunk :: NBT } deriving Show

getChunk :: Word32 -> BS.ByteString -> ChunkData
getChunk offset regionFile =
  eitherErr $ runGet parseChunk regionFile
  where
    parseChunk = do
      skip (fromIntegral offset)
      chunkLength <- getWord32be
      compression <- getWord8
      unless (compression == 2) 
        (error $ "unexpected compression format " ++ show compression)
      compressedChunk <- getBytes (fromIntegral chunkLength)
      let chunk = Zlib.decompress $ LBS.pack $ BS.unpack compressedChunk
      let nbt = eitherErr $ decodeLazy chunk
      return $ ChunkData {
        cdLength = chunkLength,
        cdCompressionType = compression,
        cdChunk = nbt }

main :: IO ()
main = do
  [fn] <- getArgs
  regionFile <- BS.readFile fn
  let chunkIndexes = parseRegionHeader regionFile
  print $ getChunk (ciFileOffset $ head chunkIndexes) regionFile
