-- Copyright © 2012 Bart Massey
-- Based on Tests.hs in the package nbt on Hackage
module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib.Raw as Deflate
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.NBT
import Data.Serialize
import Data.Word

eitherErr :: Either String a -> a
eitherErr = either error id

parseLevelFile :: String -> IO NBT
parseLevelFile fn = do
  levelFile <- LBS.readFile fn
  let levelData = GZip.decompress levelFile
  return $ eitherErr $ decodeLazy levelData
