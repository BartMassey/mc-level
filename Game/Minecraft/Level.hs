-- Copyright © 2012 Bart Massey
-- This work is licensed under the "3-clause ('new') BSD License".
-- Please see the file COPYING in this distribution for license terms.

-- | This code provides an API for Minecraft
-- <http://minecraft.net> on-disk world (aka "level") data
-- reading and writing.
-- 
-- The intent is to take or return \"decorated\" 'NBT' with
-- minimal processing, mostly merely writing or locating and
-- reading the on-disk world data that makes up a Minecraft
-- world.
-- 
-- A Minecraft world comprises overview data, player data,
-- and \"map\" data for the various regions of the world
-- (notably the surface, the Nether, and the End). The
-- on-disk data formats for all of this are pretty
-- thoroughly documented on the Minecraft Wiki
-- <http://minecraftwiki.net>.
-- 
-- This package currently supports only the \"Anvil\" format
-- as used by Minecraft 1.4.4. (Backporting to earlier
-- formats would be straightforward if there's interest.)

module Game.Minecraft.Level
where

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

-- | Given the name of a gzip-compressed \"level.dat\" file,
-- return that file as 'NBT'.
levelFileToNbt :: String -> IO NBT
levelFileToNbt fn = do
  levelFile <- LBS.readFile fn
  let levelData = GZip.decompress levelFile
  return $ eitherErr $ decodeLazy levelData

-- | Given the name of a gzip-compressed \"level.dat\" file,
-- output the given 'NBT' as the contents of that file.
nbtToLevelFile :: String -> NBT -> IO ()
nbtToLevelFile fn nbt = do
  let levelFile = encodeLazy nbt
  let levelData = GZip.compress levelFile
  LBS.writeFile fn levelData
