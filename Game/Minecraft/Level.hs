-- Copyright © 2012 Bart Massey
-- This work is licensed under the "3-clause ('new') BSD License".
-- Please see the file COPYING in this distribution for license terms.

-- | This code provides an API for Minecraft
-- <http://minecraft.net> on-disk world (aka "level") data
-- reading and writing. It was inspired by @acfoltzer@'s
-- @minecraft-data@ code
-- <http://github.com/acfoltzer/minecraft-data>.
-- 
-- The intent is to take or return \"decorated\" 'NBT' with
-- minimal processing, mostly merely writing or locating and
-- reading the on-disk world data that makes up a Minecraft
-- world.
-- 
-- A Minecraft world comprises overview data, player data,
-- and \"map\" data for the various dimensions of the world
-- (notably the Surface, the Nether, and the End). The
-- on-disk data formats for all of this are pretty
-- thoroughly documented on the Minecraft Wiki
-- <http://minecraftwiki.net>.
-- 
-- This package currently supports only the \"Anvil\" format
-- as used by Minecraft 1.4.4. (Backporting to earlier
-- formats would be straightforward if there's interest.)

module Game.Minecraft.Level (
  Level(..),
  Dims(..),
  Region(..),
  readLevel,
  readPlayerData,
  readDims,
  readDim,
  fileToNbt,
  nbtToFile,
  ChunkIndex(..), 
  ChunkData(..),
  decodeRegionIndex,
  encodeRegionIndex,
  getChunk, 
  levelToXml, 
  regionToXml )
where

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.NBT
import Data.NBT.XML
import Data.Serialize
import Data.Word
import System.Directory
import System.FilePath
import Text.Regex

-- | Reification of an entry in the chunk index header of a
-- region file.
data ChunkIndex = ChunkIndex {
  -- | Chunk position in absolute (x, z) chunk coordinates.
  ciPos :: (Int, Int),
  -- | Number of bytes from the start of the region file that
  -- this chunk's data begins. Note that this offset includes
  -- the offset of 8192 bytes for the region file header.
  ciFileOffset :: Word32,
  -- | Number of 4096-byte blocks for the entry, rounded up.
  ciApproxLength :: Word32,
  -- | Number of seconds since the start of 1 January 1970.
  ciTimestamp :: Word32
} deriving Show

-- | Reification of a chunk in a region file.
-- Includes the 'ChunkIndex' that references
-- the chunk.
-- 
-- Currently, the only actively supported compression type is
-- Zlib (RFC1950), type 2. GZip (RFC1952), type 1, is supported
-- in theory, but not in practice. Thus, the compression type
-- is implicit rather than reported.
data ChunkData = ChunkData {
  cdChunkIndex :: ChunkIndex,
  -- | Actual length in bytes of the on-disk chunk data, at
  -- the time the chunk was read from disk. Not terribly
  -- useful in practice.
  cdLength :: Maybe Word32,
  cdChunk :: NBT } deriving Show

eitherErr :: Either String a -> a
eitherErr = either error id

data Region = Region { 
  regionX, regionY :: Int,
  regionContents :: [ChunkData] } deriving Show

data Dims = Dims { 
  dimsSurface, dimsNether, dimsEnd :: Maybe [Region] } deriving Show

data Level = Level { 
  levelDat :: NBT,
  levelPlayers :: [NBT],
  levelDims :: Dims } deriving Show

readPlayerData :: FilePath -> IO [NBT]
readPlayerData pn = do
  entries <- getDirectoryContents pn
  mapM fileToNbt $ map (pn </>) $ filter (".dat" `isSuffixOf`) entries

regionFileNameRE :: Regex
regionFileNameRE =
  mkRegexWithOpts "^r\\.(-?[0-9]+)\\.(-?[0-9]+)\\.mca$" False True

readDim :: FilePath -> Bool -> IO [Region]
readDim pn short = do
  entries <- getDirectoryContents pn
  let regions = mapMaybe checkPath entries
  mapM getRegion regions
  where
    checkPath en = do
      [xstr, ystr] <- matchRegex regionFileNameRE en
      return (en, xstr, ystr)
    getRegion (en, xstr, ystr) = do
      let x = read xstr
      let y = read ystr
      cs <- readRegionFile (x, y) (pn </> en) short
      return $ Region {
        regionX = x,
        regionY = y,
        regionContents = cs }
      

readDims :: FilePath -> [FilePath] -> Bool -> IO Dims
readDims pn entries short = do
  surface <- getDim "region"
  nether <- getDim $ "DIM-1" </> "region"
  end <- getDim $ "DIM1" </> "region"
  return $ Dims {
    dimsSurface = surface,
    dimsNether = nether,
    dimsEnd = end }
  where
    getDim rn =
      if rn `elem` entries
        then do
          rs <- readDim (pn </> rn) short
          if null rs
            then return Nothing
            else return $ Just rs
        else return Nothing

readLevel :: FilePath -> Bool -> IO Level
readLevel pn short = do
  entries <- getDirectoryContents pn
  unless ("level.dat" `elem` entries) 
    (error "readLevel: path does not contain a Minecraft level")
  dat <- fileToNbt $ pn </> "level.dat"
  players <- 
    if "players" `elem` entries
      then readPlayerData $ (pn </> "players")
      else return []
  dims <- readDims pn entries short
  return $ 
    Level {
      levelDat = dat,
      levelPlayers = players,
      levelDims = dims }

-- | Given the name of a gzip-compressed 'NBT' file,
-- return that file as 'NBT'.
fileToNbt :: FilePath -> IO NBT
fileToNbt fn = do
  nbtFile <- LBS.readFile fn
  let nbtData = GZip.decompress nbtFile
  return $ eitherErr $ decodeLazy nbtData

-- | Given the name of a gzip-compressed 'NBT' file,
-- output the given 'NBT' as the contents of that file.
nbtToFile :: FilePath -> NBT -> IO ()
nbtToFile fn nbt = do
  let nbtFile = encodeLazy nbt
  let nbtData = GZip.compress nbtFile
  LBS.writeFile fn nbtData

uncoord :: (Int, Int) -> Int -> (Int, Int)
uncoord (x, z) seqnum =
    let x' = x + (seqnum `mod` 32) in
    let z' = z + (seqnum `div` 32) in
    (x', z')

-- | Given a region file represented as a strict
-- 'ByteString' and the offset of the start of the region in
-- chunk coordinates @(x, z)@, return a list of the valid
-- chunk index entries of the region file.
decodeRegionIndex :: (Int, Int) -> BS.ByteString -> [ChunkIndex]
decodeRegionIndex regionPos regionFile =
  eitherErr $ runGet parseHeader regionFile
  where
    parseHeader = do
      locBlocks <- replicateM 1024 getWord32be
      timeStampBlocks <- replicateM 1024 getWord32be
      let raw = zip [0..] $ zip locBlocks timeStampBlocks
      let filtered = filter ((/= 0) . fst . snd) raw
      return $ map makeChunkIndex filtered
      where
        makeChunkIndex (seqnum, (block, ts)) =
          ChunkIndex {
            ciPos = uncoord regionPos seqnum,
            ciFileOffset = 4096 * (block `shiftR` 8),
            ciApproxLength = 4096 * (block .&. 0xff),
            ciTimestamp = ts }

-- | Given a chunk offset for a region in chunk coords,
-- and a list of the valid chunk index entries of a
-- region file, return a strict 'ByteString' representing
-- the header of that file.
encodeRegionIndex :: (Int, Int) -> [ChunkIndex] -> BS.ByteString
encodeRegionIndex regionPos chunkIndexes  =
  runPut createHeader
  where
    ciMap = 
      M.fromList $ map mkEntry chunkIndexes
      where
        mkEntry ci = (ciPos ci, ci)
    createHeader = do
      let ps = map (uncoord regionPos) [0..1023]
      mapM_ putIndex ps
      mapM_ putTimestamp ps
      where
        putIndex c | M.member c ciMap = do
          let Just ci = M.lookup c ciMap
          let off = ciFileOffset ci `div` 4096
          let off' = off `shiftL` 8
          let siz = (4095 + ciApproxLength ci) `div` 4096
          let siz' = siz .&. 0xff
          putWord32be (off' .|. siz')
        putIndex _ =
          putWord32be 0
        putTimestamp c | M.member c ciMap = do
          let Just ci = M.lookup c ciMap
          putWord32be (ciTimestamp ci)
        putTimestamp _ =
          putWord32be 0

getChunk :: BS.ByteString -> Bool -> ChunkIndex -> ChunkData
getChunk regionFile short ci =
  eitherErr $ runGet parseChunk regionFile
  where
    parseChunk = do
      skip (fromIntegral (ciFileOffset ci))
      chunkLength <- getWord32be
      compression <- getWord8
      unless (compression == 2) 
        (error $ "unexpected compression format " ++ show compression)
      compressedChunk <- getBytes (fromIntegral chunkLength)
      let chunk = Zlib.decompress $ LBS.pack $ BS.unpack compressedChunk
      let nbt = eitherErr $ decodeLazy chunk
      return $ ChunkData {
        cdChunkIndex = ci,
        cdLength = Just chunkLength,
        cdChunk = if short then stripChunk nbt else nbt }

stripChunk :: NBT -> NBT
stripChunk (CompoundTag (Just "") [CompoundTag (Just "Level") values]) =
  let values' = filter notSection values in
  CompoundTag (Just "") [CompoundTag (Just "Level") values']
  where
    notSection (ListTag (Just "Sections") _ _ _) = False
    notSection _ = True
stripChunk _ = error "bad chunk format"

readRegionFile :: (Int, Int) -> FilePath -> Bool -> IO [ChunkData]
readRegionFile (x, y) pn short = do
  regionFile <- BS.readFile pn
  let cs = decodeRegionIndex (x, y) regionFile
  return $ map (getChunk regionFile short) cs

levelToXml :: Level -> Element
levelToXml l =
  let level = mkContent "level-data" [] $ [Elem (nbtToXml (levelDat l))]
      players = mkContent "players" [] $ 
                map mkPlayer $ 
                levelPlayers l
        where
          mkPlayer p = mkContent "player" [] [ Elem  $ nbtToXml p ]
      dims = catMaybes [
        mkDim "surface" dimsSurface,
        mkDim "nether" dimsNether,
        mkDim "end" dimsEnd ]
        where
          mkDim name sel =
            fmap m $ sel $ levelDims l
            where
              m d = mkContent "dimension" [("name", name)] (map regionToXml d)
  in
  mkElement "minecraft-level" [] $  [level, players] ++ dims


regionToXml :: Region -> Content
regionToXml r =
  let attrs = [("chunk-x", show (regionX r)), ("chunk-y", show (regionY r))] in
  mkContent "region" attrs $ map chunkDataToXml $ regionContents r

chunkDataToXml :: ChunkData -> Content
chunkDataToXml d =
  let attrs = [("index", show (cdChunkIndex d))] in
  mkContent "chunk" attrs [ Elem $ nbtToXml $ cdChunk d ]

mkElement :: String -> [(String, String)] -> [Content] -> Element
mkElement name attrs content =
  let attrs' = map (\(n, v) -> Attr (unqual n) v) attrs in
  Element (unqual name) attrs' content Nothing

mkContent :: String -> [(String, String)] -> [Content] -> Content
mkContent name attrs content =
  Elem $ mkElement name attrs content
