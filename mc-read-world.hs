-- Copyright © 2012 Bart Massey
-- Experimental Minecraft world reader

import qualified Data.ByteString as BS
import Game.Minecraft.Level
import System.Environment

import NBTXML

main :: IO ()
main = do
  [dir] <- getArgs
  levelNbt <- fileToNbt $ dir ++ "/level.dat"
  let levelXml = nbtToXml levelNbt
  putStrLn $ ppElement levelXml
  regionFile <- BS.readFile $ dir ++ "/region/r.0.0.mca"
  let (ci : _) = decodeRegionIndex (0, 0) regionFile
  let cd = getChunk ci regionFile
  let cdXml = nbtToXml $ cdChunk cd
  putStrLn $ ppElement cdXml
