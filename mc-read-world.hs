-- Copyright © 2012 Bart Massey
-- Experimental Minecraft world reader

import qualified Data.ByteString as BS
import Game.Minecraft.Level
import System.Environment

main :: IO ()
main = do
  [dir] <- getArgs
  levelNbt <- fileToNbt $ dir ++ "/level.dat"
  print levelNbt
  regionFile <- BS.readFile $ dir ++ "/region/r.0.0.mca"
  let (ci : _) = decodeRegionIndex (0, 0) regionFile
  print $ getChunk ci regionFile
