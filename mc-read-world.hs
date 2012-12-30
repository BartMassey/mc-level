-- Copyright © 2012 Bart Massey
-- Experimental Minecraft world reader

import Data.NBT.XML
import Game.Minecraft.Level
import System.Environment

main :: IO ()
main = do
  [dir] <- getArgs
  level <- readLevel dir
  putStrLn $ ppElement $ levelToXml level
