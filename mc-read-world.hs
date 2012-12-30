-- Copyright © 2012 Bart Massey
-- Experimental Minecraft world reader

import Data.NBT.XML
import Game.Minecraft.Level
import System.Console.ParseArgs

data ArgIndex = ArgIndexPath | ArgIndexShort deriving (Eq, Ord, Show)

argd :: [Arg ArgIndex]
argd = [
  Arg {
    argIndex = ArgIndexShort,
    argAbbr = Just 's',
    argName = Just "short",
    argData = Nothing,
    argDesc = "Omit section data from chunk output to save processing." },
  Arg {
    argIndex = ArgIndexPath,
    argAbbr = Nothing,
    argName = Nothing,
    argData = argDataRequired "world-path" ArgtypeString,
    argDesc = "Path to root of world data." } ]

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let dir = getRequiredArg argv ArgIndexPath
  let short = gotArg argv ArgIndexShort
  level <- readLevel dir short
  putStrLn $ ppElement $ levelToXml level
