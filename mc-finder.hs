-- Copyright © 2013 Bart Massey
-- Minecraft world item finder

import Find
import Game.Minecraft.Level
import System.Console.ParseArgs

data ArgIndex = ArgIndexExpr | ArgIndexPath deriving (Eq, Ord, Show)

argd :: [Arg ArgIndex]
argd = [
  Arg {
    argIndex = ArgIndexExpr,
    argAbbr = Nothing,
    argName = Nothing,
    argData = argDataRequired "find-expr" ArgtypeString,
    argDesc = "Expression describing object to be found." },
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
  let tree = makeTree $ getRequiredArg argv ArgIndexExpr
  level <- readLevel dir True
  case tree of
    FindItem itemId _ -> print itemId
  return ()
