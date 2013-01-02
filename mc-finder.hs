-- Copyright © 2013 Bart Massey
-- Minecraft world item finder

import Game.Minecraft.Level
import System.Console.ParseArgs
import Text.ParserCombinators.Parsec

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

type Rel = Int -> Bool

data ItemQualAttr = ItemQualLevel Rel

data ItemQual = 
  ItemQualEnch {
    itemQualEnchId :: Int, 
    itemQualEnchAttrs :: [ItemQualAttr] }

data Find = FindItem { 
  findItemId :: Maybe Int,
  findItemQuals :: [ItemQual] }

parseNumber :: Parser Int
parseNumber = do 
  ds <- many1 digit
  return $ read ds

parseId :: Parser Int
parseId = do
  _ <- char '='
  parseNumber

type BRel = Int -> Int -> Bool

parseRelLT :: Parser BRel
parseRelLT = do
  _ <- string "<"
  return (<)

parseRelLE :: Parser BRel
parseRelLE = do
  _ <- string "<="
  return (<=)

parseRelEQ :: Parser BRel
parseRelEQ = do
  _ <- string "="
  return (==)

parseRelGE :: Parser BRel
parseRelGE = do
  _ <- string ">="
  return (>=)

parseRelGT :: Parser BRel
parseRelGT = do
  _ <- string ">"
  return (>)

parseRel :: Parser Rel
parseRel = do
  rel <- parseRelLT <|> parseRelLE <|> parseRelEQ <|> parseRelGE <|> parseRelGT
  val <- parseNumber
  return $ \x -> x `rel` val

parseItemLevel :: Parser ItemQualAttr
parseItemLevel = do
  _ <- string "level"
  rel <- parseRel
  return $ ItemQualLevel rel

parseEnchQuals :: Parser [ItemQualAttr]
parseEnchQuals = do
  _ <- char '['
  enchQuals <- sepBy1 parseItemLevel (char ',')
  _ <- char ']'
  return enchQuals

parseItemEnch :: Parser ItemQual
parseItemEnch = do
  _ <- string "ench"
  val <- parseId
  quals <- option [] parseEnchQuals
  return $ ItemQualEnch {
    itemQualEnchId = val, 
    itemQualEnchAttrs = quals }

parseItemQuals :: Parser [ItemQual]
parseItemQuals = do
  _ <- char '['
  itemQuals <- sepBy1 parseItemEnch (char ',')
  _ <- char ']'
  return itemQuals

parseItem :: Parser Find
parseItem = do
  _ <- string "item"
  maybeItemId <- optionMaybe parseId
  quals <- option [] parseItemQuals
  return $ FindItem { findItemId = maybeItemId, findItemQuals = quals }

parseExpr :: Parser Find
parseExpr = do
  find <- parseItem
  eof
  return find

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let dir = getRequiredArg argv ArgIndexPath
  let tree =
        case parse parseExpr "find-expr" $ getRequiredArg argv ArgIndexExpr of
          Left msg -> error $ show msg
          Right t -> t
  level <- readLevel dir True
  case tree of
    FindItem itemId _ -> print itemId
  return ()
