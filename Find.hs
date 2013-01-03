{-# LANGUAGE FlexibleContexts #-}

module Find (
  Rel, ItemQualAttr(..), ItemQual(..), Find(..),
  makeTree, 
  ItemSource(..), Item(..),
  find )
where

import Data.Maybe
import Data.NBT
import Data.NBT.XML
import Game.Minecraft.Level
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.Printf

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
parseNumber = fmap read (many1 digit)

parseId :: Parser Int
parseId = char '=' >> parseNumber

type BRel = Int -> Int -> Bool

parseRelLT :: Parser BRel
parseRelLT = string "<" >> return (<)

parseRelLE :: Parser BRel
parseRelLE = string "<=" >> return (<=)

parseRelEQ :: Parser BRel
parseRelEQ = string "=" >> return (==)

parseRelGE :: Parser BRel
parseRelGE = string ">=" >> return (>=)

parseRelGT :: Parser BRel
parseRelGT = string ">" >> return (>)

parseRel :: Parser Rel
parseRel = do
  rel <- parseRelLT <|> parseRelLE <|> parseRelEQ <|> parseRelGE <|> parseRelGT
  val <- parseNumber
  return $ (`rel` val)

parseItemLevel :: Parser ItemQualAttr
parseItemLevel = do
  _ <- string "level"
  rel <- parseRel
  return $ ItemQualLevel rel

parseList :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
parseList parseElem = 
  between (char '[') (char ']') (sepBy1 parseElem (char ','))

parseItemEnch :: Parser ItemQual
parseItemEnch = do
  _ <- string "ench"
  val <- parseId
  quals <- option [] $ parseList parseItemLevel
  return $ ItemQualEnch {
    itemQualEnchId = val, 
    itemQualEnchAttrs = quals }

parseItem :: Parser Find
parseItem = do
  _ <- string "item"
  maybeItemId <- optionMaybe parseId
  quals <- option [] (parseList parseItemEnch)
  return $ FindItem { 
    findItemId = maybeItemId, 
    findItemQuals = quals }

parseExpr :: Parser Find
parseExpr = do
  findExpr <- parseItem
  eof
  return findExpr

makeTree :: String -> Find
makeTree expr =
  case parse parseExpr "find-expr" expr of
    Left msg -> error $ show msg
    Right t -> t


data ItemSource = ItemSourcePlayer String
                | ItemSourceTile String
                | ItemSourceFree

instance Show ItemSource where
  show ItemSourceFree = "free"
  show (ItemSourcePlayer name) = "player[" ++ name ++ "]"
  show (ItemSourceTile name) = "tile[" ++ name ++ "]"

data Item = Item {
  itemCoords :: (Int, Int, Int),
  itemSource :: ItemSource,
  itemData :: NBT }

instance Show Item where
  show (Item {itemCoords = (x, y, z), itemSource = source, itemData = nbts }) =
    printf "item=%d[x=%d,y=%d,z=%d,source=%s]" 
      itemId x y z (show source)
    where
      itemId =
        case path [Nothing, Just "id"] nbts of
          Just (ShortTag (Just "id") i) -> i
          _ -> error "item without id"
    

tagName :: NBT -> Maybe String
tagName EndTag = Nothing
tagName (ByteTag name _) = name
tagName (ShortTag name _) = name
tagName (IntTag name _) = name
tagName (LongTag name _) = name
tagName (FloatTag name _) = name
tagName (DoubleTag name _) = name
tagName (ByteArrayTag name _ _) = name
tagName (StringTag name _ _) = name 
tagName (ListTag name _ _ _) = name
tagName (CompoundTag name _) = name
tagName (IntArrayTag name _ _) = name

contents :: Maybe NBT -> Maybe [NBT]
contents (Just (CompoundTag _ nbts)) = Just nbts
contents (Just (ListTag _ _ _ nbts)) = Just nbts
contents _ = Nothing

path :: [Maybe String] -> NBT -> Maybe NBT
path [p] nbt | p == tagName nbt =
  Just nbt
path (p : ps) (CompoundTag t nbts) | p == t =
  tryRest ps nbts
path (p : ps) (ListTag t _ _ nbts) | p == t =
  tryRest ps nbts
path _ _ = Nothing

tryRest :: [Maybe String] -> [NBT] -> Maybe NBT
tryRest ps nbts =
  case mapMaybe (path ps) nbts of
    [nbt] -> Just nbt
    _ -> Nothing

find :: Level -> Find -> [Item]
find level tree =
  findPlayers
  where
    findPlayers =
      concatMap findPlayer $ levelPlayers level
      where
        findPlayer player =
          case contents $ 
               path [Just "", Just "Inventory"] $ 
               playerData player of
            Just items -> map itemize items
            _ -> error $ "player " ++ playerName player ++ " has no inventory"
          where
            itemize item = Item {
              itemCoords = playerCoords,
              itemSource = ItemSourcePlayer $ playerName player,
              itemData = item }
              where
                playerCoords =
                  case contents $ 
                       path [Just "", Just "Pos"] $ 
                       playerData player of
                    Just [ DoubleTag Nothing x, 
                           DoubleTag Nothing y, 
                           DoubleTag Nothing z ] -> 
                      (floor x, floor y, floor z)
                    _ -> 
                      error $  "player " ++ playerName player ++ 
                               " has no position"
