{-# LANGUAGE FlexibleContexts #-}

module Find (
  Rel, ItemQualAttr(..), ItemQual(..), Find(..),
  makeTree, 
  ItemSource(..), Item(..),
  find )
where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe
import Data.NBT
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


data ItemSource = ItemSourcePlayer {ispName :: String}
                | ItemSourceTile {istDim :: String, istContainer :: String}
                | ItemSourceFree {isfDim :: String}

instance Show ItemSource where
  show (ItemSourceFree dim) = "free[dim=" ++ dim ++ "]"
  show (ItemSourcePlayer name) = "player[" ++ name ++ "]"
  show (ItemSourceTile dim container) = "tile[dim=" ++ dim ++ ",container=" ++ 
                                          container ++ "]"

data Item = Item {
  itemCoords :: (Int, Int, Int),
  itemSource :: ItemSource,
  itemData :: NBT }

instance Show Item where
  show (Item {itemCoords = (x, y, z), itemSource = source, itemData = nbt }) =
    printf "item=%d[x=%d,y=%d,z=%d,source=%s]" 
      itemId x y z (show source)
    where
      itemId =
        case path [Nothing, Just "id"] nbt of
          Just (ShortTag (Just "id") i) -> i
          _ -> error "item without id"
{-   itemTags =
        concat $ mapMaybe (fmap (',' :) . showTag) $ 
          fromJust $ contents $ Just nbt
        where
          showTag (CompoundTag (Just "tag") nbts)
            decodeTag nbts
            where 
              decodeTag nbts@(ListTag (Just "ench") CompoundType _ _) =
                let enchId =
                      case path [Just "ench", Just "id"] nbt of
                        Just (ShortTag (Just "id") i) -> i
                        _ -> error "ench without id" 
                    enchLevel =
                      case path [Just "ench", Just "lvl"] nbt of
                        Just (ShortTag (Just "lvl") i) -> i
                        _ -> error "ench without lvl"
                in
                Just $ printf "ench=%d[level=%d]" enchId enchLevel
              decodeTag _ = Nothing 
          showTag _ = Nothing -}

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
  findPlayers ++
  findWorld
  where
    findPlayers =
      concatMap findPlayer $ levelPlayers level
      where
        findPlayer player =
          case path [Just "", Just "Inventory"] $ 
               playerData player of
            Just (ListTag _ CompoundType _ items) -> map itemize items
            _ -> error $ "player " ++ playerName player ++ " has no inventory"
          where
            itemize item = Item {
              itemCoords = playerCoords,
              itemSource = ItemSourcePlayer $ playerName player,
              itemData = item }
              where
                playerCoords =
                  case path [Just "", Just "Pos"] $ 
                       playerData player of
                    Just (ListTag _ DoubleType _ [ 
                           DoubleTag Nothing x, 
                           DoubleTag Nothing y, 
                           DoubleTag Nothing z ]) -> 
                      (floor x, floor y, floor z)
                    _ -> 
                      error $  "player " ++ playerName player ++ 
                               " has no position"
    findWorld =
      let d = levelDims level in
      let ds = [("surface", dimsSurface d), 
                ("nether", dimsNether d), 
                ("end", dimsEnd d)] in
      let ws = [("free", "Entities")] in
      concatMap findDim $ (,) <$> ws <*> ds
      where
        findDim ((whichName, whichTag), (dimName, Just regions)) =
          concatMap findRegion regions
          where
            findRegion region =
              concatMap findChunk $ regionContents region
              where
                findChunk chunk =
                  case path [Nothing, Just "Level", Just whichTag] $ 
                       cdChunk chunk of
                    Just (CompoundTag (Just whichTag) nbts) ->
                      mapMaybe findItem nbts
                      where
                        findItem ent =
                          case path [Just "Item"] ent of
                            Just (CompoundTag (Just "Item") item) ->
                              Just $ Item {
                                itemCoords = globalCoords,
                                itemSource = ItemSourceFree dimName,
                                itemData = ent }
                              where
                                globalCoords =
                                  case path [Just "Pos"] ent of
                                    Just (ListTag _  DoubleType _ [ 
                                             DoubleTag Nothing x, 
                                             DoubleTag Nothing y, 
                                             DoubleTag Nothing z ]) ->
                                      let (rx, rz) = regionPos region in
                                      let (cx, cz) = ciPos $ 
                                                     cdChunkIndex chunk in
                                      (mcc rx cx (floor x), 
                                       floor y, 
                                       mcc rz cz (floor z))
                                      where
                                        mcc r c b =
                                          b + 16 * (c + 32 * r)
                                    _ -> error $ "entity has no position"
                            _ -> Nothing
                    _ -> []
        findDim _ = []
