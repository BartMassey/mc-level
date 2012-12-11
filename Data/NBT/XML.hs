-- Copyright © 2012 Bart Massey
-- Converter between Minecraft 'NBT' and 'Text.XML.Light' 'Node's.

module Data.NBT.XML (module Text.XML.Light, nbtToXml)
where

import Numeric (showFFloat, showEFloat)
import Data.Int
import Data.NBT
import Text.XML.Light
import Data.Array.IArray

typeNames :: [(TagType, String)]
typeNames = [
  (ByteType, "byte"),
  (ShortType, "short"),
  (IntType, "int"),
  (LongType, "long"),
  (FloatType, "float"),
  (DoubleType, "double"),
  (ByteArrayType, "byte-array"),
  (StringType, "string"),
  (ListType, "list"),
  (CompoundType, "compound"),
  (IntArrayType, "int-array") ]

typeName :: TagType -> String
typeName tag =
  case lookup tag typeNames of
    Just name -> name
    Nothing -> error "illegal tag"

nameType :: String -> TagType
nameType name =
  let nameTypes = map (\(a, b) -> (b, a)) typeNames in
  case lookup name nameTypes of
    Just tag -> tag
    Nothing -> error $ "illegal tag name " ++ name

nbtToXml :: NBT -> Element
nbtToXml EndTag =
  error "unmatched TAG_End in NBT"
nbtToXml (ByteTag name value) =
  makeScalarTag ByteType name (show value)
nbtToXml (ShortTag name value) =
  makeScalarTag ShortType name (show value)
nbtToXml (IntTag name value) =
  makeScalarTag IntType name (show value)
nbtToXml (LongTag name value) =
  makeScalarTag LongType name (show value)
nbtToXml (FloatTag name value) =
  makeScalarTag FloatType name (genericFloat value)
nbtToXml (DoubleTag name value) =
  makeScalarTag DoubleType name (genericFloat value)
nbtToXml (ByteArrayTag name count values) =
  makeArrayTag ByteArrayType name count values
nbtToXml (StringTag name _ value) =
  makeScalarTag StringType name value
nbtToXml (ListTag name tagtype count values) =
  let countAttr = makeAttr "count" (show count) in
  let typeAttr = makeAttr "type" (typeName tagtype) in
  makeTag ListType name [countAttr, typeAttr] $ makeListContent values
nbtToXml (CompoundTag name values) =
  makeTag CompoundType name [] $ map (Elem . nbtToXml) values
nbtToXml (IntArrayTag name count values) =
  makeArrayTag IntArrayType name count values

makeArrayTag :: (IArray a e, Show e) => 
                TagType -> Maybe String -> Int32 -> a Int32 e -> Element
makeArrayTag tag name count values =
  let countAttr = makeAttr "count" (show count) in
  let es = map mkElem $ assocs values in
  makeTag tag name [countAttr] es
  where
    mkElem (i, v) =
      let indexAttr = makeAttr "index" (show i) in
      Elem $ makeTag ByteType Nothing [indexAttr] [makeText (show v)]

makeScalarTag :: TagType -> Maybe String -> String -> Element
makeScalarTag tag name value =
  makeTag tag name [] [makeText value]

makeTag :: TagType -> Maybe String -> [Attr] -> [Content] -> Element
makeTag tag Nothing attrs values = 
  Element (unqual (typeName tag)) attrs values Nothing
makeTag tag (Just name) attrs values =
  makeTag tag Nothing (makeAttr "name" name : attrs) values

makeAttr :: String -> String -> Attr
makeAttr name value =
  Attr (unqual name) value

makeText :: String -> Content
makeText s =
  Text $ CData CDataText s Nothing

genericFloat :: RealFloat a => a -> String
genericFloat v 
  | abs v > 0.0000001 && abs v <1000000.0 = showFFloat Nothing v ""
  | otherwise = showEFloat Nothing v ""

makeListContent :: [NBT] -> [Content]
makeListContent values =
  zipWith mkContent [0 :: Int32 ..] values
  where
    mkContent i v =
      Elem $ add_attr (makeAttr "index" (show i)) $ nbtToXml v
