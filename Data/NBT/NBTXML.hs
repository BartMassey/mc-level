-- Copyright © 2012 Bart Massey
-- Converter between Minecraft 'NBT' and 'Text.XML.Light' 'Node's.

module Data.NBT.NBTXML (module Text.XML.Light, nbtToXml)
where

import Data.Int
import Data.NBT
import Text.XML.Light
import Data.Array.IArray

typeName :: TagType -> String
typeName EndType = error "illegal end tag"
typeName ByteType = "byte"
typeName ShortType = "short"
typeName IntType = "int"
typeName LongType = "long"
typeName FloatType = "float"
typeName DoubleType = "double"
typeName ByteArrayType = "byte-array"
typeName StringType = "string"
typeName ListType = "list"
typeName CompoundType = "compound"
typeName IntArrayType = "int-array"

nbtToXml :: NBT -> Element
nbtToXml EndTag =
  error "unmatched TAG_End in NBT"
nbtToXml (ByteTag name value) =
  makeScalarTag "byte" name (show value)
nbtToXml (ShortTag name value) =
  makeScalarTag "short" name (show value)
nbtToXml (IntTag name value) =
  makeScalarTag "int" name (show value)
nbtToXml (LongTag name value) =
  makeScalarTag "long" name (show value)
nbtToXml (FloatTag name value) =
  makeScalarTag "float" name (show value)
nbtToXml (DoubleTag name value) =
  makeScalarTag "double" name (show value)
nbtToXml (ByteArrayTag name count values) =
  makeArrayTag "byte-array" name count values
nbtToXml (StringTag name _ value) =
  makeScalarTag "string" name value
nbtToXml (ListTag name tagtype count values) =
  let countAttr = makeAttr "count" (show count) in
  let typeAttr = makeAttr "type" (typeName tagtype) in
  let es = map (Elem . nbtToXml) values in
  makeTag "list" name [countAttr, typeAttr] es
nbtToXml (CompoundTag name values) =
  makeTag "compound" name [] $ map (Elem . nbtToXml) values
nbtToXml (IntArrayTag name count values) =
  makeArrayTag "int-array" name count values

makeArrayTag :: (IArray a e, Show e) => 
                String -> Maybe String -> Int32 -> a Int32 e -> Element
makeArrayTag tag name count values =
  let countAttr = makeAttr "count" (show count) in
  let es = map mkElem $ assocs values in
  makeTag tag name [countAttr] es
  where
    mkElem (i, v) =
      let indexAttr = makeAttr "index" (show i) in
      Elem $ makeTag "byte" Nothing [indexAttr] [CRef (show v)]

makeScalarTag :: String -> Maybe String -> String -> Element
makeScalarTag tag name value =
  makeTag tag name [] [CRef value]

makeTag :: String -> Maybe String -> [Attr] -> [Content] -> Element
makeTag tag Nothing attrs values = 
  Element (unqual tag) attrs values Nothing
makeTag tag (Just name) attrs values =
  makeTag tag Nothing (makeAttr "name" name : attrs) values

makeAttr :: String -> String -> Attr
makeAttr name value =
  Attr (unqual name) value
