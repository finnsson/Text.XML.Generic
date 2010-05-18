{-# LANGUAGE DeriveDataTypeable, PackageImports, GADTs, RankNTypes, StandaloneDeriving #-}
module Text.XML.Generic (
  decodeUnknownXML,
  fromUnknownXML,
  decodeXML,
  fromXML,
  encodeUnknownXML,
  encodeXML,
  toXML,
  DataBox (..)
  )  where

import Text.XML.Light
import Data.Generics.Aliases
import Data.List.Split
import Data.List
import Data.Data
import Utilities.Misc
import Maybe

import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
import Char

import "mtl" Control.Monad.State

decodeUnknownXML :: Data a => String -> (a -> b) -> b
decodeUnknownXML xml fn = fn $ decodeXML xml --  maybe undefined fromUnknownXML (parseXMLDoc xml)

decodeXML :: Data a => String -> a
decodeXML xml = maybe undefined fromXML (parseXMLDoc xml)


primitiveFromXML :: (Read d, Data d) => Element -> d
primitiveFromXML x = parsed -- res
  where
    parsed = read getContent
    getContent = showContent $ head $ elContent x
    myDataType = dataTypeOf parsed -- res

stringFromXML :: Element -> String
stringFromXML x = res
  where
    res = if length cont == 0 then "" else contentString $ head cont -- $ elContent x
    cont = elContent x
    contentString (Text (CData _ t _)) = t
    contentString _ = ""
    -- getContent = showContent $ head $ elContent x

type F a = Element -> a

fromUnknownXML :: Data a => Element -> (a -> b) -> b
fromUnknownXML xml fn = fn $ fromXML xml

-- fromUnknownXML :: Element -> DataBox
-- fromUnknownXML x = res
--   where
--     res = evalState ( fromConstrM f con ) children
--         where f :: (Data a) => State [Element] a
--               f = do es <- get
--                      do put (tail es)
--                         return $ fromXML (head es)
--     -- get type of first term from e
--     qname = qName $ elName x
--     xmlnss = filter (\(Attr k v) -> "xmlns" == (qName k))  $ elAttribs x
--     name = (if length xmlnss == 1 then (attrVal $ head xmlnss) ++ "." else "") ++ qname
--     -- 
--     myDataType :: DataType
--     myDataType = dataTypeOf res
-- 
--     children :: [Element]
--     children = [e | Elem e <- (elContent x)]
-- 
--     con :: Constr
--     con = fromMaybe undefined $ readConstr myDataType qname

fromXML :: Data d => Element -> d
fromXML e = fromXML'' e'
  where
    fromXML'' =
      fromXML' 
      `extR` stringFromXML
      `extR` (primitiveFromXML :: F Integer )
      `extR` (primitiveFromXML :: F Int)
      `extR` (primitiveFromXML :: F Bool)
      `extR` (primitiveFromXML :: F Word8)
      `extR` (primitiveFromXML :: F Word16)
      `extR` (primitiveFromXML :: F Word32)
      `extR` (primitiveFromXML :: F Word64)
      `extR` (primitiveFromXML :: F Int8)
      `extR` (primitiveFromXML :: F Int16)
      `extR` (primitiveFromXML :: F Int32)
      `extR` (primitiveFromXML :: F Int64)
      `extR` (primitiveFromXML :: F Double)
      `extR` (primitiveFromXML :: F Float)
    e' = if (isUpper $ head $ qName $ elName e)
         then e
         else head $ [e | Elem e <- (elContent e)]


fromXML' :: Data b => Element -> b
fromXML' x = res 
  where
    res = case dataTypeRep myDataType of
      -- AlgRep [unitConstr] -> ()
      AlgRep _ -> evalState ( fromConstrM f con ) children
        where f :: (Data a) => State [Element] a
              f = do es <- get
                     do put (tail es)
                        return $ fromXML (head es)
      CharRep -> fromConstr $ mkCharConstr myDataType (head getContent)
      -- _ -> (error "no") `extR` (primitiveFromXML x :: b)
      NoRep -> error "no"
      _ -> error "This case should not occur."

    -- FIX! I need to sort the children in the order con needs them!
    children :: [Element]
    children = [e | Elem e <- (elContent x)]

    getContent :: String
    getContent = showContent $ head $ elContent x

    conRep :: ConstrRep
    conRep = constrRep con

    con :: Constr
    con = fromMaybe undefined $ readConstr myDataType qname

    qname = qName $ elName x

    myDataType :: DataType
    myDataType = dataTypeOf res
    

encodeUnknownXML :: DataBox -> String
encodeUnknownXML (DataBox b) =  encodeXML b --showElement $ toXML b

toUnknownXML :: DataBox -> Element
toUnknownXML (DataBox b) = toXML b

-- \ Encode a Data into a String.
-- .
-- E.g.
-- .
-- > encodeXML 54
-- > <Integer>54</Integer>
-- 
-- > encodeXML User { age = 34, name = "Pelle" }
-- > <User><age><Integer>34</Integer></age><name><String>Pelle</String></name></User>
-- 
-- > Fullname "Pelle" "Larsson"
-- > <Fullname><String>Pelle</String><String>Larsson</String></Fullname>
--
-- > encodeXML "test"
-- > <String>test</String>
--
-- > encodeXML ["yes", "no"]
-- > <List><String>yes</String><String>no</String></List>
--
-- > encodeXML ("answer", 42)
-- > <Tuple><String>answer</String><Integer>42</Integer></Tuple>
encodeXML :: Data a => a -> String
encodeXML = showElement . toXML

type T a = a -> Element

-- | Serialize to XML.
toXML :: Data a => a -> Element
toXML = toXMLgeneric
        `ext1Q` xmlList
	`extQ` (showXmlString :: T String)
        `extQ` (showXmlUnit :: T ())
        `extQ` (showXml :: T Bool)
	`extQ` (showXml :: T Integer)
        `extQ` (showXml :: T Int)
	`extQ` (showXml :: T Char)
        `extQ` (showXml :: T Word8)
        `extQ` (showXml :: T Word16)
        `extQ` (showXml :: T Word32)
        `extQ` (showXml :: T Word64)
        `extQ` (showXml :: T Int8)
        `extQ` (showXml :: T Int16)
        `extQ` (showXml :: T Int32)
        `extQ` (showXml :: T Int64)
        `extQ` (showXml :: T Double)
        `extQ` (showXml :: T Float)
        -- More special cases.
        `extQ` (showXml :: T I.IntSet)
        `extQ` (showXml :: T S.ByteString)
        `extQ` (showXml :: T L.ByteString)

       
toXMLgeneric :: (Data a) => a -> Element
toXMLgeneric x =
  let dtr = dataTypeRep $ dataTypeOf x
  in case dtr of
        AlgRep _ -> element algName algNsName $ baker $ map Elem $ gmapQ toXML x
		where	name = splitWhen (== '.') $ dataTypeName $ dataTypeOf x
			algName = typeName x
			algNsName =	if algName == "Tuple"
					then Nothing
					else Just $ foldl (++) "" $ intersperse "." $ init name
			fields :: [String]
			fields = constrFields $ toConstr x
			baker :: [Content] -> [Content]
			baker conts  = if fields == [] 
				then conts 
				else map (\(f,c) -> Elem (element f Nothing [c]) ) (zip fields conts)
        _ -> error "Only AlgRep should get here!"

typeName :: Data a =>a -> String
typeName x = name
	where
		n = toString $ toConstr x
		name =	if n `elem` ["(,)",  "(,,)", "(,,,)", "(,,,,)", "(,,,,,)", "(,,,,,,)", "(,,,,,,,)"]
			then "Tuple"
			else n

xmlList :: Data a => [a] -> Element
xmlList xs = Element (QName "List" Nothing Nothing) [] (map (Elem . toXML) xs) Nothing

-- showXmlTuple2 :: (Data a) => (a,a) -> Element
showXmlTuple2 :: (Data a, Data b) => (a, b) -> Element
showXmlTuple2 x = element "Tuple" Nothing [Elem $ toXML (fst x), Elem $ toXML (snd x)]

showXmlString x = element "String" Nothing (contentText x)

showXmlUnit x = element "Unit" Nothing []

showXml x = element elemName nsName $ contentText (toString x)
  where name = splitWhen (== '.') $ dataTypeName $ dataTypeOf x
        elemName = last name
        nsName = Nothing


element name ns content =
  Element {
    elName = QName {qName = name, qURI = Nothing, qPrefix = Nothing}
    , elAttribs = namespace
    , elContent =  content -- [CRef content] --
    , elLine = Nothing
  }
  where
    namespace =
      case ns of
        Nothing -> []
        Just n -> [Attr (QName "xmlns" Nothing Nothing) n] -- ("http://www.haskell.org/hoogle/?hoogle=" ++ n)]

contentText c = [Text (CData CDataText c Nothing)]


-- Data type

data DataBox where
  DataBox :: (Show d, Eq d, Data d) => d -> DataBox


instance Show DataBox where
  show (DataBox b) = show b

instance Eq DataBox where
  (DataBox v) == (DataBox b) = 
    if (typeOf v == typeOf b)
    then True
    else False

instance Typeable DataBox where
  typeOf (DataBox d) = typeOf d

-- deriving instance Data DataBox

-- instance Data (DataBox) where
--   gfoldl k z (DataBox d) = z DataBox `k` d -- OK gfoldl x1 x2 v
-- 
--   gunfold k z c = k (z dataBox) -- (DataBox::d->DataBox d)) -- error "my gungold" -- k (z DataBox)  -- OK gunfold d --  -- 
--   toConstr (DataBox d) = toConstr d -- OK
--   dataTypeOf (DataBox d) = dataTypeOf d -- OK
--   -- dataCast1 = gcast1 --dataCast1 d
--   -- dataCast2 = gcast2 -- dataCast2 d

dataBox :: forall d. (Data d, Show d, Eq d) => d -> DataBox
dataBox x = DataBox x
