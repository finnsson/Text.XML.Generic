{-# LANGUAGE DeriveDataTypeable, PackageImports, GADTs, RankNTypes, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
module Text.XML.Generic (
  -- ** Decode
  decodeXML,
  fromXML,
  -- ** Encode
  encodeXML,
  toXML
  )  where

import Text.XML.Light
import Data.Generics
import Data.List.Split
import Data.List
import Data.Data
import NIB.String
import Data.Maybe
import Data.Either

import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
import Data.Char

import "mtl" Control.Monad.State

--------------------------------------------------------------------------
-- ** Decode

decodeXML :: Data a => String -> Either String a
decodeXML xml = maybe (Left "Could not parse xml in decodeXML") fromXML (parseXMLDoc xml)

primitiveFromXML :: (Read d, Data d) => Element -> Either String d
primitiveFromXML x = parsed
  where
    parsed = maybe (Left "No content.") (\c -> Right $ read $ showContent c ) getContent
    getContent = listToMaybe $ elContent x
    myDataType = dataTypeOf parsed

stringFromXML :: Element -> Either String String
stringFromXML x = Right $ res
  where
    res = if null cont then "" else contentString $ head cont
    cont = elContent x
    contentString (Text (CData _ t _)) = t
    contentString _ = ""

type F a = Element -> Either String a

fromXML :: Data d => Element -> Either String d
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
    e' = if qName ( elName e) == []
         then error $ "qName in fromXML is []. e: " ++ show e :: Element
         else
           if isUpper $ head $ qName $ elName e
           then e
           else 
             if  null [e | Elem e <- elContent e]
             then error $ "elContent in e in fromXML is []. e: " ++ show e :: Element
             else head [e | Elem e <- elContent e]




instance Monad (Either String) where
  return v = Right v
  fail s = Left s
  (Left s) >>= _ = Left s
  (Right v) >>= f = f v


fromXML' :: Data b => Element -> Either String b
fromXML' x = res 
  where
    res = case dataTypeRep myDataType of
      AlgRep _ -> evalStateT ( fromConstrM f con ) children
        where f :: (Data a) => StateT [Element] (Either String) a
              f = do es <- get
                     case es of [] -> lift $ Left "construct: empty list"
                                e' : es' -> do put es'; lift $ fromXML e'
      CharRep -> maybe (Left "No content") (Right . fromConstr . (mkCharConstr myDataType)) content
        where content = listToMaybe getContent
      NoRep -> Left "NoRep in fromXML'"
      _ -> Left "This case should not occur."

    -- FIX! I need to sort the children in the order con needs them!
    children :: [Element]
    children = [e | Elem e <- elContent x]

    getContent :: String
    getContent = showContent $ head $ elContent x

    conRep :: ConstrRep
    conRep = constrRep con

    con :: Constr
    con = fromMaybe (error $ "No Constr by name " ++ qname ++ " and DataType " ++ (show myDataType)) $ readConstr myDataType qname

    qname = qName $ elName x

    resType :: Either String b -> b
    resType _ = error "resType"

    myDataType :: DataType
    myDataType = dataTypeOf $ resType res
    

--------------------------------------------------------------------------
-- ** Encode

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
    , elContent =  content 
    , elLine = Nothing
  }
  where
    namespace = []

contentText c = [Text (CData CDataText c Nothing)]
