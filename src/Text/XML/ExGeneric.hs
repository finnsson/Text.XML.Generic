{-# LANGUAGE
    ExistentialQuantification,
    Rank2Types,
    ScopedTypeVariables,
    PatternGuards,
    DeriveDataTypeable, 
    PackageImports, 
    GADTs, 
    RankNTypes, 
    StandaloneDeriving #-}


-- use toConstr & constrIndex in order to find out which constr is used for an Alg

module Text.XML.ExGeneric (
  decodeUnknownXML,
  decodeUnknownXML',
  fromUnknownXML,
  decodeXML,
  fromXML,
  DataBox (..)
  )  where

import Text.XML.Light
import Data.Generics (gnodecount, extQ, ext1Q, extR, geq, gmapQ, mkCharConstr, constrFields)
import Data.Typeable (mkTyCon, mkTyConApp, typeOf, Typeable (..), cast)
import qualified Data.Data as Old
-- import Data.Data ()

import Data.List.Split
import Data.List
-- import Data.Data
import Utilities.Misc
import Maybe

import DataEx
import Text.XML.Generic hiding (fromXML, decodeXML, DataBox, decodeUnknownXML, fromUnknownXML, decodeUnknownXML') -- (DataBox (..))
-- import qualified Text.XML.Generic as Gen

import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
import Char

import "mtl" Control.Monad.State


-- From String/XML to Haskell

decodeUnknownXML' :: String -> DataBox
decodeUnknownXML' xml = maybe (error "decodeUnknownXML' failed. Not valid xml.") fromUnknownXML' (parseXMLDoc xml)

decodeUnknownXML :: DataEx a => String -> (a -> b) -> b
decodeUnknownXML xml fn = fn $ decodeXML xml --  maybe undefined fromUnknownXML (parseXMLDoc xml)

decodeXML :: DataEx a => String -> a
decodeXML xml = maybe undefined fromXML (parseXMLDoc xml)


primitiveFromXML :: (Read d, DataEx d) => Element -> d
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

fromUnknownXML :: DataEx a => Element -> (a -> b) -> b
fromUnknownXML xml fn = fn $ fromXML xml

fromUnknownXML' :: Element -> DataBox
fromUnknownXML' x = res
  where
    res = evalState ( fromConstrM f con ) children
        where f :: (DataEx a) => State [Element] a
              f = do es <- get
                     -- let e' = if length es == 0 then (error "es is zero") else es
                     do put (tail es)
                        return $ fromXML (head es)
    -- get type of first term from e
    qname = qName $ elName x
    xmlnss = filter (\(Attr k v) -> "xmlns" == (qName k))  $ elAttribs x
    name = (if length xmlnss == 1 then (attrVal $ head xmlnss) ++ "." else "") ++ qname
    -- 
    myDataType :: DataType
    myDataType = dataTypeOf res

    children :: [Element]
    children = [e | Elem e <- (elContent x)]

    con :: Constr
    -- con = fromMaybe undefined $ readConstr (dataTypeDataRep myDataType) qname
    con = fromJust $ case myDataType of DataType x -> (readConstr x qname)
    
-- forall datare. (Typeable datarep, Show datarep, Eq datarep, 
-- 		     ReadCtor datarep)
-- dataRep :: forall datare. (Typeable datare, Show datare, Eq datare, ReadCtor datare) => DataType -> datare
-- dataRep (DataType rep) = rep

-- dataRep = dataTypeDataRep

fromXML :: DataEx d => Element -> d
fromXML e = fromXML'' e'
  where
    fromXML'' =
      fromXML' 
      `extR` stringFromXML
      `extR` (primitiveFromXML :: F Integer )
      `extR` (primitiveFromXML :: F Int)
      `extR` (primitiveFromXML :: F Bool)
      -- `extR` (primitiveFromXML :: F Word8)
      -- `extR` (primitiveFromXML :: F Word16)
      -- `extR` (primitiveFromXML :: F Word32)
      -- `extR` (primitiveFromXML :: F Word64)
      -- `extR` (primitiveFromXML :: F Int8)
      -- `extR` (primitiveFromXML :: F Int16)
      -- `extR` (primitiveFromXML :: F Int32)
      -- `extR` (primitiveFromXML :: F Int64)
      -- `extR` (primitiveFromXML :: F Double)
      -- `extR` (primitiveFromXML :: F Float)
    e' = if (qName $ elName e) == []
         then (error $ "qName in fromXML is []. e: " ++ show e) :: Element
         else
           if (isUpper $ head $ qName $ elName e)
           then e
           else 
             if  length [e | Elem e <- (elContent e)] == 0
             then (error $ "elContent in e in fromXML is []. e: " ++ show e) :: Element
             else head $ [e | Elem e <- (elContent e)]

getConstr :: DataType -> String -> Maybe Constr
getConstr (DataType t) = readConstr t

fromXML' :: DataEx b => Element -> b
fromXML' x = res 
  where
    res = r myDataType
    -- r :: DataType -> b 
    r (DataType x) | Just (AlgDataType _) <- cast x =
        evalState ( fromConstrM f con ) children
        where f :: (DataEx a) => State [Element] a
              f = do es <- get
                     do put (tail es)
                        return $ fromXML (head es)
    r (DataType x) | Just CharDataType <- cast x =
        fromConstr $ fromJust $ getConstr myDataType getContent --mkCharConstr myDataType (head getContent)
    r (DataType x) | Just (DataBox _) <- cast x =
        evalState ( fromConstrM f (con) ) children
        where f :: (DataEx a) => State [Element] a
              f = do es <- get
                     do put (tail es)
                        return $ fromXML (head es)
        --fromConstr $ fromJust $ 
    r (DataType x) = error $ "In fromXML' typeOf x: " ++ (show $ typeOf x) --"In fromXml' " ++ (show x) -- "no"
      -- _ -> (error "no") `extR` (primitiveFromXML x :: b)
      -- _ -> error "no"
      -- _ -> error "This case should not occur."

    -- FIX! I need to sort the children in the order con needs them!
    children :: [Element]
    children = [e | Elem e <- (elContent x)]

    getContent :: String
    getContent = showContent $ head $ elContent x

    -- conRep :: ConstrRep
    -- conRep = constrRep con

    con :: Constr
    con = fromJust $ getConstr myDataType qname --readConstr myDataType qname

    qname = qName $ elName x

    myDataType :: DataType
    myDataType = dataTypeOf res
    

-- Data type

data DataBox where
  DataBox :: (Show d, Eq d, DataEx d) => d -> DataBox

instance Show DataBox where
  show (DataBox b) = show b

instance Typeable DataBox where
  typeOf _ = mkTyConApp (mkTyCon "DataBox") []

instance Old.Data DataBox where
  gfoldl k z (DataBox d) = z DataBox `k` d
  gunfold k z c = error "Use DataEx.gunfold instead."  --(if True then k (z dbBool) else k (z db))
          
  toConstr (DataBox d) = Old.toConstr d
  dataTypeOf (DataBox d) = Old.dataTypeOf d

-- db :: (Integer -> DataBox)
-- db = DataBox

-- dbBool :: (Bool -> DataBox)
-- dbBool = DataBox

instance Eq DataBox where
  DataBox x == DataBox y | Just y' <- cast y = x == y'
  _ == _  = False

instance ReadCtor DataBox where
    readConstr (DataBox d) str = Just . Constr . ExConstr $ d -- ????

-- begin DataTypeOfDataEx

dataTypeOfDataConstr :: AlgConstr
dataTypeOfDataConstr  = mkConstr dataTypeOfDataExDataType "DataTypeOfDataEx" [] Prefix

dataTypeOfDataExDataType = AlgDataType [dataTypeOfDataConstr]

data DataTypeOfDataEx = DataTypeOfDataEx
  deriving (Show, Eq, Typeable, Old.Data)

instance DataEx DataTypeOfDataEx where
  toConstr _ = Constr dataTypeOfDataConstr
  gunfold _ z c  = z DataTypeOfDataEx 
  dataTypeOf _ = DataType dataTypeOfDataExDataType

-- end DataTypeOfDataEx

instance DataEx DataBox where
  dataTypeOf _ = DataType (DataBox $ DataBox (DataTypeOfDataEx)) -- ("DataEx.dataTypeOf"::String))
  toConstr = Constr . ExConstr

  gunfold k z (Constr c) | Just (ExConstr ec)    <- cast c,
                           Just (DataBox (_::a)) <- cast ec =
             k (z (DataBox::a -> DataBox))
  -- gunfold k z (Constr c) | Just (ExConstr ec) <- cast c =
  --  gunfold k z (Constr ec)
    --k (z (DataBox:: a -> 
  gunfold k z (Constr c) = error $ show c -- (++) "In gunfold: " $ if c == undefined then "undef" else "c" -- (show c) -- ++ (show c) -- (show k) ++ (show z) ++ (show c)


data ExConstr = forall a. Typeable a => ExConstr a

instance Show ExConstr where
  --show (ExConstr a) = "(ExConstr " ++ show a ++ ")"
  show (ExConstr a) = "(ExConstr " ++ show (typeOf a) ++ ")"

instance Typeable ExConstr where
  typeOf _ = mkTyConApp (mkTyCon "ExConstr") []

instance Eq ExConstr where
    ExConstr x == ExConstr y = typeOf x == typeOf y
