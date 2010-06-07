{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, GADTs,
    ImplicitParams, ScopedTypeVariables, UnboxedTuples,
    TypeSynonymInstances, StandaloneDeriving, FlexibleContexts,
    FlexibleInstances, ConstrainedClassMethods, MultiParamTypeClasses,
    FunctionalDependencies, MagicHash, PolymorphicComponents,
    ExistentialQuantification, UnicodeSyntax, PostfixOperators,
    PatternGuards, LiberalTypeSynonyms, RankNTypes,
    TypeOperators, DoRec, ParallelListComp, EmptyDataDecls,
    KindSignatures, GeneralizedNewtypeDeriving, TypeFamilies,
    TemplateHaskell, DeriveDataTypeable #-}

module Text.XML.ExTestGeneric where

import Test.Framework (defaultMain, testGroup)
import Test.HUnit
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck.Arbitrary
import Text.XML.ExGeneric
-- import Text.XML.Generic
import Text.XML.Generic hiding (fromXML, decodeXML, DataBox, decodeUnknownXML, fromUnknownXML, decodeUnknownXML')
-- import Text.XML.TestGenericData

import DataEx

import Text.XML.Light
import Data.Generics.Aliases
import qualified Data.Data as Old
import Utilities.Misc

import Data.Char

main = $(defaultMainGenerator)

-- Decode

case_DecodeString =
  do let expected = ""
         actual = decodeXML "<String />" :: String
     expected @=? actual

case_DecodeArrowString =
  do let expected = ">"
         actual = decodeXML "<String>&gt;</String>" :: String
     expected @=? actual

case_DecodeInteger =
  do let expected = 56
         actual = decodeXML "<Integer>56</Integer>" :: Integer
     expected @=? actual

_prop_DecodeInteger i = i == (decodeXML ( "<Integer>" ++ show i ++ "</Integer>" ) :: Integer)
  where t = i::Integer

_prop_DecodeString s = s == (decodeXML ( showElement blank_element {
    elName = blank_name { qName = "String"},
    elContent = [Text $ CData CDataText s Nothing]
  } ) :: String) || '\r' `elem` s
  where t = s::String

case_DecodeVoo =
  do let actual = decodeXML "<Voo xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\" />" :: Voo
         expected =  Voo
     expected @=? actual

case_DecodeVooz =
  do let actual = decodeXML "<Vooz xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\"><Integer>99</Integer></Vooz>" :: Voo
         expected =  Vooz 99
     expected @=? actual

-- case_DecodeBar =
--   do let actual = decodeXML
--            ("<Bar xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\">" ++
--            "<barA><String>Jaja</String></barA>" ++
--            "<barB><Goo xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\"><String></String><String>Nejnej</String></Goo></barB></Bar>") :: Bar
--          expected =  Bar "Jaja" $ Goo "" "Nejnej" 
--      expected @=? actual

case_Decode_Bool =
  True @=? (decodeXML "<Bool>True</Bool>" :: Bool)


-- Data types for test

data Voo = Voo | Vooz Integer
	deriving (Show, Eq, Old.Typeable, Old.Data)

data Zoo = Zoo | Moo
	deriving (Show, Eq, Old.Typeable, Old.Data)

data Foo = Goo String String
	deriving (Show, Eq, Old.Typeable, Old.Data)

data Bar = Bar { barA :: String, barB :: Foo}
	deriving (Show, Eq, Old.Typeable, Old.Data)

  
-- DataEx Voo

vooConstr, voozConstr :: AlgConstr
vooConstr  = mkConstr vooDataType "Voo" [] Prefix
voozConstr   = mkConstr vooDataType "Vooz"  [] Prefix

vooDataType :: AlgDataType
vooDataType = AlgDataType [vooConstr,voozConstr]


instance DataEx Voo where
  toConstr Voo = Constr vooConstr
  toConstr (Vooz _)  = Constr voozConstr
  gunfold k z c  = case constrIndex c of
                     1 -> z Voo
                     2 -> k (z Vooz)
                     _ -> error "gunfold"
  dataTypeOf _ = DataType vooDataType


-- DataEx Zoo

zooConstr, mooConstr :: AlgConstr
zooConstr  = mkConstr zooDataType "Zoo" [] Prefix
mooConstr   = mkConstr zooDataType "Moo"  [] Prefix

zooDataType :: AlgDataType
zooDataType = AlgDataType [zooConstr,mooConstr]


instance DataEx Zoo where
  toConstr Zoo = Constr zooConstr
  toConstr Moo = Constr mooConstr
  gunfold k z c  = case constrIndex c of
                     1 -> z Zoo
                     2 -> z Moo
                     _ -> error "gunfold"
  dataTypeOf _ = DataType zooDataType

-- DataEx Foo

-- DataEx Bar


-- Difficult scenarios

-- deserializing to unknown data type

case_Deserialize_unknwon_Int =
  (DataBox ("42" :: String)) @=? (decodeXML "<DataBox><String>hej</String></DataBox>")

  -- (DataBox (42 :: Integer)) @=? (decodeXML "<DataBox><Integer>42</Integer></DataBox>")

-- decodeUnknownXML

-- case_Deserialize_unknown_True =
--  True @=? (decodeUnknownXML "<Bool>True</Bool>" (&& True))
