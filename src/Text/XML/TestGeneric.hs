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

module Text.XML.TestGeneric where

import Test.Framework (defaultMain, testGroup)
import Test.HUnit
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck.Arbitrary
import Text.XML.Generic
-- import Text.XML.TestGenericData

import Text.XML.Light
import Data.Generics.Aliases
import Data.Data
-- import Utilities.Misc

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

prop_DecodeInteger i = i == (decodeXML ( "<Integer>" ++ show i ++ "</Integer>" ) :: Integer)
  where t = i::Integer

prop_DecodeString s = s == (decodeXML ( showElement blank_element {
    elName = blank_name { qName = "String"},
    elContent = [Text $ CData CDataText s Nothing]
  } ) :: String) || '\r' `elem` s
  where t = s::String

case_DecodeVoo =
  do let actual = decodeXML "<Voo />" :: Voo
         expected =  Voo
     expected @=? actual

case_DecodeVooz =
  do let actual = decodeXML "<Vooz><Integer>99</Integer></Vooz>" :: Voo
         expected =  Vooz 99
     expected @=? actual

case_DecodeBar =
  do let actual = decodeXML
           ("<Bar>" ++
           "<barA><String>Jaja</String></barA>" ++
           "<barB><Goo><String></String><String>Nejnej</String></Goo></barB></Bar>") :: Bar
         expected =  Bar "Jaja" $ Goo "" "Nejnej" 
     expected @=? actual

case_Decode_Bool =
  True @=? (decodeXML "<Bool>True</Bool>" :: Bool)

-- Encode 

case_Encode_True =
  do "<Bool>True</Bool>" @=? encodeXML True

case_Encode_Integer =
  do let expected = "<Integer>56</Integer>"
         actual = encodeXML (56 :: Integer)
     expected @=? actual

prop_Integer i = encodeXML i == "<Integer>" ++ show i ++ "</Integer>"
  where t = i::Integer

prop_String s = encodeXML s == showElement blank_element {
    elName = blank_name { qName = "String"},
    elContent = [Text $ CData CDataText s Nothing]
  }
  where t = s::String

case_Encode_Voo =
  do let expected = "<Voo />"
         actual = encodeXML Voo
     expected @=? actual

case_Encode_Vooz =
  do let expected = "<Vooz><Integer>99</Integer></Vooz>"
         actual = encodeXML $ Vooz 99
     expected @=? actual

case_Encode_Bar =
  do let expected =
           "<Bar>" ++
           "<barA><String>Jaja</String></barA>" ++
           "<barB><Goo><String></String><String>Nejnej</String></Goo></barB></Bar>"
         actual = encodeXML $ Bar "Jaja" $ Goo "" "Nejnej" 
     expected @=? actual

-- Data types for test

data Voo = Voo | Vooz Integer
	deriving (Show, Eq, Typeable, Data)

data Zoo = Zoo | Moo
	deriving (Show, Eq, Typeable, Data)

data Foo = Goo String String
	deriving (Show, Eq, Typeable, Data)

data Bar = Bar { barA :: String, barB :: Foo}
	deriving (Show, Eq, Typeable, Data)


-- Difficult scenarios

-- serializing from unknown data type
case_serialize_unknown_True =
  do let expected = "<Bool>True</Bool>"
         d = DataBox True
         actual = encodeUnknownXML d
     expected @=? actual


case_serialize_unknown_Integer =
  do let expected = "<Integer>42</Integer>"
         d = DataBox (42 :: Integer)
         actual = encodeUnknownXML d
     expected @=? actual

case_serialize_unknown_Voo =
  do let expected = "<Voo />"
         d = DataBox Voo
         actual = encodeUnknownXML d
     expected @=? actual

-- deserializing to unknown data type

case_Deserialize_unknown_True =
  True @=? (decodeUnknownXML "<Bool>True</Bool>" (&& True))
