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
import TestGenerator
import Test.QuickCheck.Arbitrary
import Text.XML.Generic

import Text.XML.Light
import Data.Generics.Aliases
import Data.Data
import Utilities.Misc

import Data.Char

main = $(defaultMainGenerator)

-- Decode

caseDecodeString =
  do let expected = ""
         actual = decodeXML "<String />" :: String
     expected @=? actual

caseDecodeArrowString =
  do let expected = ">"
         actual = decodeXML "<String>&gt;</String>" :: String
     expected @=? actual

caseDecodeInteger =
  do let expected = 56
         actual = decodeXML "<Integer>56</Integer>" :: Integer
     expected @=? actual

propDecodeInteger i = i == (decodeXML ( "<Integer>" ++ show i ++ "</Integer>" ) :: Integer)
  where t = i::Integer

propDecodeString s = s == (decodeXML ( showElement blank_element {
    elName = blank_name { qName = "String"},
    elContent = [Text $ CData CDataText s Nothing]
  } ) :: String) || '\r' `elem` s
  where t = s::String

caseDecodeVoo =
  do let actual = decodeXML "<Voo xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\" />" :: Voo
         expected =  Voo
     expected @=? actual

caseDecodeVooz =
  do let actual = decodeXML "<Vooz xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\"><Integer>99</Integer></Vooz>" :: Voo
         expected =  Vooz 99
     expected @=? actual

caseDecodeBar =
  do let actual = decodeXML
           ("<Bar xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\">" ++
           "<barA><String>Jaja</String></barA>" ++
           "<barB><Goo xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\"><String></String><String>Nejnej</String></Goo></barB></Bar>") :: Bar
         expected =  Bar "Jaja" $ Goo "" "Nejnej" 
     expected @=? actual

-- Encode 

caseInteger =
  do let expected = "<Integer>56</Integer>"
         actual = encodeXML (56 :: Integer)
     expected @=? actual

propInteger i = encodeXML i == "<Integer>" ++ show i ++ "</Integer>"
  where t = i::Integer

propString s = encodeXML s == showElement blank_element {
    elName = blank_name { qName = "String"},
    elContent = [Text $ CData CDataText s Nothing]
  }
  where t = s::String

caseVoo =
  do let expected = "<Voo xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\" />"
         actual = encodeXML Voo
     expected @=? actual

caseVooz =
  do let expected = "<Vooz xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\"><Integer>99</Integer></Vooz>"
         actual = encodeXML $ Vooz 99
     expected @=? actual

caseBar =
  do let expected =
           "<Bar xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\">" ++
           "<barA><String>Jaja</String></barA>" ++
           "<barB><Goo xmlns=\"http://www.haskell.org/hoogle/?hoogle=Text.XML.TestGeneric\"><String></String><String>Nejnej</String></Goo></barB></Bar>"
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
