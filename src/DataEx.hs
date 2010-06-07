{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

-- Re-implemenation of the unfolding part of Data.hs, 
-- to make the representation for DataType and Constr extensible
--
-- The current version is also ``more strongly typed'': we avoid a class
-- of run-time errors like
--   error "constrFields"
--   error "mkIntConstr"
-- compared to the original Data/Data.hs
--
-- In general, our code parallels Data/Data.hs with extensions where noted

module DataEx (
	        DataEx(..)
	      , fromConstr
	      , fromConstrB
	      , fromConstrM
	      , DataType(..)
	      , Constr(..)
	      , ConIndex
	      , Fixity (..)
	      , ReadCtor(..)
	      , AlgDataType(..)
	      , AlgConstr (..)
	      , constrIndex
	      , mkConstr
	      , IntDataType(..), IntConstr(..)
	      , CharDataType(..), CharConstr(..)
	      -- , module Data.Typeable
	      )
    where

import Data.Typeable
import qualified Data.Data as OldD


-- Our variant of the Data class, which uses our new Constr and Datatype types
--
-- Generic folding and related operations (mapT, mapQ, etc) remain as
-- they were in the original Data.hs. Therefore, we elide the
-- gfold method in the DataEx class below.

-- The text of DataEx methods is identical to those in the original Data.hs.
-- The only difference is the new definitions for Constr and Datatype,
-- given in this file.

class OldD.Data a => DataEx a where

  -- | Unfolding constructor applications
  gunfold :: (forall b r. DataEx b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  -- | Obtaining the constructor from a given datum.
  -- For proper terms, this is meant to be the top-level constructor.
  -- Primitive datatypes are here viewed as potentially infinite sets of
  -- values (i.e., constructors).
  toConstr   :: a -> Constr


  -- | The outer type constructor of the type
  dataTypeOf  :: a -> DataType


------------------------------------------------------------------------------
--
--      Generic unfolding (taken verbatim from Data.hs)
--
------------------------------------------------------------------------------

-- | The identity type constructor needed for the definition of gmapT
newtype ID x = ID { unID :: x }

-- | Build a term skeleton
fromConstr :: DataEx a => Constr -> a
fromConstr = fromConstrB undefined


-- | Build a term and use a generic function for subterms
fromConstrB :: DataEx a
            => (forall d. DataEx d => d)
            -> Constr
            -> a
fromConstrB f = unID . gunfold k z
 where
  k c = ID (unID c f)
  z = ID

-- | Monadic variation on 'fromConstrB'
fromConstrM :: (Monad m, DataEx a)
            => (forall d. DataEx d => m d)
            -> Constr
            -> m a
fromConstrM f = gunfold k z
 where
  k c = do { c' <- c; b <- f; return (c' b) }
  z = return


------------------------------------------------------------------------------
--
--      Datatype and constructor representations (NEW)
--
------------------------------------------------------------------------------

-- | Representation of datatypes (NEW)
-- A package of constructor representations with names of type and module.
-- The new representation is extensible along the lines of the new exceptions
data DataType = 
    forall datarep. (Typeable datarep, Show datarep, Eq datarep, 
		     ReadCtor datarep) =>
    DataType {dataTypeDataRep :: datarep}

instance Show DataType where
    show (DataType rep) = show rep


-- | Representation of constructors (NEW)
-- The new representation is extensible along the lines of the new exceptions
data Constr = forall a. (Typeable a, Eq a, Show a) => Constr a

instance Show Constr where
    show (Constr a) = show (typeOf a) ++ show a

-- | Equality of constructors
instance Eq Constr where
    Constr x == Constr y | Just y' <- cast y = x == y'
    Constr _ == Constr _  = False


-- The following bit from the old Data.hs is no longer required
-- | Public representation of datatypes
-- data DataRep = AlgRep [Constr] | ...


-- The following bit from the old Data.hs is no longer required
-- | Public representation of constructors
-- data ConstrRep = AlgConstr    ConIndex

-- | Unique index for datatype constructors,
-- counting from 1 in the order they are given in the program text.
-- Remains the same as before (Data.hs)
type ConIndex = Int

-- | Fixity of constructors
-- Remains the same
data Fixity = Prefix
            | Infix     -- Later: add associativity and precedence
            deriving (Eq,Show)



------------------------------------------------------------------------------
--
--      Observers for datatype representations (no longer needed)
--
------------------------------------------------------------------------------


-- | Gets the type constructor including the module
-- The following is no longer needed: use show (typeOf a)
-- dataTypeName :: DataType -> String



-- | Gets the public presentation of a datatype
-- The following is no longer needed: DataType ctor is public
-- dataTypeRep :: (Typeable datarep, Show datarep, Eq datarep) => 
--	          DataType -> datarep

-- | Gets the datatype of a constructor
-- The following is no longer needed
-- constrType :: Constr -> DataType


-- | Gets the public presentation of constructors
-- The following is no longer needed
-- constrRep :: (Typeable crep, Show crep, Eq crep) => 
-- 	     Constr -> crep

-- The following bit from the old Data.hs is no longer required
-- | Look up a constructor by its representation
-- repConstr :: DataType -> ConstrRep -> Constr

------------------------------------------------------------------------------
--
--      From strings to constr's and vice versa: all data types (NEW)
--
------------------------------------------------------------------------------


-- | Gets the string for a constructor
-- No need for a separate function: use Show
-- showConstr :: Constr -> String


-- | Lookup a constructor via a string
-- readConstr is now a method of a ReadCtor class
class ReadCtor dtrep where
    readConstr :: dtrep -> String -> Maybe Constr

------------------------------------------------------------------------------
--
--      Representations of algebraic data types (NEW)
--
------------------------------------------------------------------------------

data AlgDataType = AlgDataType [AlgConstr]
   deriving (Typeable, Show, Eq)

-- Constructor for the algebraic data types, closely following the
-- one in Data.hs
data AlgConstr = AlgConstr
                        { cindex    :: ConIndex -- index in the datatype defn
                        , constring :: String
                        , confields :: [String]
                        , confixity :: Fixity
                        }
	       deriving Typeable


instance Show AlgConstr where
    show = constring

instance Eq AlgConstr where
    x == y = cindex x == cindex y


instance ReadCtor AlgDataType where
    readConstr (AlgDataType cons) str = 
	case filter ((==) str . show) cons of
          []    -> Nothing
	  (h:_) -> Just (Constr h)


-- | Constructs a constructor
mkConstr :: AlgDataType -> String -> [String] -> Fixity -> AlgConstr
mkConstr (AlgDataType cs) str fields fix =
        AlgConstr
                { cindex    = idx
                , constring = str
                , confields = fields
                , confixity = fix
                }
  where
    idx = head [ i | (c,i) <- cs `zip` [1..], show c == str ]

-- | Gets the index of a constructor (algebraic datatypes only)
constrIndex :: Constr -> ConIndex
constrIndex (Constr con) = case cast con of
                    Just (AlgConstr{cindex=idx}) -> idx
                    _ -> error "constrIndex"

------------------------------------------------------------------------------
--
--      Representation of primitive types
--
------------------------------------------------------------------------------

data IntDataType = IntDataType
    deriving (Typeable, Eq, Show)
newtype IntConstr = IntConstr Integer
    deriving (Typeable, Eq, Show)


instance ReadCtor IntDataType where
    readConstr _ str = 
	case (reads str) of
	  [(t,"")] -> Just . Constr . IntConstr $ t
          _        -> Nothing


data CharDataType = CharDataType
    deriving (Typeable, Eq, Show)
newtype CharConstr = CharConstr String
    deriving (Typeable, Eq, Show)


instance ReadCtor CharDataType where
    readConstr _ str = 
	case (reads str) of
	  [(t,"")] -> Just . Constr . CharConstr $ [t]
          _        -> Nothing


------------------------------------------------------------------------------
--
--      Instances of the Data class for Prelude-like types.
--      We define top-level definitions for representations.
--
------------------------------------------------------------------------------

instance DataEx Int where
  toConstr = Constr . IntConstr . fromIntegral
  gunfold _ z (Constr c) = case cast c of
                    Just (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = DataType IntDataType

instance DataEx Integer where
  toConstr = Constr . IntConstr
  gunfold _ z (Constr c) = case cast c of
                    Just (IntConstr x) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = DataType IntDataType

-- Int8, Word8, etc. are all similar

instance DataEx Char where
  toConstr = Constr . CharConstr . (:[])
  gunfold _ z (Constr c) = case cast c of
                    Just (CharConstr [x]) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = DataType CharDataType


falseConstr, trueConstr :: AlgConstr
falseConstr  = mkConstr boolDataType "False" [] Prefix
trueConstr   = mkConstr boolDataType "True"  [] Prefix

boolDataType :: AlgDataType
boolDataType = AlgDataType [falseConstr,trueConstr]


instance DataEx Bool where
  toConstr False = Constr falseConstr
  toConstr True  = Constr trueConstr
  gunfold _ z c  = case constrIndex c of
                     1 -> z False
                     2 -> z True
                     _ -> error "gunfold"
  dataTypeOf _ = DataType boolDataType



nothingConstr, justConstr :: AlgConstr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr    = mkConstr maybeDataType "Just"    [] Prefix

maybeDataType :: AlgDataType
maybeDataType = AlgDataType [nothingConstr,justConstr]

instance DataEx a => DataEx (Maybe a) where
  toConstr Nothing  = Constr nothingConstr
  toConstr (Just _) = Constr justConstr
  gunfold k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> k (z Just)
                    _ -> error "gunfold"
  dataTypeOf _ = DataType maybeDataType


nilConstr, consConstr :: AlgConstr
nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr   = mkConstr listDataType "(:)" [] Infix

listDataType :: AlgDataType
listDataType = AlgDataType [nilConstr,consConstr]

instance DataEx a => DataEx [a] where
  toConstr []    = Constr nilConstr
  toConstr (_:_) = Constr consConstr
  gunfold k z c = case constrIndex c of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> error "gunfold"
  dataTypeOf _ = DataType listDataType


tuple2Constr :: AlgConstr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix

tuple2DataType :: AlgDataType
tuple2DataType = AlgDataType [tuple2Constr]

instance (DataEx a, DataEx b) => DataEx (a,b) where
  toConstr _ = Constr tuple2Constr
  gunfold k z c | constrIndex c == 1 = k (k (z (,)))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = DataType tuple2DataType

-- Tests

-- generic ``minimum''
-- (I took a liberty to define 0 as the min Int value, since
-- it prints better)
genMin :: forall a. DataEx a => a
genMin = r
 where
 r = case dataTypeOf r of DataType x -> build . min_ctor $ x
 min_ctor x | Just (AlgDataType (c:_)) <- cast x = Constr c
 min_ctor x | Just IntDataType  <- cast x  = Constr . IntConstr $ 0
 min_ctor x | Just CharDataType <- cast x  = Constr . CharConstr $ " "
 build = fromConstrB genMin


td11 = genMin :: [Int]
-- []
td12 = genMin :: (Int,String)
-- (0,"")

