{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoRec #-}

module Data.YamlObject.Types where

import Data.Map (Map)
import qualified Data.Map as Map (insert, lookup, insertWith)
import Data.DynStableName (DynStableName)
import Control.Monad.State (StateT, get, put, modify)
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import GHC.Generics
import Control.Monad.Reader
import Text.Libyaml (Position, AnchorName, Event)
import Data.Map (Map)
import Data.Dynamic (Dynamic)
import Control.Monad.Error (ErrorT, MonadError, Error(..), throwError)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Applicative (Applicative, Alternative)

import Data.Convertible (convert)
import Data.ConvertibleInstances
import Data.Dynamic (toDyn, fromDynamic, dynTypeRep)
import Data.Typeable (typeOf)
import Data.DynStableName
import System.IO.Unsafe (unsafePerformIO)

{- |
   YamlObject is a representation of general data in a way that's highly
   palatable to YAML, especially the yaml package on Hackage.. It's based on
   Data.Object.Object but extends it with explicit sharing and with annotations
   that can be used to carry things like source position and style information.
-}
data YamlObject ann k v
    = Scalar { annotation :: ann, value :: v }
    | Sequence { annotation :: ann, contents :: [YamlObject ann k v] }
    | Mapping { annotation :: ann, contentPairs :: [(k, YamlObject ann k v)] }
    | Reference { annotation :: ann, anchor :: Anchor }
    -- | The yaml package doesn't see anchors as separate events, so they don't
    -- get per-event information - and hence, no annotations either
    | Anchor { anchor :: Anchor, content :: (YamlObject ann k v) }
      deriving (Eq, Show)

-- | Surrogate anchors are anchors generated in the parsing code, used to
-- implement data merges (i.e. the <<) syntax. Originals are just 
-- YAML anchors.
-- 
-- Note that there's not really any need to parametrise Anchors like
-- YamlObject keys and values, and in any case having surrogate anchors around
-- would complicate it.
data Anchor = Surrogate Int | Original Text deriving (Eq, Ord, Show)

type ToYamlAction = ToYamlM ToYamlObject

-- | The Share class is used for controlling what things get anchors generated
-- | from them. If you are only outputting data that is small compared to the
-- | amount of memory you have to work with, you can try adding Share instances
-- | indiscriminately and going back and making things neater with
-- | cleanUpReferences. Otherwise, whatever object share returns True on will
-- | have an anchor to refer to it in the outputted document (unless replaced
-- | with a reference)
-- | If the instance of share for this type returns True when called,
-- | place an anchor everywhere values of this type are output, and a
-- | reference where the same value is used later. The argument
-- | is safe to evaluate.
class Share a where
    share :: a -> Bool
    share _ = False

instance Share a

-- | Exclusion happens first, translation after that, so exclude based on 
-- untranslated field names
class TranslateField a where
    translateField :: a -> Text -> Text
    translateField _ = id

instance TranslateField a

class Share a => ToYaml a where
    toYaml :: a -> ToYamlAction
    -- | Applies to record types only. You can specialize this method to
    --   prevent certain fields from being serialized.
    --   Given a Haskell field name, it should return False if that field is
    --   to be serialized, and True otherwise.
    --   The first argument is a dummy and must not be evaluated..
    exclude  :: a -> Text -> Bool
    exclude _ _ = False

    default toYaml :: (Generic a, GToYaml (Rep a)) => a -> ToYamlAction
    toYaml x = toCache x $ runReaderT (gToYaml $ from x) (GToYamlDict (exclude :: a -> Text -> Bool) (share :: a -> Bool) (translateField :: a -> Text -> Text))

-- there might be a more elegant way to do this than straight out dictionary passing, but I'm not sure I even want to know what it is
data GToYamlDict a = GToYamlDict {
      excludeD :: a -> Text -> Bool
    , shareD :: a -> Bool
    , translateFieldD :: a -> Text -> Text
}


class GToYaml f where
    gToYaml ::
           f a 
        -> GToYamlAction a

type GToYamlAction a = ReaderT (GToYamlDict a) ToYamlM ToYamlObject

data ToYamlState = ToYamlState {
      nextId :: Int
    , cache :: Map Int [(DynStableName, Int)]
}

-- | currently no annotations for encoding
data ToYamlAnnotation = ToYamlAnnotation () deriving (Show)

type ToYamlObject = YamlObject ToYamlAnnotation Text Text

newtype ToYamlT m a = ToYamlT { unToYamlT :: StateT ToYamlState m a }
    deriving (Monad, Functor)

type ToYamlM = ToYamlT Identity

data FromYamlAnnotation = 
    FromYamlAnnotation { fromYamlPosition :: Position } deriving Show
type FromYamlObject = YamlObject FromYamlAnnotation Text Text

data FromYamlState = FromYamlState { 
      refs :: Map Anchor Dynamic
}
newtype FromYamlT m a = FromYamlT { unFromYamlT :: ErrorT FromYamlException (StateT FromYamlState m) a }
    deriving (Monad, Applicative, Alternative, MonadError FromYamlException, MonadFix, Functor)

instance Error FromYamlException where
    noMsg = OtherException "no message given"
    strMsg s = OtherException s

instance MonadTrans FromYamlT where
    lift m = FromYamlT $ lift (lift m)

type FromYamlM = FromYamlT Identity

data FromYamlException =
    CouldntReadScalar { _position :: Position,
                        _expectedType :: String,
                        _content :: String} |
    MissingMapElement { _position :: Position
                      , _neededElement :: String} |
    InsufficientElements { _position :: Position
                         , _collection :: String } |
    TypeMismatch { _position :: Position
                 , _expectedType :: String
                 , _receivedInstead :: String } |
    ReferenceToNonexistentAnchor { _position :: Position,
                                   _anchor :: String } |
    ReferenceToWrongAnchorType { _positionOfReference :: Position,
--                               _expectedType :: String,  -- not sure if can be
                                                           -- provided at all
                                 _typeOfReferent :: String } |
    ParseException ParseException |
    OtherException { _message :: String }
                   deriving (Show, Typeable, Eq) 
instance Exception FromYamlException

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: AnchorName }
                    | UnexpectedEvent { _pe_received :: Maybe Event
                                      , _pe_expected :: Maybe Event
                                      , _pe_position :: Maybe Position
                                      }
                    | InvalidYaml (Maybe String)
    deriving (Show, Typeable, Eq)
instance Exception ParseException

data GFromYamlDict a = GFromYamlDict {
      toYamlD :: a -> Text -> Bool,
      translateFieldD' :: a -> Text -> Text
}

class Typeable a => FromYaml a where
    fromYaml :: FromYamlObject -> FromYamlM a
    -- | Applies to record types only. You can specialize this method to
    -- allow given fields of a record to not be included in the YAML document.
    -- Be careful if you ever allow exclusion for a strict field. Doing so will
    -- leave not just that field but also the containing constructor undefined.
    -- The first argument is a dummy and must not be evaluated.
    allowExclusion  :: a -> Text -> Bool
    allowExclusion _ _ = False

    default fromYaml :: (Generic a, GFromYaml (Rep a)) => 
                        FromYamlObject -> FromYamlM a
    fromYaml x = fromCache x (\x -> to `fmap` runReaderT (gFromYaml x) (GFromYamlDict (allowExclusion :: a -> Text -> Bool) (translateField :: a -> Text -> Text)))

class GFromYaml f where
    gFromYaml :: FromYamlObject -> GFromYamlM f a

type GFromYamlM f a = ReaderT (GFromYamlDict a) FromYamlM (f a)


-- a bit annoying that these have to be here (it bloats up the import list too),
-- but I didn't want to deal with recursive imports, and these have to be
-- in the default instances

toScalar x = return $ Scalar (ToYamlAnnotation ()) x
toReference x = return $ Reference (ToYamlAnnotation ()) x
toAnchor a x = return $ Anchor a x
toMapping xs = return $ Mapping (ToYamlAnnotation ()) xs
toSequence xs = return $ Sequence (ToYamlAnnotation ()) xs

toCache
  :: (ToYaml a) =>
     a
     -> ToYamlM (YamlObject ToYamlAnnotation k v)
     -> ToYamlM (YamlObject ToYamlAnnotation k v)
toCache x cont =
    ToYamlT get >>= \(ToYamlState index m) ->
    if share x
    then let name = unsafePerformIO $ makeDynStableName $! x
             hash = hashDynStableName name in
         case do vals <- Map.lookup hash m
                 lookup name vals of
           Nothing -> do ToYamlT $ modify (\e -> e {nextId = succ (nextId e),
                                                    cache = Map.insertWith (++) hash [(name, index)] (cache e)})
                         v <- cont
                         toAnchor (Original (convert index)) v
           Just index -> toReference (Original $ convert index)
    else cont


fromCache :: (Typeable b)
             => FromYamlObject
          -> (FromYamlObject -> FromYamlM b)
          -> FromYamlM b
fromCache (Anchor a v) cont = do 
  FromYamlState refs <- FromYamlT get
  rec { FromYamlT $ put (FromYamlState (Map.insert a (toDyn r) refs))
      ; r <- case v of
               Anchor a' v' -> fromCache v cont
               Reference ann a' -> fromCache v cont
               _ -> cont v }
  return r
fromCache (Reference ann a) _ = do
  FromYamlState refs <- FromYamlT get 
  case Map.lookup a refs of
    Nothing -> throwError $ 
               ReferenceToNonexistentAnchor (fromYamlPosition ann) (show a)
    Just v -> case fromDynamic v of
                Nothing -> throwError $ 
                           ReferenceToWrongAnchorType
                           (fromYamlPosition ann)
                           (show $ dynTypeRep v)
                Just r -> return r
fromCache a cont = cont a
