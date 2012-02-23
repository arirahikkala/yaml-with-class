{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.YamlObject.Types where

import Data.Map (Map)
import Data.DynStableName (DynStableName)
import Control.Monad.State (StateT)
import Control.Monad.Identity (Identity)
import Data.Text (Text)
import GHC.Generics

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

class (TranslateField a) => ToYaml a where
    toYaml :: a -> ToYamlAction
    -- | Applies to record types only. You can specialize this method to
    --   prevent certain fields from being serialized.
    --   Given a Haskell field name, it should return False if that field is
    --   to be serialized, and True otherwise.
    exclude  :: a -> Text -> Bool
    exclude _ _ = False

    share :: a -> Bool
    share _ = False

    default toYaml :: (Generic a, GToYaml (Rep a)) => a -> ToYamlAction
    toYaml = gToYaml . from

class GToYaml f where
    gToYaml :: f a -> ToYamlAction


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


class TranslateField a where
    -- | This method defines the mapping from Haskell record field names
    --   to YAML object field names. The default is to strip any initial
    --   underscores. Specialize this method to define a different behavior.
    translateField :: a -> String -> String


