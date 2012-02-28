{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE DoRec #-} 
{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}


-- todo:
-- test if AllowExclusion works
-- add usingCache to all ToYaml instances
-- stacked errors (i.e. "got error x on line a1-b1, within element y on lines a2-b2, within element z on...")
-- write FromYaml instances
-- internalise internal stuff, actually design a proper interface finally
{- | Extremely quick start guide to Data.YamlObject and Text.YamlPickle:

>>> Data.ByteString.putStrLn $ encode $ makeYaml [["hello", "world"], ["yaml here"]]
- - 'hello'
  - 'world'
- - 'yaml here'

-}


module Data.YamlObject (module Data.YamlObject.Types, makeYaml, unmakeYaml, addDummyFromYamlAnnotations)

where

import Data.YamlObject.Types
import Data.YamlObject.Generic
import Data.YamlObject.Support
import Data.YamlObject.Instances

import Data.ConvertibleInstances ()
import Control.Arrow

import System.IO.Unsafe

import Control.Monad.State
import Control.Monad.Trans.Class

import Data.Attempt
import Data.Convertible
import Control.Monad.Error
import Control.Monad.Identity
import Control.Exception

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Array
import Data.Ratio

import qualified Data.HashMap as Hash
import Data.Dynamic

import Data.List (find, findIndex, sortBy)
import Data.Ord (comparing)
import Data.Monoid (getFirst, First(First), mappend)

import qualified Data.Text as T
import Data.Text (Text)

import Data.DynStableName
import Text.Libyaml (Position(..))

import Control.Applicative
import Data.Traversable (traverse)



-- todo: Some kind of a nice tree fold would make this shorter, possibly
-- | Removes unused anchors. Note that this function forces the structure of the
-- YamlObject, so don't use it if you're writing out a lot of data within
-- a constrained memory environment.
cleanUpReferences :: YamlObject ann k v -> YamlObject ann k v
cleanUpReferences o = 
    let usedReferences = execState (countRefs o) (Set.empty)
        countRefs s@(Scalar {}) = return ()
        countRefs (Sequence ann xs) = 
            mapM_ countRefs xs
        countRefs (Mapping ann ps) = do
          mapM_ countRefs $ map snd ps
        countRefs (Anchor a o) = countRefs o
        countRefs r@(Reference ann a) = modify (Set.insert a)

        go refs s@(Scalar {}) = s
        go refs (Sequence ann xs) = Sequence ann $ map (go refs) xs
        go refs (Mapping ann ps) = 
            Mapping ann $ zip (map fst ps) $ map (go refs . snd) ps
        go refs (Anchor a o) 
            | a `Set.member` refs = Anchor a $ go refs o
            | otherwise = go refs o
        go refs s@(Reference {}) = s
    in go usedReferences o



evalToYamlT :: Monad m => ToYamlT m a -> ToYamlState -> m a
evalToYamlT (ToYamlT a) s = evalStateT a s

runToYamlT (ToYamlT a) s = runStateT a s

makeYaml :: ToYaml a => 
            a ->
            ToYamlObject
makeYaml x = runIdentity $ evalToYamlT (toYaml x) (ToYamlState 0 Map.empty)

evalFromYamlT (FromYamlT a) s = evalStateT (runErrorT a) s
runFromYamlT (FromYamlT a) s = runStateT (runErrorT a) s

unmakeYaml :: FromYaml a => FromYamlObject -> Either FromYamlException a
unmakeYaml a = runIdentity $ evalFromYamlT (fromYaml a) $ FromYamlState Map.empty


mapAnnotations f (Scalar ann a) = Scalar (f ann) a
mapAnnotations f (Sequence ann ss) = 
    Sequence (f ann) $ map (mapAnnotations f) ss
mapAnnotations f (Mapping ann ps) =
    Mapping (f ann) $ zip (map fst ps) $ map (mapAnnotations f) $ map snd ps
mapAnnotations f (Reference ann a) = Reference (f ann) a
mapAnnotations f (Anchor a c) = Anchor a $ mapAnnotations f c

addDummyFromYamlAnnotations :: 
    YamlObject a k v -> YamlObject FromYamlAnnotation k v

addDummyFromYamlAnnotations = mapAnnotations (const $ FromYamlAnnotation $ Position 0 0 0 0)


