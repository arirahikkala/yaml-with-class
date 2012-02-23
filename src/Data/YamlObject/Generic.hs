{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- full disclosure: This file is basically a straight monadization of https://github.com/bos/aeson/blob/940449f4adc804fda4597cab4c25eae503e598ac/Data/Aeson/Types/Generic.hs (that being the most recent version that didn't scare me by doing crazy stuff with mutable vectors) and as such all credit for having actual programming skill goes straight to Bryan O. Sullivan and Bas Van Dijk.

module Data.YamlObject.Generic where

import GHC.Generics
import Data.YamlObject.Types
import Data.YamlObject.Support
import Data.DList (DList, toList, singleton)
import Data.Monoid (mappend)
import Control.Monad (return, liftM)
import Data.Text (Text, pack)

instance (ToYaml a) => GToYaml (K1 i a) where
    gToYaml (K1 x) = cToYaml x
    {-# INLINE gToYaml #-}

instance (GToYaml a) => GToYaml (M1 i c a) where
    gToYaml (M1 x) = gToYaml x
    {-# INLINE gToYaml #-}

instance GToYaml U1 where
    gToYaml _ = toSequence []
    {-# INLINE gToYaml #-}

instance (GProductToValues a, GProductToValues b) => GToYaml (a :*: b) where
    gToYaml x = 
        do objs <- sequence . toList . gProductToValues $ x
           toSequence objs
    {-# INLINE gToYaml #-}

instance (GObject a, GObject b) => GToYaml (a :+: b) where
    gToYaml (L1 x) = gObject x
    gToYaml (R1 x) = gObject x
    {-# INLINE gToYaml #-}

----

class ConsToYaml f where consToYaml :: f a -> ToYamlAction
class ConsToYaml' b f where consToYaml' :: Tagged b (f a -> ToYamlAction)

newtype Tagged s b = Tagged {unTagged :: b}

instance (IsRecord f b, ConsToYaml' b f) => ConsToYaml f where
    consToYaml = unTagged
                 (consToYaml' :: Tagged b (f a -> ToYamlAction))
--    {-# INLINE consToYaml #-}

instance (GRecordToPairs f) => ConsToYaml' True f where
    consToYaml' = Tagged (\x -> do r <- gRecordToPairs x
                                   toMapping $ toList r)
    {-# INLINE consToYaml' #-}

instance GToYaml f => ConsToYaml' False f where
    consToYaml' = Tagged gToYaml
    {-# INLINE consToYaml' #-}

----

class GRecordToPairs f where
    gRecordToPairs :: f a -> ToYamlM (DList (Text, ToYamlObject))

instance (GRecordToPairs a, GRecordToPairs b) => GRecordToPairs (a :*: b) where
    gRecordToPairs (a :*: b) = 
        do ra <- gRecordToPairs a
           rb <- gRecordToPairs b
           return (ra `mappend` rb)
    {-# INLINE gRecordToPairs #-}

instance (Selector s, GToYaml a) => GRecordToPairs (S1 s a) where
    gRecordToPairs m1 = 
        do obj <- gToYaml (unM1 m1)
           return $ singleton (pack (selName m1), obj)
    {-# INLINE gRecordToPairs #-}

----

class GProductToValues f where
    gProductToValues :: f a -> DList ToYamlAction

instance (GProductToValues a, GProductToValues b) => GProductToValues (a :*: b) where
    gProductToValues (a :*: b) = gProductToValues a `mappend` gProductToValues b
    {-# INLINE gProductToValues #-}

instance (GToYaml a) => GProductToValues a where
    gProductToValues = return . gToYaml
    {-# INLINE gProductToValues #-}

---

class GObject f where
    gObject :: f a -> ToYamlAction

instance (GObject a, GObject b) => GObject (a :+: b) where
    gObject (L1 x) = gObject x
    gObject (R1 x) = gObject x
    {-# INLINE gObject #-}

instance (Constructor c, GToYaml a, ConsToYaml a) => GObject (C1 c a) where
    gObject (M1 o) = do
      obj <- gToYaml o
      toMapping [(pack $ conName (undefined :: t c a p), obj)]
    {-# INLINE gObject #-}

--

class IsRecord (f :: * -> *) b | f -> b

data True
data False

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False

