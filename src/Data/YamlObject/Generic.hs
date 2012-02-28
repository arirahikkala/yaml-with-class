{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

-- todo: Go over everything in this file, make sure that everything is regular (this should mean that everything should be incredibly simple), so as to get stuff to actually work

-- full disclosure: This file is basically a straight monadization of https://github.com/bos/aeson/blob/940449f4adc804fda4597cab4c25eae503e598ac/Data/Aeson/Types/Generic.hs (that being the most recent version that didn't scare me by doing crazy stuff with mutable vectors) and as such all credit for having actual programming skill goes straight to Bryan O. Sullivan and Bas Van Dijk.

module Data.YamlObject.Generic where

import GHC.Generics
import Data.YamlObject.Types
import Data.YamlObject.Support
import Data.DList (DList, toList, singleton, empty)
import Data.Monoid (mappend)
import Control.Monad (return, liftM)
import Data.Text (Text, pack, unpack)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Error (throwError)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (get, put)
import Data.Dynamic
import Control.Monad.Fix
import Control.Arrow
import Text.Libyaml (Position(..))
import Debug.Trace (trace)

instance (ToYaml a) => GToYaml (K1 i a) where
    gToYaml (K1 x) = lift $ toYaml x
    {-# INLINE gToYaml #-}

instance (GToYaml a) => GToYaml (M1 D c a) where
    gToYaml (M1 x) = gToYaml x
    {-# INLINE gToYaml #-}

instance (GToYaml a) => GToYaml (M1 S c a) where
    gToYaml (M1 x) = gToYaml x
    {-# INLINE gToYaml #-}

instance GToYaml U1 where
    gToYaml _ = toSequence []
    {-# INLINE gToYaml #-}

instance (GObject (M1 C c a), Constructor c, GToYaml a) => GToYaml (M1 C c a) where
    gToYaml = gObject

instance (GProductToValues a, GProductToValues b) => GToYaml (a :*: b) where
    gToYaml x =
        do objs <- sequence . toList . gProductToValues $ x
           toSequence objs
    {-# INLINE gToYaml #-}

instance (GObject a, GObject b) => GToYaml (a :+: b) where
    gToYaml (L1 x) = gObject x
    gToYaml (R1 x) = gObject x
    {-# INLINE gToYaml #-}

---

class ConsToYaml f where consToYaml :: f a -> GToYamlAction a
class ConsToYaml' b f where consToYaml' :: Tagged b (f a -> GToYamlAction a)

newtype Tagged s b = Tagged {unTagged :: b}

instance (IsRecord f b, ConsToYaml' b f) => ConsToYaml f where
    consToYaml = unTagged
                 (consToYaml' :: Tagged b (f a -> GToYamlAction a))
--    {-# INLINE consToYaml #-}

instance (GRecordToPairs f) => ConsToYaml' True f where
    consToYaml' = Tagged (\x -> 
        do r <- gRecordToPairs x
           toMapping $ toList r)
    {-# INLINE consToYaml' #-}

instance GToYaml f => ConsToYaml' False f where
    consToYaml' = Tagged gToYaml
    {-# INLINE consToYaml' #-}

----

class GRecordToPairs f where
    gRecordToPairs :: f a -> ReaderT (GToYamlDict a) ToYamlM (DList (Text, ToYamlObject))

instance (GRecordToPairs a, GRecordToPairs b) => GRecordToPairs (a :*: b) where
    gRecordToPairs (a :*: b) = 
        do ra <- gRecordToPairs a
           rb <- gRecordToPairs b
           return (ra `mappend` rb)
    {-# INLINE gRecordToPairs #-}

instance (Selector s, GToYaml a) => GRecordToPairs (S1 s a) where
    gRecordToPairs m1 =
        do obj <- gToYaml (unM1 m1)
           dict <- ask
           let name = pack $ selName m1
           if excludeD dict (error "exclude evaluated its first argument") name
             then return empty
             else return $ singleton (translateFieldD dict undefined name, obj)
    {-# INLINE gRecordToPairs #-}

----

class GProductToValues f where
    gProductToValues :: f a -> DList (GToYamlAction a)

instance (GProductToValues a, GProductToValues b) => GProductToValues (a :*: b) where
    gProductToValues (a :*: b) = gProductToValues a `mappend` gProductToValues b
    {-# INLINE gProductToValues #-}

instance (GToYaml a) => GProductToValues a where
    gProductToValues = singleton . gToYaml
    {-# INLINE gProductToValues #-}

---

class GObject f where
    gObject :: f a -> GToYamlAction a

instance (GObject a, GObject b) => GObject (a :+: b) where
    gObject (L1 x) = gObject x
    gObject (R1 x) = gObject x
    {-# INLINE gObject #-}

instance (Constructor c, GToYaml a, ConsToYaml a) => GObject (C1 c a) where
    gObject (M1 o) = do
      obj <- consToYaml o
      toMapping [(pack $ conName (undefined :: t c a p), obj)]
    {-# INLINE gObject #-}

-- nullary constructors become strings
instance (Constructor c) => GObject (C1 c U1) where
    gObject =
      toScalar . pack . conName
    {-# INLINE gObject #-}

---
--- generic fromYaml

instance (GFromYaml a) => GFromYaml (M1 i c a) where
    gFromYaml = fmap M1 . gFromYaml

instance (FromYaml a) => GFromYaml (K1 i a) where
    gFromYaml x =
        do r <- lift $ fromYaml x
           return $ K1 r

instance GFromYaml U1 where
    gFromYaml (Sequence _ []) = return U1
    gFromYaml v = typeMismatch "unit constructor (U1)" v

instance (ConsFromYaml a) => GFromYaml (C1 c a) where
--    gFromYaml v | trace ("C1 instance: " ++ show v) False = undefined
    gFromYaml (Mapping ann [(key, value)]) = M1 `fmap` (consParseYaml $ value)
-- todo: actually check the constructor name

instance (GFromProduct a, GFromProduct b) => GFromYaml (a :*: b) where
--    gFromYaml v | trace ("(:*:) instance: " ++ show v) False = undefined
    gFromYaml (Sequence ann xs) = gParseProduct xs
    gFromYaml v = typeMismatch "product (:*:)" v
    {-# INLINE gFromYaml #-}

instance (GFromSum a, GFromSum b) => GFromYaml (a :+: b) where
--    gFromYaml v | trace ("(:+:) instance: " ++ show v) False = undefined
    gFromYaml (Mapping ann ([keyVal@(constr, content)])) =
        gParseSum (unpack constr, content)
    gFromYaml (Scalar ann constr) =
        gParseSum (unpack constr, error "gFromYaml: tried to find value inside nullary constructor")
    gFromYaml v = typeMismatch "sum (:+:)" v
    {-# INLINE gFromYaml #-}

---

class GFromRecord f where
    gParseRecord :: FromYamlObject -> GFromYamlM f a

instance (GFromRecord a, GFromRecord b) => GFromRecord (a :*: b) where
    gParseRecord obj = (:*:) <$> gParseRecord obj <*> gParseRecord obj
    {-# INLINE gParseRecord #-}

instance (Selector s, GFromYaml a) => GFromRecord (S1 s a) where
    gParseRecord (Mapping ann ps) =
        let tkey = pack key in
        ask >>= \dict -> 
        case lookup (translateFieldD' dict undefined $ tkey) $ ps of
          Nothing -> if allowExclusionD dict undefined tkey
                     then error "exclusion allowed and no value provided"
                     else throwError $ MissingMapElement (fromYamlPosition ann)
                              (unpack $ translateFieldD' dict undefined $ pack $ key)
          Just k -> gFromYaml k
        where
          key = selName (undefined :: t s a p)
    gParseRecord v = typeMismatch "field selector" v
    {-# INLINE gParseRecord #-}

---

class GFromSum f where
    gParseSum :: (String, FromYamlObject) -> GFromYamlM f a

instance (GFromSum a, GFromSum b) => GFromSum (a :+: b) where
    gParseSum keyVal = (L1 <$> gParseSum keyVal) <|> (R1 <$> gParseSum keyVal)
    {-# INLINE gParseSum #-}

instance (Constructor c, GFromYaml a, ConsFromYaml a) => GFromSum (C1 c a) where
    gParseSum (key, value)
        | key == constructor = M1 `fmap` consParseYaml value
        | otherwise = throwError $ TypeMismatch (Position (-1) (-1) (-1) (-1) {-fromYamlPosition $ annotation value-}) (constructor ++ " (constructor)") key
        where  constructor = conName (undefined :: t c a p)
    {-# INLINE gParseSum #-}

instance (Constructor c) => GFromSum (C1 c U1) where
    gParseSum (key, _)
        | key == constructor = return $ M1 U1
        | otherwise = fail "could not find constructor" -- todo: proper error reporting
        where constructor = conName (undefined :: t c U1 p)

--

class ConsFromYaml f where consParseYaml :: FromYamlObject -> GFromYamlM f a
class ConsFromYaml' b f where consParseYaml' :: Tagged b (FromYamlObject -> GFromYamlM f a)

instance (IsRecord f b, ConsFromYaml' b f) => ConsFromYaml f where
    consParseYaml = unTagged (consParseYaml' :: Tagged b (FromYamlObject -> GFromYamlM f a))
    {-# INLINE consParseYaml #-}

instance (GFromRecord f) => ConsFromYaml' True f where
    consParseYaml' = Tagged gParseRecord
        where
          parseRecord o@(Mapping ann [(constr, obj)]) = gParseRecord obj
          parseRecord v = typeMismatch "record (:*:)" v
    {-# INLINE consParseYaml' #-}

instance (GFromYaml f) => ConsFromYaml' False f where
    consParseYaml' = Tagged gFromYaml
        where go (Mapping ann [(constr, obj)]) = gFromYaml obj
              go v = typeMismatch "product (:*:)" v
    {-# INLINE consParseYaml' #-}
--

class GFromProduct f where
    gParseProduct :: [FromYamlObject] -> GFromYamlM f a

instance (GFromProduct a, GFromProduct b) => GFromProduct (a :*: b) where
    gParseProduct arr = (:*:) <$> gParseProduct arrL <*> gParseProduct arrR
        where
          (arrL, arrR) = splitAt (length arr `div` 2) arr
    {-# INLINE gParseProduct #-}

instance (GFromYaml a) => GFromProduct a where
    gParseProduct ([v]) = gFromYaml v
    gParseProduct _ = throwError $ OtherException "Product too small" -- todo: Better error
    {-# INLINE gParseProduct #-}

data True
data False

class IsRecord (f :: * -> *) b | f -> b

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False
