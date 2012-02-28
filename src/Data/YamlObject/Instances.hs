{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.YamlObject.Instances where

import Data.YamlObject.Types
import Data.YamlObject.Support
import Data.ConvertibleInstances

import Data.Convertible (convert)
import Data.Typeable
import qualified Data.Text as T
import Data.Array (Array, Ix, elems, bounds, listArray)
import Data.Ratio (Ratio, numerator, denominator)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Error (throwError)
import Data.List (sortBy, findIndex)
import Data.Ord (comparing)

instance ToYaml Bool where
    toYaml True = toScalar $ T.pack "true"
    toYaml False = toScalar $ T.pack "false"

instance ToYaml Int where
    toYaml x = toCache x . toScalar . convert $ x

instance ToYaml Integer where
    toYaml x = toCache x . toScalar . convert $ x
instance ToYaml Float where
    toYaml x = toCache x . toScalar . convert $ x
instance ToYaml Double where
    toYaml x = toCache x . toScalar . convert $ x


-- This way seems the most reasonable. Falling back on the generic instance would just output 'Scalar ":%"', and dividing into a float would lose precision (and if you were using Rational you might actually have been using it)
instance (Integral a, Show a, Typeable a) => ToYaml (Ratio a) where
    toYaml i = toCache i $ do
      num <- toScalar $ convert $ numerator i
      denom <- toScalar $ convert $ denominator i
      toMapping [(T.pack "numerator", num), (T.pack "denominator", denom)]

instance ToYaml Char where
    toYaml c = toCache c $ toScalar $ T.singleton c

instance ToYaml [Char] where
    toYaml c = toCache c $ toScalar $ T.pack c

instance (Typeable a, ToYaml a) => ToYaml (Maybe a) where
    toYaml x = toCache x $ 
               case x of
                 (Just c) -> do inside <- toYaml c
                                toMapping [(T.pack "just", inside)]
                 Nothing -> toMapping []

instance (ToYaml a, Show k, Typeable a, Typeable k)
         => ToYaml (Map k a) where
    toYaml x = toCache x $
        do vals <- mapM toYaml $ Map.elems x
           toMapping $ zip (map convert $ Map.keys x) vals

instance (ToYaml a, Typeable a) => ToYaml (Set a) where
    toYaml a = toCache a $
        do vals <- mapM toYaml $ Set.elems a
           toSequence vals
 
instance (ToYaml a, Typeable a) => ToYaml [a] where
    toYaml xs = toCache xs $
                (toSequence =<< mapM toYaml xs)

-- todo: Generic array instance. I actually had one earlier, but it was causing weird type errors in some client code. Will have to figure out what the hell was going on with that.
instance (Ix i, ToYaml e, Typeable e, Typeable i, ToYaml i) => ToYaml (Array i e) where
    toYaml a = toCache a $
        do bs <- toYaml $ bounds a
           es <- toYaml $ elems a
           toMapping [(T.pack "bounds", bs), (T.pack "elems", es)]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b) => ToYaml (a, b) where
    toYaml x@(a, b) = toCache x $
        do ar <- toYaml a
           br <- toYaml b
           toSequence [ar, br]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b, ToYaml c, Typeable c) => ToYaml (a, b, c) where
    toYaml x@(a, b, c) = toCache x $
        do ar <- toYaml a
           br <- toYaml b
           cr <- toYaml c
           toSequence [ar, br, cr]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b, ToYaml c, Typeable c, ToYaml d, Typeable d) => ToYaml (a, b, c, d) where
    toYaml x@(a, b, c, d) = toCache x $
        do ar <- toYaml a
           br <- toYaml b
           cr <- toYaml c
           dr <- toYaml d
           toSequence [ar, br, cr, dr]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b, ToYaml c, Typeable c, ToYaml d, Typeable d, ToYaml e, Typeable e) => ToYaml (a, b, c, d, e) where
    toYaml x@(a, b, c, d, e) = toCache x $
        do ar <- toYaml a
           br <- toYaml b
           cr <- toYaml c
           dr <- toYaml d
           er <- toYaml e
           toSequence [ar, br, cr, dr, er]




instance FromYaml Int where
    fromYaml v = fromCache v scalarFromYamlAttempt

instance FromYaml Bool where
    fromYaml v = fromCache v go where
                       go (Scalar ann c)
                           | T.unpack c == "true" = return True
                           | T.unpack c == "false" = return False
                           | otherwise = typeMismatch "Bool" v

instance FromYaml Float where
    fromYaml v = fromCache v scalarFromYamlAttempt
instance FromYaml Double where
    fromYaml v = fromCache v scalarFromYamlAttempt

instance FromYaml Char where
    fromYaml v = fromCache v go where
                       err ann s = throwError $ TypeMismatch (fromYamlPosition ann) "Char" s
                       go (Sequence ann xs) = err ann "Sequence"
                       go (Mapping ann xs) = err ann "Mapping"
                       go (Scalar ann c) = if T.length c == 1 then return (T.head c) else err ann "Scalar (wanted one of precisely one character)"

instance FromYaml [Char] where
    fromYaml v = fromCache v scalarFromYamlAttempt

instance (Typeable a, FromYaml a) => FromYaml [a] where
    fromYaml v = fromCache v go
                 where 
                   err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("[" ++ show (typeOf (undefined :: a)) ++ "]") s

                   go (Sequence ann xs) = mapM fromYaml xs
                   go (Mapping ann _) = err ann "Mapping"
                   go (Scalar ann _) = err ann "Scalar"

instance (Typeable a, FromYaml a) => FromYaml (Maybe a) where
    fromYaml v = fromCache v go
                 where
                   err ann s = throwError $ TypeMismatch (fromYamlPosition ann) 
                               (show $ typeOf (undefined :: Maybe a))
                                s
                   go (Mapping ann ((key, val):[])) 
                       | key == T.pack "just" = Just `fmap` fromYaml val
                       | otherwise = err ann "Mapping containing something other than only the key \"just\""
                   go (Mapping ann []) = return Nothing
                   go (Mapping ann _) = err ann "Mapping (containing something other than only the key \"just\")"
                   go (Sequence ann xs) = err ann "Sequence"
                   go (Scalar ann _) = err ann "Scalar"

instance (Typeable a, FromYaml a, FromYaml k, Typeable k, Read k, Ord k) => FromYaml (Map k a) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) 
                    ("Map (" ++ show (typeOf (undefined :: k)) ++ ") (" ++ show (typeOf (undefined :: a)))
                    s
        go (Sequence ann _) = err ann "Sequence"
        go (Scalar ann _) = err ann "Scalar"

        go (Mapping ann ps) = 
            let keysAttempts :: [[(k, String)]]
                keysAttempts = map (reads . (convert :: T.Text -> String) . fst) ps in
            case findIndex null keysAttempts of
                Just i -> throwError $ CouldntReadScalar (fromYamlPosition ann) (show (typeOf (undefined :: k))) (convert . fst $ ps !! i)
                Nothing -> do vs <- mapM (fromYaml . snd) ps
                              return $ Map.fromList $ zip (map (fst . head) keysAttempts) vs


instance (Typeable a, FromYaml a, Ord a) => FromYaml (Set a) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("Set (" ++ show (typeOf (undefined :: a)) ++ ")") s
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann ss) = do
          rss <- mapM fromYaml ss
          return $ Set.fromList rss

instance (Ix i, FromYaml i, FromYaml e, Typeable i, Typeable e) => FromYaml (Array i e) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("Array (" ++
                                                                               show (typeOf (undefined :: i)) ++ ") (" ++
                                                                               show (typeOf (undefined :: e)) ++ ")") s
        go (Scalar ann _) = err ann "Scalar"
        go (Sequence ann _) = err ann "Sequence"
        go (Mapping ann xs@(_:_:[])) = correctLength ann (sortBy (comparing fst) xs)
        go (Mapping ann _) = err ann "Mapping with a different number of elements than 2"
        correctLength ann ((bsKey,bs):(esKey,es):[])
                | bsKey /= T.pack "bounds" = err ann "Mapping whose first key (after sorting) is something other than \"bounds\""
                | esKey /= T.pack "elems" = err ann "Mapping whose second key (after sorting) is something other than \"elems\""
                | otherwise = do rbs <- fromYaml bs
                                 res <- fromYaml es
                                 return $ listArray rbs res

instance (FromYaml a, Typeable a, FromYaml b, Typeable b) => FromYaml (a, b) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: b)) ++ ")") s
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann (a:b:[])) =
            do ra <- fromYaml a
               rb <- fromYaml b
               return $ (ra, rb)
        go (Sequence ann _) = err ann "Sequence of length other than 2"


instance (FromYaml a, Typeable a, FromYaml b, Typeable b, FromYaml c, Typeable c) => FromYaml (a, b, c) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: b)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: c)) ++ ")") s 
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann (a:b:c:[])) =
            do ra <- fromYaml a
               rb <- fromYaml b
               rc <- fromYaml c
               return $ (ra, rb, rc)
        go (Sequence ann _) = err ann "Sequence of length other than 3"


instance (FromYaml a, Typeable a, FromYaml b, Typeable b, FromYaml c, Typeable c, FromYaml d, Typeable d) => FromYaml (a, b, c, d) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: b)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: c)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: d)) ++ ")") s 
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann (a:b:c:d:[])) =
            do ra <- fromYaml a
               rb <- fromYaml b
               rc <- fromYaml c
               rd <- fromYaml d
               return $ (ra, rb, rc, rd)
        go (Sequence ann _) = err ann "Sequence of length other than 4"

instance (FromYaml a, Typeable a, FromYaml b, Typeable b, FromYaml c, Typeable c, FromYaml d, Typeable d, FromYaml e, Typeable e) => FromYaml (a, b, c, d, e) where
    fromYaml v = fromCache v go where
        err ann s = throwError $ TypeMismatch (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: b)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: c)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: d)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: e)) ++ ")") s 
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann (a:b:c:d:e:[])) =
            do ra <- fromYaml a
               rb <- fromYaml b
               rc <- fromYaml c
               rd <- fromYaml d
               re <- fromYaml e
               return $ (ra, rb, rc, rd, re)
        go (Sequence ann _) = err ann "Sequence of length other than 5"
