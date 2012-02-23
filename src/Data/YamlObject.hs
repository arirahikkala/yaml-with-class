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


module Data.YamlObject {- (
-- * Simple use
        makeYaml, ToYamlObject, unmakeYaml, FromYamlObject, FromYamlException (..), TranslateField(..), 
-- * Support for ephemeral data
        AllowExclusion(..),
-- * Sharing
        cleanUpReferences,
-- * Specialized instances 
        ToYaml (..), FromYaml (..), ToYamlM, FromYamlM, YamlObject (..), Anchor (..), ToYamlAnnotation(..), FromYamlAnnotation(..), mapKeysValues, mapKeysValuesA, mapKeysValuesM) -} where

import Data.YamlObject.Types
import Data.YamlObject.Generic
import Data.YamlObject.Support

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


-- | Apply a conversion to both the keys and values of an 'YamlObject'.
mapKeysValues :: (kIn -> kOut)
              -> (vIn -> vOut)
              -> YamlObject ann kIn vIn
              -> YamlObject ann kOut vOut
mapKeysValues _ fv (Scalar ann v) = Scalar ann $ fv v
mapKeysValues fk fv (Sequence ann os)= 
    Sequence ann $ map (mapKeysValues fk fv) os
mapKeysValues fk fv (Mapping ann pairs) =
    Mapping ann $ map (fk *** mapKeysValues fk fv) pairs
mapKeysValues fk fv (Reference ann s) = Reference ann s
mapKeysValues fk fv (Anchor s v) = Anchor s $ mapKeysValues fk fv v

mapKeysValuesA :: Applicative f 
                  => (kIn -> f kOut)
               -> (vIn -> f vOut)
               -> YamlObject ann kIn vIn
               -> f (YamlObject ann kOut vOut)
mapKeysValuesA _ fv (Scalar ann v) = Scalar ann <$> fv v
mapKeysValuesA fk fv (Sequence ann os)= 
    Sequence ann <$> traverse (mapKeysValuesA fk fv) os
mapKeysValuesA fk fv (Mapping ann pairs) =
    Mapping ann <$> 
    traverse (uncurry (liftA2 (,)) .  (fk *** mapKeysValuesA fk fv)) pairs
mapKeysValuesA fk fv (Reference ann s) = pure $ Reference ann s
mapKeysValuesA fk fv (Anchor s v) = Anchor s <$> mapKeysValuesA fk fv v

mapKeysValuesM :: Monad m =>
                 (kIn -> m kOut)
              -> (vIn -> m vOut)
              -> YamlObject ann kIn vIn
              -> m (YamlObject ann kOut vOut)
mapKeysValuesM fk fv =
    let fk' = WrapMonad . fk
        fv' = WrapMonad . fv
     in unwrapMonad . mapKeysValuesA fk' fv'


instance TranslateField a where
    translateField _ x = x

-- | Removes initial underscores from a string.
stripInitialUnderscores = T.dropWhile (=='_')

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

--
-- ToYaml class plus SYB boilerplate. Copied from RJson and modified.
--
-- | New instances can be added to this class to customize certain aspects
--   of the way in which Haskell types are serialized to JSON.




instance ToYaml Int where
    toYaml x = {-usingCache x-} (toScalar . convert $ x)

makeYaml :: ToYaml a => 
            a ->
            ToYamlObject
makeYaml x = runIdentity $ evalToYamlT (toYaml x) (ToYamlState 0 Map.empty)


{-
genericToYaml x
    | isAlgType (dataTypeOf toYamlProxy x) =
        usingCache x $
        withConstructorName x $
        case getFields x of
          [] ->
              do rs <- sequence $ gmapQ toYamlProxy (toYamlD dict) x
                 toSequence rs
          fs ->
              let
                translatedFsToInclude =
                  {-map (T.pack . translateFieldD' dict x)-} (filter (not . (excludeD dict x)) (getFields x))
              in do rs <- sequence $ gmapQ toYamlProxy (toYamlD dict) x
                    toMapping $ zip (map T.pack $ (getFields x)) rs
    | otherwise =
        error $ "Unable to serialize the primitive type '" ++ typename x ++ "'"
-}

-- Implementations of toYaml for different data types.
--
{-
instance ToYaml Bool where
    toYaml True = return $ Scalar $ T.pack "true"
    toYaml False = return $ Scalar $ T.pack "false"
-}
{-





instance ToYaml Integer where
    toYaml x = usingCache x . toScalar . cs . show $ x
instance ToYaml Float where
    toYaml x = usingCache x . toScalar . cs . show $ x
instance ToYaml Double where
    toYaml x = usingCache x . toScalar . cs . show $ x


-- This way seems the most reasonable. Falling back on the generic instance would just output 'Scalar ":%"', and dividing into a float would lose precision (and if you were using Rational you might actually have been using it)
instance (Integral a, Show a, Typeable a) => ToYaml (Ratio a) where
    toYaml i = do
      num <- toScalar $ cs $ show $ numerator i
      denom <- toScalar $ cs $ show $ denominator i
      toMapping [(T.pack "numerator", num), (T.pack "denominator", denom)]

instance ToYaml Char where
    toYaml c = toScalar $ T.singleton c


instance ToYaml [Char] where
    toYaml c = toScalar $ T.pack c

instance (Typeable a, ToYaml a) => ToYaml (Maybe a) where
    toYaml x = case x of
                 (Just c) -> do inside <- toYaml c
                                toMapping [(T.pack "just", inside)]
                 Nothing -> toMapping []

instance (ToYaml a, Show k, Typeable a, Typeable k)
         => ToYaml (Map k a) where
    toYaml x =
        do vals <- mapM toYaml $ Map.elems x
           toMapping $ zip (map (cs . show) $ Map.keys x) vals

instance (ToYaml a, Typeable a) => ToYaml (Set a) where
    toYaml a =
        do vals <- mapM toYaml $ Set.elems a
           toSequence vals
 
instance (ToYaml a, Typeable a) => ToYaml [a] where
    toYaml xs = toSequence =<< mapM toYaml xs

-- todo: Generic array instance. I actually had one earlier, but it was causing weird type errors in some client code. Will have to figure out what the hell was going on with that.
instance (Ix i, ToYaml e, Typeable e, Typeable i, ToYaml i) => ToYaml (Array i e) where
    toYaml a =
        do bs <- toYaml $ bounds a
           es <- toYaml $ elems a
           toMapping [(T.pack "bounds", bs), (T.pack "elems", es)]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b) => ToYaml (a, b) where
    toYaml (a, b) =
        do ar <- toYaml a
           br <- toYaml b
           toSequence [ar, br]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b, ToYaml c, Typeable c) => ToYaml (a, b, c) where
    toYaml (a, b, c) =
        do ar <- toYaml a
           br <- toYaml b
           cr <- toYaml c
           toSequence [ar, br, cr]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b, ToYaml c, Typeable c, ToYaml d, Typeable d) => ToYaml (a, b, c, d) where
    toYaml (a, b, c, d) =
        do ar <- toYaml a
           br <- toYaml b
           cr <- toYaml c
           dr <- toYaml d
           toSequence [ar, br, cr, dr]

instance (ToYaml a, Typeable a, ToYaml b, Typeable b, ToYaml c, Typeable c, ToYaml d, Typeable d, ToYaml e, Typeable e) => ToYaml (a, b, c, d, e) where
    toYaml (a, b, c, d, e) =
        do ar <- toYaml a
           br <- toYaml b
           cr <- toYaml c
           dr <- toYaml d
           er <- toYaml e
           toSequence [ar, br, cr, dr, er]

instance (Data ToYamlD t, TranslateField t, DoShare t) => ToYaml t where
    toYaml x = genericToYaml x

getFields :: Data ToYamlD a => a -> [String]
getFields = constrFields . (toConstr toYamlProxy)

typename x = dataTypeName (dataTypeOf toYamlProxy x)

genericToYaml :: forall a.
                 (Data ToYamlD a, TranslateField a, DoShare a) => 
                 a
              -> ToYamlM (YamlObject ToYamlAnnotation Text Text)

genericToYaml x
    | isAlgType (dataTypeOf toYamlProxy x) =
        usingCache x $
        withConstructorName x $
        case getFields x of
          [] ->
              do rs <- sequence $ gmapQ toYamlProxy (toYamlD dict) x
                 toSequence rs
          fs ->
              let
                translatedFsToInclude =
                  {-map (T.pack . translateFieldD' dict x)-} (filter (not . (excludeD dict x)) (getFields x))
              in do rs <- sequence $ gmapQ toYamlProxy (toYamlD dict) x
                    toMapping $ zip (map T.pack $ (getFields x)) rs
    | otherwise =
        error $ "Unable to serialize the primitive type '" ++ typename x ++ "'"

withConstructorName x cont =
    case gmapQ toYamlProxy (const ()) x of
      [] -> toScalar $ T.pack $ constring $ toConstr toYamlProxy x -- nullary constructor, just output the name
      _ -> do c <- cont
              toMapping [(T.pack $ constring $ toConstr toYamlProxy x, c)]

{- | 
   Convert general Haskell values into intermediate YAML document objects which
   can be encoded into streams of YAML text using 'Text.YamlPickle.encode'.

-}
-}

{-
-- | annotation type for parsing YAML, recording source positions
data FromYamlAnnotation = 
    FromYamlAnnotation { fromYamlPosition :: Position } deriving Show
type FromYamlObject = YamlObject FromYamlAnnotation Text Text

data FromYamlState = FromYamlState { 
      refs :: Map.Map Anchor Dynamic
}
newtype FromYamlT m a = FromYamlT { unFromYamlT :: ErrorT FromYamlException (StateT FromYamlState m) a }
    deriving (Monad, MonadError FromYamlException, MonadFix, Functor)

data FromYamlException = 
    UnexpectedElementType { _position :: Position
                          , _expectedType :: String
                          , _receivedType :: String} |
    CouldntReadScalar { _position :: Position,
                        _expectedType :: String,
                        _content :: String} |
    MissingMapElement { _position :: Position
                      , _collection :: String
                      , _neededElement :: String} |
    InsufficientElements { _position :: Position
                         , _collection :: String } |
    OtherFromYamlException { _errorText :: String }
                      deriving (Show, Typeable)

instance Control.Monad.Error.Error FromYamlException where
    noMsg = OtherFromYamlException ""
    strMsg = OtherFromYamlException


evalFromYamlT (FromYamlT a) s = evalStateT (runErrorT a) s
runFromYamlT (FromYamlT a) s = runStateT (runErrorT a) s

instance MonadTrans FromYamlT where
    lift m = FromYamlT $ lift (lift m)

getFromYaml :: Monad m => FromYamlT m FromYamlState
getFromYaml = FromYamlT get

putFromYaml :: Monad m => FromYamlState -> FromYamlT m ()
putFromYaml = FromYamlT . put

modifyFromYaml :: Monad m => (FromYamlState -> FromYamlState) -> FromYamlT m ()
modifyFromYaml = FromYamlT . modify

type FromYamlM = FromYamlT Identity

{-| Note: Implementations of allowExclusion MUST NOT EVALUATE THEIR ARGUMENT. It's only there to specify the type. Similarly, you MUST NOT allow the exclusion of a strict member of a record.

-}
class AllowExclusion a where
    allowExclusion :: a -> String -> Bool

data AllowExclusionD a = AllowExclusionD {
      allowExclusionD :: a -> String -> Bool
}

instance AllowExclusion t => Sat (AllowExclusionD t) where
    dict = AllowExclusionD { allowExclusionD = allowExclusion }

class (TranslateField a, AllowExclusion a) => FromYaml a where
    fromYaml :: YamlObject FromYamlAnnotation Text Text -> FromYamlM a

data FromYamlD a = FromYamlD {
      fromYamlD :: YamlObject FromYamlAnnotation Text Text -> FromYamlM a,
      allowExclusionD' :: a -> String -> Bool,
      translateFieldD'' :: a -> String -> String }

fromYamlProxy :: Proxy FromYamlD
fromYamlProxy = error "fromYamlProxy should never be evaluated!"

instance FromYaml t => Sat (FromYamlD t) where
    dict = FromYamlD { fromYamlD = fromYaml,
                       allowExclusionD' = allowExclusion,
                       translateFieldD'' = translateField }

instance AllowExclusion a where
    allowExclusion _ _ = False

instance FromYaml Int where
    fromYaml v = tryCache v scalarFromYamlAttempt

instance FromYaml Integer where
    fromYaml v = tryCache v scalarFromYamlAttempt



scalarFromYamlAttempt :: forall t k x. (Convertible String t, Typeable t, Show t) => YamlObject FromYamlAnnotation Text Text -> FromYamlM t
scalarFromYamlAttempt (Scalar ann n) =
    case (safeConvert :: String -> ConvertResult t) $ convert n of
      Right v -> return v
      Left e -> throwError $ CouldntReadScalar (fromYamlPosition ann) (show $ typeOf (undefined :: t)) (cs n) 

scalarFromYamlAttempt e = 
    throwError $ UnexpectedElementType
                   (fromYamlPosition (annotation e))
                   (show (typeOf (undefined :: t)))
                   (head . words . show $ e)

scalarFromYaml :: forall t k x. (Convertible String t, Typeable t) => YamlObject FromYamlAnnotation Text Text -> FromYamlM t
scalarFromYaml (Scalar ann n) =
    return $ (cs :: String -> t) $ cs n
scalarFromYaml e = 
    throwError $ UnexpectedElementType
                   (fromYamlPosition (annotation e))
                   (show (typeOf (undefined :: t)))
                   (head . words . show $ e)

{-
instance Convert [Char] Int where
    safeConvert s = 
        case reads s of
          [(v, "")] -> Success v
          _ -> failureString ("Invalid Double: " ++ s)
-}
instance FromYaml Float where
    fromYaml v = tryCache v scalarFromYamlAttempt
instance FromYaml Double where
    fromYaml v = tryCache v scalarFromYamlAttempt

instance FromYaml Char where
    fromYaml v = tryCache v go where
                       err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) "Char" s
                       go (Sequence ann xs) = err ann "Sequence"
                       go (Mapping ann xs) = err ann "Mapping"
                       go (Scalar ann c) = if T.length c == 1 then return (T.head c) else err ann "Scalar (wanted one of precisely one character)"

instance FromYaml [Char] where
    fromYaml v = tryCache v scalarFromYamlAttempt

instance (Typeable a, Data FromYamlD a) => FromYaml [a] where
    fromYaml v = tryCache v go
                 where 
                   err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("[" ++ show (typeOf (undefined :: a)) ++ "]") s

                   go (Sequence ann xs) = mapM fromYaml xs
                   go (Mapping ann _) = err ann "Mapping"
                   go (Scalar ann _) = err ann "Scalar"

instance (Typeable a, Data FromYamlD a) => FromYaml (Maybe a) where
    fromYaml v = tryCache v go
                 where
                   err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) 
                               (show $ typeOf (undefined :: Maybe a))
                                s
                   go (Mapping ann ((key, val):[])) 
                       | key == T.pack "just" = Just `fmap` fromYaml val
                       | otherwise = err ann "Mapping containing something other than only the key \"just\""
                   go (Mapping ann []) = return Nothing
                   go (Mapping ann _) = err ann "Mapping (containing something other than only the key \"just\")"
                   go (Sequence ann xs) = err ann "Sequence"
                   go (Scalar ann _) = err ann "Scalar"

instance (Typeable a, Data FromYamlD a, Data FromYamlD k, Typeable k, Read k, Ord k) => FromYaml (Map k a) where
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) 
                    ("Map (" ++ show (typeOf (undefined :: k)) ++ ") (" ++ show (typeOf (undefined :: a)))
                    s
        go (Sequence ann _) = err ann "Sequence"
        go (Scalar ann _) = err ann "Scalar"

        go (Mapping ann ps) = 
            let keysAttempts :: [[(k, String)]]
                keysAttempts = map (reads . (cs :: Text -> String) . fst) ps in
            case findIndex null keysAttempts of
                Just i -> throwError $ CouldntReadScalar (fromYamlPosition ann) (show (typeOf (undefined :: k))) (cs . fst $ ps !! i)
                Nothing -> do vs <- mapM (fromYaml . snd) ps
                              return $ Map.fromList $ zip (map (fst . head) keysAttempts) vs


instance (Typeable a, Data FromYamlD a, Ord a) => FromYaml (Set a) where
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("Set (" ++ show (typeOf (undefined :: a)) ++ ")") s
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann ss) = do
          rss <- mapM fromYaml ss
          return $ Set.fromList rss

instance (Ix i, Data FromYamlD i, Data FromYamlD e, Typeable i, Typeable e) => FromYaml (Array i e) where
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("Array (" ++
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

instance (Data FromYamlD a, Typeable a, Data FromYamlD b, Typeable b) => FromYaml (a, b) where
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
                                                                                   ++ show (typeOf (undefined :: b)) ++ ")") s
        go (Scalar ann _) = err ann "Scalar"
        go (Mapping ann _) = err ann "Mapping"
        go (Sequence ann (a:b:[])) =
            do ra <- fromYaml a
               rb <- fromYaml b
               return $ (ra, rb)
        go (Sequence ann _) = err ann "Sequence of length other than 2"
{-
instance (FromYaml a, Typeable a, FromYaml b, Typeable b, FromYaml c, Typeable c) => FromYaml (a, b, c) where
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
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
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
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
    fromYaml v = tryCache v go where
        err ann s = throwError $ UnexpectedElementType (fromYamlPosition ann) ("(" ++ show (typeOf (undefined :: a)) ++ ", "
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
-}

-- todo: Some instances might be missing, I think at least the one for Ratio...

{- Note that references to references are OK, as references can only be made to earlier data, and at any point in the data all earlier references will already have been resolved to their target. Anchors containing anchors need to be specially handled by tryCache though (as do anchors containing references). Though I'm not sure if those even occur in YamlPickle-generated trees or if they're just an artifact of the overly permissive datatype.

-}

tryCache :: forall a. (Data FromYamlD a, Typeable a) => YamlObject FromYamlAnnotation Text Text -> (YamlObject FromYamlAnnotation Text Text -> FromYamlM a) -> FromYamlM a
tryCache (Anchor a v) cont = do 
  FromYamlState refs <- getFromYaml
  rec { putFromYaml (FromYamlState (Map.insert a (toDyn r) refs))
      ; r <- case v of
               Anchor a' v' -> tryCache v cont
               Reference ann a' -> tryCache v cont
               _ -> cont v }
  return r
tryCache (Reference ann a) _ = do
  FromYamlState refs <- getFromYaml 
  case Map.lookup a refs of
    Nothing -> fail ("Bad fromYaml conversion: Reference to non-existent anchor " ++ show a)
    Just v -> case (fromDynamic v) :: Maybe a of
                Nothing -> fail "Bad fromYaml conversion: Reference to anchor of wrong type"
                Just r -> return r
tryCache a cont = cont a

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

genericFromYaml :: forall a. (Data FromYamlD a, TranslateField a) => YamlObject FromYamlAnnotation Text Text -> FromYamlM a
genericFromYaml yamlData =
    tryCache yamlData $ \yamlData' ->
    let dummy = undefined :: a
        datatype = dataTypeOf fromYamlProxy dummy
        rep = datarep datatype
        withConstructor v cs a = 
            case find ((== v) . T.pack . constring) cs of
              Nothing -> throwError $ UnexpectedElementType
                         (fromYamlPosition (annotation yamlData'))
                         ("a constructor of " ++ show datatype)
                         (convert v)
              Just c -> a c
        constructField :: 
            (Data FromYamlD x) =>
            Position
            -> String
            -> StateT ([(Text,  YamlObject FromYamlAnnotation Text Text)], [Text]) FromYamlM x
        constructField pos name = do
          (fieldsMap, fieldsList) <- get
          case fieldsList of
            [] -> error "constructField in an impossible situation: fromConstrM called us more times than there are fields to look at!"
            (field:fs) -> do
              put (fieldsMap, fs)
              case lookup field fieldsMap of
                Nothing -> if allowExclusionD' dict dummy (cs field) 
                           then return $ error "constructField: exclusion allowed per allowExclusion instance"
                           else throwError $ MissingMapElement pos name (cs field)
                Just y -> lift $ fromYamlD dict y

    in
      case rep of
        AlgRep cs ->
            case yamlData' of
              Scalar ann v -> -- we're in the generic instance and seeing a scalar? Must be a nullary constructor (I think)
                  withConstructor v cs (return . fromConstr fromYamlProxy)
              (Mapping ann [(v, o)]) -> 
                  tryCache o $ \o' ->
                  case o' of
                    Mapping ann as -> 
                        withConstructor v cs (\c -> evalStateT (fromConstrM fromYamlProxy (constructField (fromYamlPosition ann) (show datatype)) c)
                                              (as, map (convert . translateFieldD'' dict dummy) $ constrFields c))
                    Sequence ann as ->
                        withConstructor v cs (\c -> evalStateT (fromConstrM fromYamlProxy (constructUnnamedParameter (fromYamlPosition ann) (show datatype)) c)
                                                                as)
              _ -> throwError $ 
                   UnexpectedElementType (fromYamlPosition $ annotation yamlData')
                                         "(scalar or single-element mapping)"
                                         (head . words . show $ yamlData')
        _ -> error "genericFromYaml: generic instance called for a non-algebraic data type!"

constructUnnamedParameter :: 
    forall a. (Data FromYamlD a) => 
    Position
    -> String
    -> StateT [YamlObject FromYamlAnnotation Text Text] FromYamlM a
constructUnnamedParameter pos name = do
  objs <- get
  case objs of
    [] -> throwError $ InsufficientElements pos name
    (o:objs) -> do
              put objs
              lift $ fromYamlD dict o

type YObject = YamlObject Text Text


instance (Data FromYamlD t, TranslateField t) => FromYaml t where
    fromYaml = genericFromYaml

unmakeYaml :: FromYaml a => FromYamlObject -> Either FromYamlException a
unmakeYaml a = runIdentity $ evalFromYamlT (fromYaml a) $ FromYamlState Map.empty

-}