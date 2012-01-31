{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Text.YamlPickle
    ( encode, encodeFile, decode, decodeFile, ParseException (..), YamlScalar (..))
{-
    ( -- * Definition of 'YamlObject'
      YamlScalar (..)
    , YamlObject
      -- * Automatic scalar conversions
    , IsYamlScalar (..)
    , toYamlObject
    , fromYamlObject
      -- * Encoding/decoding
    , encode
    , encodeFile
    , decode
    , decodeFile
      -- * Exceptions
    , ParseException (..)
    ) -} where

import qualified Text.Libyaml as Y
import Text.Libyaml hiding (encode, decode, encodeFile, decodeFile, Anchor)
import Data.YamlObject hiding (value, FromYamlObject, ToYamlObject)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Control.Failure

import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

import Data.Convertible.Text (cs)
import Data.Data

--import Control.Monad.Trans.Class
--import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import qualified Data.Enumerator.List as EL
import Prelude hiding (catch)
import Control.Exception (throwIO, Exception)
import Data.String (IsString (fromString))

import Control.Arrow ((***))

import Foreign.C
import Foreign.Ptr

-- | Equality depends on 'value' and 'tag', not 'style' and 'anchor'.
data YamlScalar = YamlScalar
    { value :: ByteString
    , tag :: Tag
    , style :: Style
    }
    deriving (Show, Typeable)

type FromYamlObject = YamlObject FromYamlAnnotation YamlScalar YamlScalar

-- | Merge assoc-lists by keys.
-- First list overrides second:
--     [(k1, x), (k2, y)] `mergeAssocLists` [(k3, z)] == [(k1, x), (k2, y), (k3, z)]
--     [(k1, x), (k2, y)] `mergeAssocLists` [(k2, z)] == [(k1, x), (k2, y)]
mergeAssocLists :: (Eq k) => [(k, v)] -> [(k, v)] -> [(k, v)]
mergeAssocLists a [] = a
mergeAssocLists [] b = b
mergeAssocLists a ((bk, bv):bs) =
    case lookup bk a of
      Nothing -> (bk, bv) : mergeAssocLists a bs
      Just av -> (bk, av) : mergeAssocLists (filter (\(x, _) -> x /= bk) a) bs


instance Eq YamlScalar where
    (YamlScalar v t _) == (YamlScalar v' t' _) = v == v' && t == t'
--instance IsString YamlScalar where
--    fromString = toYamlScalar

-- todo: use a separate constructor for this?
noPosition = Position (-1) (-1) (-1) (-1)

class (Eq a) => IsYamlScalar a where
    fromYamlScalar :: YamlScalar -> a
    toYamlScalar :: a -> YamlScalar

instance IsYamlScalar YamlScalar where
    fromYamlScalar = id
    toYamlScalar = id
instance IsYamlScalar Data.Text.Text where
    fromYamlScalar = cs . value
    toYamlScalar t = YamlScalar (cs t) NoTag Any
instance IsYamlScalar Data.Text.Lazy.Text where
    fromYamlScalar = cs . value
    toYamlScalar t = YamlScalar (cs t) NoTag Any
instance IsYamlScalar [Char] where
    fromYamlScalar = cs . value
    toYamlScalar s = YamlScalar (cs s) NoTag Any
instance IsYamlScalar Data.ByteString.ByteString where
    fromYamlScalar = value
    toYamlScalar b = YamlScalar b NoTag Any
instance IsYamlScalar Data.ByteString.Lazy.ByteString where
    fromYamlScalar = cs . value
    toYamlScalar b = YamlScalar (cs b) NoTag Any


toYamlObject :: IsYamlScalar k
             => IsYamlScalar v
             => YamlObject ToYamlAnnotation k v
             -> YamlObject ToYamlAnnotation YamlScalar YamlScalar
toYamlObject = mapKeysValues toYamlScalar toYamlScalar

fromYamlObject :: IsYamlScalar k
               => IsYamlScalar v
               => YamlObject FromYamlAnnotation YamlScalar YamlScalar
               -> YamlObject FromYamlAnnotation k v
fromYamlObject = mapKeysValues fromYamlScalar fromYamlScalar

encode :: (IsYamlScalar k, IsYamlScalar v) 
          => YamlObject ToYamlAnnotation k v
              -> ByteString
encode obj = unsafePerformIO $ do
    x <- E.run $ E.enumList 1 (objToEvents $ toYamlObject obj) $$ Y.encode
    case x of
        Left err -> throwIO err
        Right y -> return y

encodeFile :: (IsYamlScalar k, IsYamlScalar v)
           => FilePath
           -> YamlObject ToYamlAnnotation k v
           -> IO ()
encodeFile fp obj = do
    x <- E.run $ E.enumList 1 (objToEvents $ toYamlObject obj)
              $$ Y.encodeFile fp
    case x of
        Left err -> throwIO err
        Right () -> return ()

objToEvents :: YamlObject ToYamlAnnotation YamlScalar YamlScalar -> [Y.Event]
objToEvents o = (:) EventStreamStart
              . (:) EventDocumentStart
              $ objToEvents' Nothing o
              [ EventDocumentEnd
              , EventStreamEnd
              ]

scalarToEvent :: Maybe String -> YamlScalar -> Event
scalarToEvent r (YamlScalar v t s) = EventScalar v t s r

objToEvents' :: Maybe String 
             -> YamlObject ToYamlAnnotation YamlScalar YamlScalar 
             -> ([Y.Event] -> [Y.Event])
-- recursive Anchors? I dunno, let's just ignore all but the inmost one I guess
objToEvents' _ (Anchor (Original s) v) rest = objToEvents' (Just $ cs s) v rest
-- surrogates should never go back out
objToEvents' _ (Anchor (Surrogate s) v) rest = objToEvents' Nothing v rest
objToEvents' _ (Reference ann (Original s)) rest = EventAlias (cs s) : rest
objToEvents' _ (Reference ann (Surrogate s)) rest = rest
objToEvents' a (Scalar ann s) rest = scalarToEvent a s : rest
objToEvents' a (Sequence ann list) rest =
    EventSequenceStart a
  : foldr ($) (EventSequenceEnd : rest) (map (objToEvents' Nothing) list)
objToEvents' a (Mapping ann pairs) rest =
    EventMappingStart a
  : foldr ($) (EventMappingEnd : rest) (map pairToEvents pairs)

pairToEvents :: 
    (YamlScalar, YamlObject ToYamlAnnotation YamlScalar YamlScalar) 
        -> [Y.Event] 
        -> [Y.Event]
pairToEvents (k, v) rest =
    scalarToEvent Nothing k
  : objToEvents' Nothing v rest


-- Parsing


data ParserState = ParserState {
      refs :: Map.Map String [(YamlScalar, Int)]
    , firstFreeId :: Int
}

newtype PErrorT m a = PErrorT { runPErrorT :: m (Either ParseException a) }
type Parser = PErrorT (StateT ParserState IO)

data ParseException = NonScalarKey
                    | UnknownAlias { _anchorName :: Y.AnchorName }
                    | UnexpectedEvent { _received :: Maybe Event
                                      , _expected :: Maybe Event
                                      , _position :: Maybe Position
                                      }
                    | InvalidYaml (Maybe String)
    deriving (Show, Typeable)
instance Exception ParseException


instance Monad m => Monad (PErrorT m) where
    return = PErrorT . return . Right
    (PErrorT m) >>= f = PErrorT $ do
        e <- m
        case e of
            Left e' -> return $ Left e'
            Right a -> runPErrorT $ f a
instance MonadTrans PErrorT where
    lift = PErrorT . liftM Right
instance MonadIO m => MonadIO (PErrorT m) where
    liftIO = lift . liftIO

pfailure :: Monad m => ParseException -> PErrorT m a
pfailure = PErrorT . return . Left




--requireEvent :: (Event, Ptr ParserStruct) -> E.Iteratee (Event, Ptr ParserStruct) Parser ()
requireEvent e = do
    f <- EL.head
    if snd `fmap` f == Just e
        then return ()
        else lift $ pfailure $ unexpected f (Just e)

parse :: E.Iteratee (Position, Event) Parser FromYamlObject
parse = do
    requireEvent EventStreamStart
    requireEvent EventDocumentStart
    res <- parseO
    requireEvent EventDocumentEnd
    requireEvent EventStreamEnd
    return res


--parseScalar :: ByteString -> Tag -> Style
--            -> E.Iteratee (Position, Event) Parser YamlScalar
parseScalar v t s = do
  return $ YamlScalar v t s


anchorOn Nothing a = a
anchorOn (Just s) a = 
    do r <- a
       return $ Anchor (Original $ cs s) r

startedAt (FromYamlAnnotation (Position _ el _ ec)) (Position sl _ sc _)  =
    FromYamlAnnotation (Position sl el sc ec)

unexpected Nothing expected = UnexpectedEvent Nothing expected Nothing
unexpected (Just (pos, received)) expected = 
    UnexpectedEvent (Just received) expected (Just pos)

parseO :: E.Iteratee (Position, Event) Parser FromYamlObject
parseO = do
    me <- EL.head
    case me of
        Just (p, EventScalar v t s a) -> 
            anchorOn a $ (Scalar (FromYamlAnnotation p) `liftM` parseScalar v t s)
        Just (p, EventSequenceStart a) -> anchorOn a $ parseS p id
        Just (p, EventMappingStart a) -> anchorOn a $ parseM p id a id
        Just (p, EventAlias an) -> 
            return $ Reference (FromYamlAnnotation p) (Original $ cs an)
        _ -> lift $ pfailure $ unexpected me Nothing

parseS :: Position
       -> ([FromYamlObject] -> [FromYamlObject])
       -> E.Iteratee (Position, Event) Parser FromYamlObject
parseS start front = do
    me <- E.peek
    case me of
        Just (pos, EventSequenceEnd) -> do
            EL.drop 1
            return $ Sequence (FromYamlAnnotation pos `startedAt` start) $ front []
        _ -> do
            o <- parseO
            parseS start $ front . (:) o



--toB = Data.ByteString.pack . map (toEnum . fromEnum)

{- Data merge handling adds a fair bit of complexity to this function. 

   Any time we process a mapping that has an anchor, we create surrogate anchors
   for each element of the mapping, and store the anchor and the list of keys
   and their generated anchors in the state.

   The simple and wrong thing to do would be to process merges right when we
   see them, like any other key. Thing is, earlier mapping node keys override
   later ones *unless* they come from merges. So we need to store the merges
   and add them right at the end (mapping node order is a serialization detail
   that we're allowed to define as we like)
-}
putParser a b = lift $ lift $ put $ ParserState a b

parseMergeSequence refs merges ss =
    mapM go ss >>= \ms -> return $ foldr (.) merges ms where
        go (Reference pos (Original ref)) = 
            case Map.lookup (cs ref) refs of
              Just v -> return (map (pos, ) v ++)
              Nothing -> lift $ pfailure $ UnknownAlias $ cs ref
        go _ = lift $ pfailure $ error "todo: define error"

type Merges = ([(FromYamlAnnotation, (YamlScalar, Int))] -> [(FromYamlAnnotation, (YamlScalar, Int))])
type Front = ([(YamlScalar, FromYamlObject)] -> [(YamlScalar, FromYamlObject)])
parseMerges :: YamlScalar
            -> FromYamlObject
            -> Y.Anchor
            -> Merges
            -> Front
            -> E.Iteratee (Position, Event) Parser (Front, Merges)
parseMerges key value a merges front = do
  ParserState refs firstFreeId <- lift $ lift $ get
  if fromYamlScalar key == "<<"
  then do (ParserState refs firstFreeId) <- lift $ lift get
          case value of
            -- todo: record position in error message
            Reference pos (Original r) -> 
                case Map.lookup (cs r) refs of
                  Nothing -> lift $ pfailure $ UnknownAlias $ cs r
                  Just v -> return (front, (merges (map (pos,) v) ++))
            -- todo: record position in error message
            Sequence pos rs -> 
                (front,) `liftM` parseMergeSequence refs merges rs
            _ -> 
                 return (front, merges) -- todo: any error handling at all
  else case a of
         Nothing -> return ((front [(key, value)] `mergeAssocLists`), merges)
         Just anchor -> do
             putParser (Map.insertWith (flip (++)) anchor [(key, firstFreeId)] refs) 
                       (succ firstFreeId)
             return ((front [(key, Anchor (Surrogate firstFreeId) value)] `mergeAssocLists`),
                     merges)

parseM :: Position
       -> Merges
       -> Y.Anchor
       -> Front
       -> E.Iteratee (Position, Event) Parser FromYamlObject
parseM start merges a front = do
    me <- E.peek
    case me of
        Just (p, EventMappingEnd) -> do
            EL.drop 1
            let final = front $ map (\(p, (v, anchor)) ->
                                                 (v, Reference p $ Surrogate anchor)) $ merges []
            return $ Mapping (FromYamlAnnotation p `startedAt` start) $ final
        _ -> do
            me' <- EL.head
            key <- case me' of
                    Just (p, EventScalar v t s a') -> parseScalar v t s
                    _ -> lift $ pfailure $ unexpected me' Nothing
            value <- parseO

            (front', merges') <- parseMerges key value a merges front

            parseM start merges' a front'




decode :: (Failure ParseException m, IsYamlScalar k, IsYamlScalar v)
       => ByteString
       -> m (YamlObject FromYamlAnnotation k v) --YamlScalar YamlScalar)-- k v)
decode bs = unsafePerformIO $ do
    x <- flip evalStateT (ParserState Map.empty 0) $ runPErrorT $ E.run $ Y.decode bs $$ parse
    case x of
        Left err -> return $ failure err
        Right (Left err) -> return $ failure $ InvalidYaml $ Just $ show err
        Right (Right y) -> return $ return $ fromYamlObject y

decodeFile
  :: (Text.YamlPickle.IsYamlScalar k,
      Text.YamlPickle.IsYamlScalar v,
      Failure ParseException f) =>
     FilePath -> IO (f (YamlObject FromYamlAnnotation k v))
decodeFile fp = do
    x <- flip evalStateT (ParserState Map.empty 0) $ runPErrorT $ E.run $ Y.decodeFile fp $$ parse
    case x of
        Left err -> return $ failure err
        Right (Left err) -> return $ failure $ InvalidYaml $ Just $ show err
        Right (Right y) -> return $ return $ fromYamlObject y

