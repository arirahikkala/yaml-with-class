{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.YamlObject.Support where

import Data.YamlObject.Types
import Control.Monad.State (get, modify, put)
import System.IO.Unsafe
import Data.DynStableName
import Data.Dynamic (toDyn, fromDynamic)
import qualified Data.Map as Map
import Data.Convertible (convert, convertVia, Convertible(safeConvert), ConvertError)
import Data.ConvertibleInstances
import Data.Text (Text)
import Control.Monad.Error (throwError)
import Text.Libyaml (Position(..))
import Data.Typeable (Typeable(typeOf))
import GHC.Generics

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
    do (ToYamlState index m) <- ToYamlT get
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

cToYaml x = toCache x $ toYaml x

--fromCache :: forall a. (FromYaml a, Typeable a) => FromYamlObject -> (FromYamlObject -> FromYamlM a) -> FromYamlM a


uncurry3 f (a, b, c) = f a b c

typeMismatch str x = 
    throwError $ uncurry3 TypeMismatch $
    case x of
      Scalar ann v -> (fromYamlPosition ann, str, "Scalar")
      Sequence ann v -> (fromYamlPosition ann, str, "Sequence")
      Mapping ann v -> (fromYamlPosition ann, str, "Mapping")
      Reference ann v -> (fromYamlPosition ann, str, "Reference")
      Anchor {} -> (Position (-1) (-1) (-1) (-1), str, "Anchor (how did that happen?")

scalarFromYaml :: forall t. (Convertible Text t, Typeable t) => FromYamlObject -> FromYamlM t
scalarFromYaml (Scalar ann n) =
    return $ (convert :: Text -> t) n
scalarFromYaml e = 
    throwError $ TypeMismatch
                   (fromYamlPosition (annotation e))
                   (show (typeOf (undefined :: t)))
                   (head . words . show $ e)

scalarFromYamlAttempt :: forall t k x. (Convertible Text t, Typeable t, Show t) => YamlObject FromYamlAnnotation Text Text -> FromYamlM t
scalarFromYamlAttempt (Scalar ann n) =
    case (safeConvert :: Text -> Either ConvertError t) n of
      Right v -> return v
      Left e -> throwError $ CouldntReadScalar (fromYamlPosition ann) (show $ typeOf (undefined :: t)) (show n) 

scalarFromYamlAttempt e = 
    throwError $ TypeMismatch
                   (fromYamlPosition (annotation e))
                   (show (typeOf (undefined :: t)))
                   (head . words . show $ e)
