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
import Control.Arrow ((***))
import Control.Applicative
import Data.Traversable (traverse)

import GHC.Generics



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
