{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.YamlObject.Support where

import Data.YamlObject.Types
import Control.Monad.State (get, modify)
import System.IO.Unsafe
import Data.DynStableName
import qualified Data.Map as Map
import Data.Convertible (convert, convertVia, Convertible(safeConvert))
import Data.ConvertibleInstances
import Data.Text (Text)

toScalar x = return $ Scalar (ToYamlAnnotation ()) x
toReference x = return $ Reference (ToYamlAnnotation ()) x
toAnchor a x = return $ Anchor a x
toMapping xs = return $ Mapping (ToYamlAnnotation ()) xs
toSequence xs = return $ Sequence (ToYamlAnnotation ()) xs

usingCache
  :: (ToYaml a) =>
     a
     -> ToYamlM (YamlObject ToYamlAnnotation k v)
     -> ToYamlM (YamlObject ToYamlAnnotation k v)
usingCache x cont =
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

cToYaml x = usingCache x $ toYaml x