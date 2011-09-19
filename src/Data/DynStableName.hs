module Data.DynStableName where

import System.Mem.StableName
import Unsafe.Coerce

-- DynStableName copied from data-reify by Andy Gill
-- Stable names that not use phantom types.
-- As suggested by Ganesh Sittampalam.
data DynStableName = DynStableName (StableName ())

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
	(DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
	st <- makeStableName a
	return $ DynStableName (unsafeCoerce st)
