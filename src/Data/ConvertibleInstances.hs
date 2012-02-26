{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ConvertibleInstances () where

import Data.Convertible
import Data.Text
import Numeric

instance Convertible Text Double where
    safeConvert = convertVia (undefined :: String)

instance Convertible Text Float where
    safeConvert = convertVia (undefined :: String)

instance Convertible Text Integer where
    safeConvert = convertVia (undefined :: String)

instance Convertible Double Text where
    safeConvert = convertVia (undefined :: String)

instance Convertible Float Text where
    safeConvert = convertVia (undefined :: String)

instance Convertible Integer Text where
    safeConvert = convertVia (undefined :: String)

instance Show a => Convertible a String where
    safeConvert = Right . show

instance Show a => Convertible a Text where
    safeConvert = convertVia (undefined :: String)

instance Convertible [Char] Float where
    safeConvert s = 
        case readFloat s of
          [(v, "")] -> Right v
          _ -> convError ("Invalid Float: ") s

instance Convertible [Char] Double where
    safeConvert s = 
        case readFloat s of
          [(v, "")] -> Right v
          _ -> convError ("Invalid Double: ") s

instance Convertible [Char] Int where
    safeConvert s = 
        case reads s of
          [(v, "")] -> Right v
          _ -> convError ("Invalid Int: " ++ s) s

instance Convertible [Char] Integer where
    safeConvert s = 
        case reads s of
          [(v, "")] -> Right v
          _ -> convError ("Invalid Integer: " ++ s) s

instance Convertible Int [Char] where
    safeConvert = Right . show

instance Convertible [Char] [Char] where
    safeConvert = Right 

instance Convertible Int Text where
    safeConvert = convertVia (undefined :: [Char])

instance Convertible Text Int where
    safeConvert = convertVia (undefined :: [Char])
