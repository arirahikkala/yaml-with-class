{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.ConvertibleInstances () where

import Data.Convertible
import Data.Text
import Numeric

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
