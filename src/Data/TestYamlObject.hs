{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}

module Data.TestYamlObject where

import Data.YamlObject
import Data.YamlObject.Types
import Debug.Trace
import GHC.Generics
import Data.Typeable

data Foo = Foo {
      a :: Int
    , b :: Int 
} | Bar Foo Foo
    deriving (Show, Generic, Typeable)

data Baz = Baz Int Baz deriving (Show, Generic, Typeable)

instance Share Foo where
    share _ = True
instance Share Baz where
    share _ = True

instance FromYaml Baz
instance ToYaml Baz
--instance FromYaml Foo