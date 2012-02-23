{-# LANGUAGE DeriveGeneric #-}

module Data.TestYamlObject where

import Data.YamlObject
import Data.YamlObject.Types
import Debug.Trace
import GHC.Generics

data Foo = Foo {
      a :: Int
    , b :: Int 
} | Bar Foo Foo
    deriving (Show, Generic)

data Baz = Baz Int Int Int deriving (Show, Generic)

instance ToYaml Foo where
    share _ = True
instance ToYaml Baz