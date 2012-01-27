{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.TestYamlObject where

import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Derive
import Data.YamlObject
import Debug.Trace

data Foo = Foo {
      a :: Int
    , b :: Int 
} | Bar Foo Foo
    deriving Show

--instance DoShare Foo where
--    doShare _ = True

instance AllowExclusion Foo where
    allowExclusion _ _ | trace "foo1" False = undefined
    allowExclusion _ "a" = trace "foo" True
    allowExclusion _ _ = False

$( derive [''Foo] )