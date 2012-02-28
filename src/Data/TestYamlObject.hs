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
import Data.Maybe
import Text.YamlPickle
import Data.Array
import qualified Data.Text as Text

data Foo = Bar Foo Foo |Foo {
      a :: Int
    , b :: Int 
}
    deriving (Show, Generic, Typeable, Eq)

data Baz = Baz Int Int Baz | Qux | Seq Int Int | Abc { stuff :: Foo, also :: Char } deriving (Show, Generic, Typeable, Eq)

data Aaaa = Aaaa Int Int deriving (Typeable, Generic, Show)

instance ToYaml Aaaa
instance FromYaml Aaaa

data FurniturePrototype = FurniturePrototype {
      furniturePrototypeName :: String 
    , furniturePrototypeGlyph :: StyledChar
    , furniturePrototypeDamageSequence :: [(Double, RubbleMaterial)]
    , furniturePrototypeWeight :: Double
    , furniturePrototypeWalkable :: Bool
    , furniturePrototypeConcealment :: Double
} deriving (Show, Read, Eq, Typeable, Generic)

instance Share FurniturePrototype where
    share = const False

data RubbleMaterial = WoodRubble | BookRubble | StoneRubble
                      deriving (Show, Read, Eq, Typeable, Generic)

data ColorName = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Generic)

data Style = Style {
      bright :: Bool
    , brightBg :: Bool
    , fgColor :: ColorName
    , bgColor :: ColorName
} deriving (Show, Read, Eq, Typeable, Generic)

data StyledChar = StyleAnimatedChar {
      styles :: [Style]
    , c :: Char } | StyledChar {
      style :: Style
    , c :: Char 
    }
                  deriving (Show, Read, Eq, Typeable, Generic)

instance Share Foo where
    share _ = False
instance Share Baz where
    share _ = True

instance FromYaml Foo
instance ToYaml Foo
instance FromYaml Baz
instance ToYaml Baz
instance FromYaml FurniturePrototype
instance FromYaml RubbleMaterial
instance FromYaml StyledChar
instance FromYaml Style
instance FromYaml ColorName
instance ToYaml FurniturePrototype
instance ToYaml RubbleMaterial
instance ToYaml StyledChar
instance ToYaml Style
instance ToYaml ColorName

data IdlingType = UseWorkstation | GetStuffOnShelf deriving (Show, Read, Eq, Typeable, Generic)

data IdlingPoint = IdlingPoint {
      ipIdlingType_ :: IdlingType
    , ipLocation_ :: Coord
} deriving (Show, Read, Eq, Typeable, Generic)

instance TranslateField IdlingPoint where
    translateField _ = Text.init

data Coord = Coord { x :: !Int, y :: !Int }
             deriving (Eq, Ord, Show, Ix, Read, Typeable, Generic)


data StoredFurnitureLayout = StoredFurnitureLayout {
      layoutCharacters :: String
    , layoutMap :: [String]
    , layoutWallConstraints :: (WallConstraint, WallConstraint, WallConstraint, WallConstraint)

    , layoutIdlingPoints :: [IdlingPoint]
    , layoutRooms :: [String]
    , layoutFrequency :: Double
} deriving (Show, Read, Eq, Typeable, Generic)

data WallConstraint = NoConstraint | AnyWall | OpenSpace | JustGlassWall | JustPlainWall deriving (Show, Read, Eq, Typeable, Generic)


instance ToYaml IdlingType
instance ToYaml IdlingPoint
instance ToYaml Coord
instance ToYaml WallConstraint
instance ToYaml StoredFurnitureLayout
instance FromYaml IdlingType
instance FromYaml IdlingPoint
instance FromYaml Coord
instance FromYaml WallConstraint
instance FromYaml StoredFurnitureLayout

test = let a = [Abc (Foo 1 2) 'a', Qux, Seq 1 2, Abc (Bar (Foo 1 2) (Foo 4 5)) 'c']
       in (unmakeYaml $ addDummyFromYamlAnnotations $ makeYaml a) == Right a

main = print ((unmakeYaml $ fromJust $ decode $ encode $ makeYaml $ FurniturePrototype "bookshelf" (StyledChar (Style False False Grey Black) 'c') [(50, BookRubble)] 100 False 0.8) :: Either FromYamlException FurniturePrototype)