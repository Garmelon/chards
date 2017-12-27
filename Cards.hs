module Cards
  ( Element
  , Card
  , Comment
  , fromElement
  , toElement
  , isDue
  , sides
  , reset
  , update
  , showElement
  , showElements
  , parseElement
  , parseElements
  ) where

import Data.List
import Data.Time

wee = EComment $ Comment "wee"

data Element = ECard Card | EComment Comment
  deriving (Show)

data Comment = Comment String
  deriving (Show)

type LastChecked = Integer
type Delay = Integer
data Card = Card Tier LastChecked Delay [String]
  deriving (Show)

data Tier = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  deriving (Show, Eq, Ord, Enum, Bounded)

{-
 - Basic utility functions
 -}

isCard :: Element -> Bool
isCard (ECard _) = True
isCard _         = False

fromElement :: Element -> Maybe Card
fromElement (ECard c) = Just c
fromElement  _        = Nothing

toElement :: Card -> Element
toElement = ECard

isDue :: UTCTime -> Card -> Bool
isDue = undefined

sides :: Card -> [String]
sides (Card _ _ _ s) = s

reset :: Card -> Card
reset (Card t l d s) = Card minBound l d s

update :: UTCTime -> Card -> Card
update = undefined

showElements :: [Element] -> String
showElements = intercalate "\n\n" . map showElement

showElement :: Element -> String
showElement = undefined

{-
 - Parsing
 -}

parseElements = undefined
parseElement = undefined
