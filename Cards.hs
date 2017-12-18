module Cards
  ( Element
  , Card
  , Comment
  , isCard
  , sides
  , resetLevel
  , nextLevel
  , showElement
  , showElements
  , parseElement
  , parseElements
  ) where

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

sides :: Card -> [String]
sides (Card _ _ _ s) = s

resetTier :: Card -> Card
resetTier (Card t l d s) = Card minBound l d s

nextTier :: Card -> Card
nextTier c@(Card t l d s)
  | t == maxBound = c
  | otherwise     = Card (succ t) l d s

showElements :: [Element] -> String
showElements = intercalate "\n\n" . map showElement

showElement :: Element -> String
showElement = undefined

{-
 - Parsing
 -}

parseElements = undefined
parseElement = undefined
