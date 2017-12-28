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
  , testElements
  ) where

import Data.List
import Data.Time

testElements =
  [ card ["first card", "really"]
  , card ["second card", "really"]
  , comment "first comment"
  , card ["third card", "really"]
  , comment "second comment"
  ]
  where card = ECard . Unrevised
        comment = EComment . Comment

data Element = ECard Card | EComment Comment
  deriving (Show)

data Comment = Comment String
  deriving (Show)

data Card = Unrevised [String]
          | Revised [String] Tier UTCTime NominalDiffTime
  deriving (Show)

data Tier = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
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

tierDiff :: Tier -> NominalDiffTime
tierDiff One   = 60 * 10
tierDiff Two   = 60 * 20
tierDiff Three = 60 * 40
tierDiff Four  = 3600 * ( 1 * 24 - 8)
tierDiff Five  = 3600 * ( 2 * 24 - 8)
tierDiff Six   = 3600 * ( 4 * 24 - 8)
tierDiff Seven = 3600 * ( 8 * 24 - 8)
tierDiff Eight = 3600 * (16 * 24 - 8)
tierDiff Nine  = 3600 * (32 * 24 - 8)
tierDiff Ten   = 3600 * (64 * 24 - 8)

tierName :: Tier -> String
tierName One   = "10min"
tierName Two   = "20min"
tierName Three = "40min"
tierName Four  =  "1d"
tierName Five  =  "2d"
tierName Six   =  "4d"
tierName Seven =  "8d"
tierName Eight = "16d"
tierName Nine  = "32d"
tierName Ten   = "64d"

isDue :: UTCTime -> Card -> Bool
isDue _ (Unrevised _) = True
isDue time (Revised _ tier ctime cdiff) =
  let tdiff = tierDiff tier
  in  diffUTCTime time ctime >= cdiff + tdiff 

sides :: Card -> [String]
sides (Unrevised s)     = s
sides (Revised s _ _ _) = s

reset :: Card -> Card
reset (Revised s _ _ _) = Unrevised s
reset c@(Unrevised _)   = c

-- Uses the global RNG.
-- TODO: Add random offset based on tierDiff.
update :: UTCTime -> Card -> IO Card
update time (Unrevised s)     =
  return $ Revised s minBound                             time (fromInteger 0)
update time (Revised s t _ _) =
  return $ Revised s (if t < maxBound then succ t else t) time (fromInteger 0)

showElements :: [Element] -> String
showElements = intercalate "\n\n" . map showElement

showElement :: Element -> String
showElement = undefined

{-
 - Parsing
 -}

parseElements = undefined
parseElement = undefined
