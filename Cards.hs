module Cards
  ( module Cards.Card
  , Element
  , Comment
  , toCard
  , fromCard
  , testElements
  ) where

import Cards.Card
import Data.Time

testElements :: [Element]
testElements =
  [ card ["first card", "really"]
  , card ["second card", "really"]
  , comment "first comment"
  , card ["third card", "really"]
  , comment "second comment"
  ]
  where card = ECard . createCard someutctime
        comment = EComment . Comment
        someutctime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

data Element = ECard Card | EComment Comment
  deriving (Show)

data Comment = Comment String
  deriving (Show)

{-
 - Basic utility functions
 -}

toCard :: Element -> Maybe Card
toCard (ECard c) = Just c
toCard  _        = Nothing

fromCard :: Card -> Element
fromCard = ECard

