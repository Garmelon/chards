module Cards
  ( Elements -- Elements stuff
  , updateElements
  , toCards
  , toDueCards
  , fromCards
  , elementsToString
  , Card -- Card stuff
  , sides
  , tier
  , lastChecked
  , offset
  , isDue
  , reset
  , update
  , createCard
  , Tier -- Tier stuff
  , tierDiff
  , tierName
  , testElements
  ) where

import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Time

data Elements = Elements (Map.Map Integer Element)
  deriving (Show)

data Element = ECard Card
             | EComment String
  deriving (Show)

data Card = Card
  { sides :: [String]
  , tier :: Tier
  , lastChecked :: UTCTime
  , offset :: NominalDiffTime
  } deriving (Show)

data Tier = Unrevised
          | TenMin | TwentyMin | FortyMin
          | OneDay | TwoDays | FourDays | EightDays
          | SixteenDays | ThirtyTwoDays | SixtyFourDays
  deriving (Show, Eq, Ord, Enum, Bounded)

testElements :: Elements
testElements = Elements . Map.fromList. zip [1..] $
  [ card ["first card", "really"]
  , card ["second card", "really"]
  , comment "first comment"
  , card ["third card", "really"]
  , comment "second comment"
  ]
  where card = ECard . createCard someutctime
        comment = EComment
        someutctime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

{-
 - Elements stuff
 -}

updateElements :: Elements -> Elements -> Elements
updateElements (Elements old) (Elements new) = Elements $ Map.union new old

toCards :: Elements -> [(Integer, Card)]
toCards (Elements elms) =
  [(key, card) | (key, Just card) <- mapSnd toCard $ Map.toList elms]

toDueCards :: UTCTime -> Elements -> [(Integer, Card)]
toDueCards time = filter (isDue time . snd) . toCards

fromCards :: [(Integer, Card)] -> Elements
fromCards = Elements . Map.fromList . mapSnd fromCard

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f l = [(a, f b) | (a, b) <- l]

{-
 - Element stuff
 -}

toCard :: Element -> Maybe Card
toCard (ECard c) = Just c
toCard _         = Nothing

fromCard :: Card -> Element
fromCard = ECard

{-
 - Card stuff
 -}

isDue :: UTCTime -> Card -> Bool
isDue time Card{tier=t, lastChecked=lc, offset=o} =
  diffUTCTime time lc >= o + tierDiff t

-- These functions use the IO monad for generating random offsets.
-- TODO: actually implement random offset
updateOffset :: Card -> IO Card
updateOffset Card{sides=s, tier=t, lastChecked=lc} = do
  return Card{sides=s, tier=t, lastChecked=lc, offset=0}

reset :: UTCTime -> Card -> Card
reset time Card{sides=s} =
  Card{sides=s, tier=minBound, lastChecked=time, offset=0}

update :: UTCTime -> Card -> IO Card
update time Card{sides=s, tier=t} =
  updateOffset $ Card {sides=s, tier=boundedSucc t, lastChecked=time, offset=0}

-- helper function
boundedSucc :: (Eq a, Bounded a, Enum a) => a -> a
boundedSucc a
  | a == maxBound = a
  | otherwise     = succ a

createCard :: UTCTime -> [String] -> Card
createCard time s =
  Card{sides=s, tier=minBound, lastChecked=time, offset=0}

{-
 - Tier stuff
 -}

tierDiff :: Tier -> NominalDiffTime
tierDiff Unrevised     = 0
tierDiff TenMin        = 60 * 10
tierDiff TwentyMin     = 60 * 20
tierDiff FortyMin      = 60 * 40
tierDiff OneDay        = 3600 * ( 1 * 24 - 8)
tierDiff TwoDays       = 3600 * ( 2 * 24 - 8)
tierDiff FourDays      = 3600 * ( 4 * 24 - 8)
tierDiff EightDays     = 3600 * ( 8 * 24 - 8)
tierDiff SixteenDays   = 3600 * (16 * 24 - 8)
tierDiff ThirtyTwoDays = 3600 * (32 * 24 - 8)
tierDiff SixtyFourDays = 3600 * (64 * 24 - 8)

tierName :: Tier -> String
tierName Unrevised     = "unrevised"
tierName TenMin        = "10min"
tierName TwentyMin     = "20min"
tierName FortyMin      = "40min"
tierName OneDay        =  "1d"
tierName TwoDays       =  "2d"
tierName FourDays      =  "4d"
tierName EightDays     =  "8d"
tierName SixteenDays   = "16d"
tierName ThirtyTwoDays = "32d"
tierName SixtyFourDays = "64d"

{-
 - Converting to String
 -}

elementsToString :: Elements -> String
elementsToString (Elements e) =
  let elms = map snd $ Map.toList e
  in unlines $ intercalate ["", ""] $ map (\x -> [elementToString x]) elms

elementToString :: Element -> String
elementToString (EComment str) = '#' : str
elementToString (ECard card)   = cardToString card

cardToString :: Card -> String
cardToString Card{sides=s, tier=t, lastChecked=lc, offset=o} =
  let info = ":: {\"level\": " ++ (show $ fromEnum t) ++
             ", \"last_checked\": " ++ (show $ formatTime defaultTimeLocale "%s" lc) ++
             ", \"delay\": " ++ (show $ fromEnum o) ++
             "}"
  in unlines $ info : intersperse "::" s

{-
 - Parsing
 -}

-- TODO
