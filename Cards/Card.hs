module Cards.Card
  ( Tier -- Tier stuff
  , tierDiff
  , tierName
  , Card -- Card stuff
  , sides
  , tier
  , lastChecked
  , offset
  , isDue
  , reset
  , update
  , createCard
  ) where

import Data.Time

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
 - Parsing Cards
 -}

-- TODO
