module Cards
  ( Elements -- Elements stuff
  , updateElements
  , toCards
  , toDueCards
  , fromCards
  , elementsToString
  , parseElements
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

-- | Contains 'Card's and comments with a certain ordering.
-- To update some 'Card's in an 'Elements', use 'toCards' or 'toDueCards' and
-- 'fromCards'.
-- 'Card's can be removed (not added), as long as the numbers
-- associated to the remaining cards stay with their original card.
data Elements = Elements (Map.Map Integer Element)
  deriving (Show)

data Element = ECard Card
             | EComment String
  deriving (Show)

-- | A single index card with one or more sides.
data Card = Card
  { sides :: [String]
  -- ^ The sides of a 'Card'.
  -- As opposed to real index cards, a 'Card' may have more or less than two
  -- sides.
  , tier :: Tier
  -- ^ The 'Tier' of a 'Card'.
  , lastChecked :: UTCTime
  -- ^ The time a 'Card' was last looked at.
  , offset :: NominalDiffTime
  -- ^ A random offset, used when determining whether a 'Card' needs to be
  -- revised.
  -- This is to "stretch out" cards revised in a short time frame.
  } deriving (Show)

-- | The "level" of a 'Card' in a typical index card learning scheme.
-- Represents the time which should elapse before the 'Card' is looked at again.
data Tier = Unrevised
          | TenMin | TwentyMin | FortyMin
          | OneDay | TwoDays | FourDays | EightDays
          | SixteenDays | ThirtyTwoDays | SixtyFourDays
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | A 'Elements' containing some cards and some comments, for testing.
-- Will be removed soon.
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

-- | @'updateElements' a b@ adds the updated values from @b@ to @a@.
updateElements :: Elements -> Elements -> Elements
updateElements (Elements old) (Elements new) = Elements $ Map.union new old

-- | Extract all 'Card's from an 'Elements'.
-- Entries may be deleted or modified, as long as the numbers are not changed
-- and stay associated to their original 'Card'.
toCards :: Elements -> [(Integer, Card)]
toCards (Elements elms) =
  [(key, card) | (key, Just card) <- mapSnd toCard $ Map.toList elms]

-- | Extract all 'Card's which are due from an 'Elements'.
-- Entries may be deleted or modified, as long as the numbers are not changed
-- and stay associated to their original 'Card'.
toDueCards :: UTCTime -> Elements -> [(Integer, Card)]
toDueCards time = filter (isDue time . snd) . toCards

-- | Convert a list of 'Card's back into an 'Elements'.
-- As long as the same numbers are assosiated to the same cards as they were
-- originally, this can safely be used to update the original 'Elements'.
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

-- | @'isDue' time card@ returns whether @card@ needs to be revised at @time@.
isDue :: UTCTime -> Card -> Bool
isDue time Card{tier=t, lastChecked=lc, offset=o} =
  diffUTCTime time lc >= o + tierDiff t

-- These functions use the IO monad for generating random offsets.
-- TODO: actually implement random offset
updateOffset :: Card -> IO Card
updateOffset Card{sides=s, tier=t, lastChecked=lc} = do
  return Card{sides=s, tier=t, lastChecked=lc, offset=0}

-- | Reset a card's 'Tier'.
-- This also resets the 'lastChecked' and 'offset' times.
reset :: UTCTime -> Card -> Card
reset time Card{sides=s} =
  Card{sides=s, tier=minBound, lastChecked=time, offset=0}

-- | Push a 'Card' to the next highest 'Tier'.
-- Set the 'lastChecked' and 'offset' time accordingly.
-- Uses the IO monad to access the global random number generator.
update :: UTCTime -> Card -> IO Card
update time Card{sides=s, tier=t} =
  updateOffset $ Card {sides=s, tier=boundedSucc t, lastChecked=time, offset=0}

boundedSucc :: (Eq a, Bounded a, Enum a) => a -> a
boundedSucc a
  | a == maxBound = a
  | otherwise     = succ a

-- | Create a new card.
createCard :: UTCTime -> [String] -> Card
createCard time s =
  Card{sides=s, tier=minBound, lastChecked=time, offset=0}

{-
 - Tier stuff
 -}

-- | The 'NominalDiffTime' associated with each 'Tier'.
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

-- | The name associated with each 'Tier'.
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

-- | Convert an 'Elements' to a string which can be parsed by 'parseElements'.
-- This string can then be written to a text file for storage.
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
  in unlines $ info : intersperse "::" s ++ [""] -- newline at the end

{-
 - Parsing
 -}

-- | Not yet implemented.
parseElements :: String -> Maybe Elements
parseElements = undefined
