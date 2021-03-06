-- | Index-card-like system.
module Cards
  ( Elements -- Elements stuff
  , updateElements
  , toCards
  , toDueCards
  , fromCards
  , elementsToString
  , parseElements
  , parseElementsMaybe
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
  , Parser -- Other stuff
  ) where

import           Control.Monad
import           Data.Function
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Void
import           System.Random
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Contains 'Card's and comments with a certain ordering.
--
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
  { sides       :: [String]
  -- ^ The sides of a 'Card'.
  --
  -- As opposed to real index cards, a 'Card' may have more or less than two
  -- sides.
  , tier        :: Tier
  -- ^ The 'Tier' of a 'Card'.
  , lastChecked :: UTCTime
  -- ^ The time a 'Card' was last looked at.
  , offset      :: NominalDiffTime
  -- ^ A random offset, used when determining whether a 'Card' needs to be
  -- revised.
  --
  -- This is to "stretch out" cards revised in a short time frame.
  } deriving (Show)

-- | The "level" of a 'Card' in a typical index card learning scheme.
--
-- Represents the time which should elapse before the 'Card' is looked at again.
data Tier = Unrevised
          | TenMin | TwentyMin | FortyMin
          | OneDay | TwoDays | FourDays | EightDays
          | SixteenDays | ThirtyTwoDays | SixtyFourDays
  deriving (Show, Eq, Ord, Enum, Bounded)

{-
 - Elements stuff
 -}

-- | @'updateElements' a b@ adds the updated values from @b@ to @a@.
updateElements :: Elements -> Elements -> Elements
updateElements (Elements old) (Elements new) = Elements $ Map.union new old

-- | Convert a list of 'Card's back into an 'Elements'.
--
-- As long as the same numbers are assosiated to the same cards as they were
-- originally, this can safely be used to update the original 'Elements'.
fromCards :: [(Integer, Card)] -> Elements
fromCards = Elements . Map.fromList . mapSnd ECard
  where mapSnd f = map (\(a, b) -> (a, f b))

-- | Extract all 'Card's from an 'Elements'.
--
-- Entries may be deleted or modified as long as the numbers are not changed
-- and stay associated to their original 'Card'.
toCards :: Elements -> [(Integer, Card)]
toCards (Elements elms) =
  [(key, card) | (key, ECard card) <- Map.toList elms]

-- | Extract all 'Card's which are due from an 'Elements'.
--
-- Entries may be deleted or modified, as long as the numbers are not changed
-- and stay associated to their original 'Card'.
toDueCards :: UTCTime -> Elements -> [(Integer, Card)]
toDueCards time = filter (isDue time . snd) . toCards

{-
 - Card stuff
 -}

-- | @'isDue' time card@ returns whether @card@ needs to be revised at @time@.
isDue :: UTCTime -> Card -> Bool
isDue time Card{tier=t, lastChecked=lc, offset=o} =
  diffUTCTime time lc >= o + tierDiff t

-- These functions use the IO monad for generating random offsets.
updateOffset :: Card -> IO Card
updateOffset Card{sides=s, tier=t, lastChecked=lc} = do
  let maxOffset = tierDiff t / 4
  o <- integerToNom <$> randomRIO (0, nomToInteger maxOffset)
  return Card{sides=s, tier=t, lastChecked=lc, offset=o}

-- | Reset a card's 'Tier'.
--
-- This also resets the 'lastChecked' and 'offset' times.
reset :: UTCTime -> Card -> Card
reset time Card{sides=s} =
  Card{sides=s, tier=minBound, lastChecked=time, offset=0}

-- | Push a 'Card' to the next highest 'Tier'.
--
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

-- Two more utility functions for dealing with NominalDiffTimes

integerToNom :: Integer -> NominalDiffTime
integerToNom = fromInteger

nomToInteger :: NominalDiffTime -> Integer
nomToInteger = (truncate :: Double -> Integer) . realToFrac

{-
 - Converting to String
 -}

-- | Convert an 'Elements' to a string which can be parsed by 'parseElements'.
--
-- This string can then be written to a text file for storage.
elementsToString :: Elements -> String
elementsToString (Elements e) =
  let elms = map snd $ sortBy (compare `on` fst) $ Map.toList e
  in unlines $ intercalate ["", ""] $ map elementToLines elms

elementToLines :: Element -> [String]
elementToLines (EComment str) = lines ('#' : str)
elementToLines (ECard card)   = cardToLines card

cardToLines :: Card -> [String]
cardToLines Card{sides=s, tier=t, lastChecked=lc, offset=o} =
  let infoTier = show $ fromEnum t
      infoLastChecked = formatTime defaultTimeLocale "%s" lc
      infoOffset = show $ nomToInteger o
      info = ":: {\"level\": " ++ infoTier ++
             if t /= minBound
               then ", \"last_checked\": " ++ infoLastChecked ++
                    ", \"delay\": " ++ infoOffset ++ "}"
               else "}"
  in info : intercalate ["::"] (map lines s)

{-
 - Parsing
 -}

-- | Simple alias to clean up type signatures.
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

nonemptyLine :: Parser String
nonemptyLine = anyChar `someTill` newline

-- Combines try and lookAhead: Never modifies the stack.
followedBy :: Parser a -> Parser ()
followedBy = void . try . lookAhead

-- separates sides of cards
cardSeparator :: Parser String
cardSeparator = string "::" <?> "card separator"

-- separates elements
separator :: Parser ()
separator = void newline <|> void eof <?> "separator"

-- infostring

infostring :: UTCTime -> Parser Card
infostring time = label "infostring" $ do
  _ <- string ":: "
  cardModifiers <- between (string "{") (string "}") (innerinfo `sepBy` symbol ",")
  let card = createCard time []
  return $ foldr (.) id cardModifiers card

innerinfo :: Parser (Card -> Card)
innerinfo =   try tierInfo
          <|> try lastCheckedInfo
          <|> offsetInfo
          <?> "inner info"

integer :: Parser Integer
integer = do
  sign <- (-1) <$ char '-' <|> 1 <$ char '+' <|> return 1
  digits <- many digitChar
  return $ sign * read digits

field :: String -> Parser Integer
field name = between (char '"') (char '"') (string name) >> symbol ":" >> integer

tierInfo :: Parser (Card -> Card)
tierInfo = do
  number <- field "level"
  let t = toEnum $ fromInteger number
  return (\card -> card {tier=t})

lastCheckedInfo :: Parser (Card -> Card)
lastCheckedInfo = do
  number <- field "last_checked"
  let lc = posixSecondsToUTCTime $ fromInteger number
  return (\card -> card {lastChecked=lc})

offsetInfo :: Parser (Card -> Card)
offsetInfo = do
  number <- field "delay"
  let o = integerToNom number
  return (\card -> card {offset=o})

-- sides of a card

side :: Parser String
side = unlines <$> (nonemptyLine `someTill` followedBy end)
  where end = void cardSeparator <|> void separator

pSides :: Parser [String]
pSides = side `sepBy1` (try $ cardSeparator >> newline)

-- an actual card

pCard :: UTCTime -> Parser Card
pCard time = cardInfo time <|> cardNoInfo time <?> "card"

cardInfo :: UTCTime -> Parser Card
cardInfo time = do
  Card{tier=t, lastChecked=lc, offset=o} <- infostring time
  _ <- newline
  s <- pSides
  return Card{sides=s, tier=t, lastChecked=lc, offset=o}

cardNoInfo :: UTCTime -> Parser Card
cardNoInfo time = do
  s <- pSides
  return $ createCard time s

-- an element

comment :: Parser Element
comment = do
  _ <- char '#'
  text <- nonemptyLine `manyTill` followedBy separator
  return $ EComment $ unlines text

element :: UTCTime -> Parser Element
element time = comment <|> (ECard <$> pCard time) <?> "element"

-- a bunch of elements

-- | A megaparsec parser parsing a list of elements in the format of the original python script.
--
-- Use this parser if you want nice error messages to display.
parseElements :: UTCTime -> Parser Elements
parseElements time = do
  elms <- many newline *> (element time `sepEndBy` some newline) <* label "end of file" eof
  return $ Elements $ Map.fromList $ zip [1..] elms

-- | The 'parseElements' parser, but simpler to use.
--
-- Use this when the user doesn't need to see any error messages.
parseElementsMaybe :: UTCTime -> String -> Maybe Elements
parseElementsMaybe time = parseMaybe (parseElements time)
