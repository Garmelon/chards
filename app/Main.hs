module Main
  ( main
  ) where

import           Cards
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.Time
import           System.Console.Haskeline
import           System.Console.Haskeline.History
import           System.Environment
import           System.Random.Shuffle
import qualified Text.Megaparsec                  as Mega

type Input = InputT IO

inputSettings :: Settings IO
inputSettings = Settings
  { complete       = noCompletion
  , historyFile    = Nothing
  , autoAddHistory = False
  }

{-
 - Helper functions
 -}

-- The prompt functions use a MaybeT wrapper because they can fail at any time.
-- This happens when the user presses ctrl+D (EOF).

-- Simple yes/no prompt (defaults to yes)
promptYesNo :: String -> MaybeT Input Bool
promptYesNo question = do
  i <- MaybeT $ getInputLine $ question ++ " [Y/n] "
  case map toLower i of
    ""  -> return True
    "y" -> return True
    "n" -> return False
    _   -> do
      lift $ outputStrLn $ "Incorrect input: " ++ show i
      promptYesNo question

-- Wait until user pressed Enter
promptContinue :: String -> MaybeT Input ()
promptContinue question = void $ MaybeT $ getInputLine $ question ++ " [Enter] "

-- Just span, but with monads.
spanM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ [] = return ([], [])
spanM f l@(x:xs) = do
  result <- f x
  if result
    then do
      (with, without) <- spanM f xs
      return (x:with, without)
    else do
      return ([], l)

-- A few inefficient string formatting functions

-- Simple right justify
rjust :: Char -> Int -> String -> String
rjust c l s = replicate (max 0 $ l - length s) c ++ s

-- Simple left justify
--ljust :: Char -> Int -> String -> String
--ljust c l s = s ++ replicate (max 0 $ l - length s) c

-- Trims characters from the front and back of a string.
trim :: Char -> String -> String
trim c = dropWhile (== c) . reverse . dropWhile (== c) . reverse

{-
 - Dealing with Elements/Cards.
 -}

askElements :: UTCTime -> Elements -> Input Elements
askElements time elms = do
  cards <- lift $ shuffleM $ toDueCards time elms
  newCards <- askCountdown time cards
  return $ updateElements elms (fromCards newCards)

askCountdown :: UTCTime -> [(Integer, Card)] -> Input [(Integer, Card)]
askCountdown _ [] = return []
askCountdown time l@((key, card):xs) = do
  result <- runMaybeT $ askCardWithInfo time card (length xs)
  case result of
    Nothing    -> return l
    Just card' -> ((key, card') :) <$> askCountdown time xs

-- These functions use a MaybeT wrapper because they can fail at any time,
-- because they use the prompt functions.

-- Print out info about a card when asking it
askCardWithInfo :: UTCTime -> Card -> Int -> MaybeT Input Card
askCardWithInfo time card left = do
  let t = rjust ' ' 9 $ tierName $ tier card
      l = rjust ' ' 5 $ show left
  lift $ outputStrLn ""
  lift $ outputStrLn ""
  lift $ outputStrLn $ "─────< tier: " ++ t ++ ", left: " ++ l ++ " >─────"
  askCard time card

-- Ask the sides on a card and reset or update the card accordingly
-- Doesn't check whether the card is due or not.
askCard :: UTCTime -> Card -> MaybeT Input Card
askCard time card = do
  (_, unasked) <- spanM askSide $ sides card
  mapM_ showSide $ drop 1 unasked
  if null unasked
    then lift $ lift $ update time card
    else return $ reset time card

askSide :: String -> MaybeT (InputT IO) Bool
askSide side = do
  lift $ displaySide side
  promptYesNo "──> Did you know that side?"

showSide :: String -> MaybeT (InputT IO) ()
showSide side = do
  lift $ displaySide side
  promptContinue "──> Continue"

displaySide :: String -> InputT IO ()
displaySide side = outputStr side

{-
 - Display stats
 -}

count :: (Card -> Bool) -> Elements -> Int
count f = length . filter f . map snd . toCards

countTier :: Elements -> Tier -> Int
countTier e t = count (\card -> tier card == t) e

printBar :: Int -> Int -> String
printBar maxInt int =
  let l = (30 * int) `div` maxInt
      s = replicate l '█'
  in rjust ' ' 30 s

printLine :: Int -> String -> Int -> String
printLine maxAmount name amount =
  rjust ' ' 9 name ++ " │ " ++
  printBar maxAmount amount ++ " │ " ++ rjust ' ' 6 (show amount)

{-
 - User prompt.
 -}

learn :: Elements -> Input Elements
learn elms = do
  time <- lift $ getCurrentTime
  askElements time elms

stats :: Elements -> Input ()
stats elms = do
  time <- lift $ getCurrentTime
  outputStrLn $ "     tier │              graph             │ amount"
  outputStrLn $ "──────────┼────────────────────────────────┼───────"
  let total = length $ toCards elms
      due   = length $ toDueCards time elms
      maxAmount = maximum $ due : map (countTier elms) [minBound..maxBound]
  mapM_ (outputStrLn . printTierLine maxAmount) [minBound..maxBound]
  outputStrLn $ "──────────┼────────────────────────────────┼───────"
  outputStrLn $ "    total │                                │ " ++ rjust ' ' 6 (show total)
  outputStrLn $ printLine maxAmount "learn" due
  where
    printTierLine m t = printLine m (tierName t) (countTier elms t)

help :: Input ()
help = do
  outputStrLn "List of commands:"
  outputStrLn "  h, help  -> display this help"
  outputStrLn "  l, learn -> start revising cards (press ctrl+D to exit)"
  outputStrLn "  q, quit  -> exit program"
  outputStrLn "  s, show  -> show how many cards are in which tiers"

run :: Elements -> Input Elements
run elms = do
  cmd <- getInputLine "%> "
  let logCmd command = modifyHistory $ addHistoryUnlessConsecutiveDupe command
  case trim ' ' . map toLower <$> cmd of
    Nothing      -> return elms
    Just ""      -> run elms
    Just "quit"  -> logCmd "quit" >> return elms
    Just "q"     -> logCmd "quit" >> return elms
    Just "learn" -> logCmd "learn" >> learn elms >>= run
    Just "l"     -> logCmd "learn" >> learn elms >>= run
    Just "show"  -> logCmd "show" >> stats elms >> run elms
    Just "s"     -> logCmd "show" >> stats elms >> run elms
    Just "help"  -> logCmd "help" >> help >> run elms
    Just "h"     -> logCmd "help" >> help >> run elms
    Just x       -> do
      outputStrLn $ "Unknown command " ++ show x ++ ". Try \"help\" for a list of commands."
      run elms

fromFile :: FilePath -> Input ()
fromFile filepath = do
  time <- lift $ getCurrentTime
  content <- lift $ readFile filepath
  let result = Mega.parse (parseElements time) filepath content
  case result of
    Left parseError -> outputStrLn $ Mega.parseErrorPretty parseError
    Right elms      -> do
      newElms <- run elms
      toFile filepath newElms

toFile :: FilePath -> Elements -> Input ()
toFile filepath elms = void $ runMaybeT $ do
  result <- promptYesNo "Save the cards?"
  when result $ do
    lift $ lift $ writeFile filepath $ elementsToString elms

main :: IO ()
main = runInputT inputSettings $ do
  name <- lift $ getProgName
  args <- lift $ getArgs
  if length args == 1
    then fromFile (args !! 0)
    else do
      outputStrLn $ "  USAGE: " ++ name ++ " <cards file>"
      void $ runMaybeT $ promptContinue "Continue"
