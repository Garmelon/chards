module Main
  ( main
  ) where

import Cards
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.Time
import System.Console.Haskeline

type Input = InputT IO

inputSettings :: Settings IO
inputSettings = Settings
  { complete       = noCompletion
  , historyFile    = Nothing
  , autoAddHistory = True
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
promptContinue question = void $ MaybeT $ getInputLine $ question ++ "[Enter] "

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

-- A simple right justify
rjust :: Char -> Int -> String -> String
rjust c l s = replicate (max 0 $ l - length s) c ++ s

-- Trims characters from the front and back of a string.
trim :: Char -> String -> String
trim c = dropWhile (== c) . reverse . dropWhile (== c) . reverse

{-
 - Dealing with Elements/Cards.
 -}

askElements :: UTCTime -> Elements -> Input Elements
askElements time elms = do
  let l = toDueCards time elms
  -- TODO: Randomize order
  newCards <- askCountdown time l
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
askCardWithInfo :: UTCTime -> Card -> Int -> MaybeT (InputT IO) Card
askCardWithInfo time card left = do
  let t = rjust ' ' 9 $ tierName $ tier card
      l = rjust ' ' 3 $ show left
  lift $ outputStrLn ""
  lift $ outputStrLn $ "-----< tier: " ++ t ++ ", left: " ++ l ++ " >-----"
  askCard time card

-- Ask the sides on a card and reset or update the card accordingly
-- Doesn't check whether the card is due or not.
askCard :: UTCTime -> Card -> MaybeT (InputT IO) Card
askCard time card = do
  (_, unasked) <- spanM askSide $ sides card
  mapM_ showSide $ drop 1 unasked
  if null unasked
    then lift $ lift $ update time card
    else return $ reset time card

askSide :: String -> MaybeT (InputT IO) Bool
askSide side = do
  lift $ displaySide side
  promptYesNo "Did you know that side?"

showSide :: String -> MaybeT (InputT IO) ()
showSide side = do
  lift $ displaySide side
  promptContinue "Continue"

displaySide :: String -> InputT IO ()
displaySide side = lift (putStrLn side)

{-
 - User prompt.
 -}

learn :: Elements -> InputT IO Elements
learn elms = do
  time <- lift $ getCurrentTime
  askElements time elms

stats :: Elements -> InputT IO ()
stats = undefined -- TODO: Use tierName

run :: Elements -> InputT IO Elements
run elms = do
  cmd <- getInputLine "%> "
  case trim ' ' . map toLower <$> cmd of
    Nothing      -> return elms
    Just ""      -> run elms
    Just "quit"  -> return elms
    Just "q"     -> return elms
    Just "learn" -> learn elms >>= run
    Just "l"     -> learn elms >>= run
    Just "show"  -> stats elms >> run elms
    Just "s"     -> stats elms >> run elms
    Just x       -> do
      outputStrLn $ "Unknown command " ++ show x ++ "."
      run elms
    -- Maybe save cards?

main :: IO ()
main = do
  elms <- runInputT inputSettings $ run testElements
  putStrLn $ show elms