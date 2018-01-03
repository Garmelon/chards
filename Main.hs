module Main
  ( main
  ) where

import Cards
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.Time
import System.Console.Haskeline

inputSettings :: Settings IO
inputSettings = Settings
  { complete       = noCompletion
  , historyFile    = Nothing
  , autoAddHistory = True
  }

-- The prompt functions use a MaybeT wrapper because they can fail at any time.
-- This happens when the user presses ctrl+D (EOF).

-- Simple yes/no prompt (defaults to yes)
promptYesNo :: String -> MaybeT (InputT IO) Bool
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
promptContinue :: String -> MaybeT (InputT IO) ()
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

{-
 - Dealing with Elements/Cards.
 -}

-- Generic card counting function
countCardsBy :: (Card -> Bool) -> [Element] -> Int
countCardsBy f = length . filter elmF
  where elmF e = fromMaybe False (f <$> toCard e)

-- Ask all cards in the list of elements which are due.
-- When askNthCard fails, don't modify the rest of the list.
-- This bit uses two MaybeTs inside each other, so beware :P
askCountdown :: UTCTime -> [Element] -> InputT IO [Element]
askCountdown _ [] = return []
askCountdown time elms@(e:es) =
  defaultTo elms $ do
    result <- runMaybeT $ do
      card <- MaybeT $ return $ toCard e
      guard $ isDue time card
      card' <- lift $ askCardWithInfo time card (countCardsBy (isDue time) es)
      lift $ lift $ (fromCard card' :) <$> askCountdown time es
    case result of
      Nothing -> lift $ continue
      Just r  -> return r
  where defaultTo what monad = fromMaybe what <$> runMaybeT monad
        continue = (e :) <$> askCountdown time es

-- A simple right justify
rjust :: Char -> Int -> String -> String
rjust c l s = replicate (max 0 $ l - length s) c ++ s

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

learn :: [Element] -> InputT IO [Element]
learn elms = do
  time <- lift $ getCurrentTime
  askCountdown time elms

stats :: [Element] -> InputT IO ()
stats = undefined -- TODO: Use tierName

trim :: Char -> String -> String
trim c = dropWhile (== c) . reverse . dropWhile (== c) . reverse

run :: [Element] -> InputT IO [Element]
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
  mapM_ (putStrLn . show) elms
