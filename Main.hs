module Main
  ( main
  ) where

import Cards
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Time
import System.Console.Haskeline

inputSettings = Settings
  { complete       = noCompletion
  , historyFile    = Nothing
  , autoAddHistory = True
  }

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

promptContinue :: String -> MaybeT (InputT IO) ()
promptContinue question = void $ MaybeT $ getInputLine $ question ++ "[Enter] "

{-
 - General functions for functions operating on lists within monads.
 -}

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

-- A combination of span and map, but with monads.
-- Note the line-by-line similarity to spanM.
-- Basically like spanM, but instead of splitting the list by a Bool, splits it
-- by a Maybe.
spanJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m ([b], [a])
spanJustM _ [] = return ([], [])
spanJustM f l@(x:xs) = do
  result <- f x
  case result of
    Just r -> do
      (just, nothing) <- spanJustM f xs
      return (r:just, nothing)
    Nothing -> do
      return ([], l)

-- Basically spanJustM, but more similar to map.
mapWhileJustM :: (Monad m) => (a -> m (Maybe a)) -> [a] -> m [a]
mapWhileJustM f l = uncurry (++) <$> spanJustM f l

{-
 - Dealing with Elements/Cards.
 -}

askElement :: UTCTime -> Element -> MaybeT (InputT IO) Element
askElement time elem =
  case fromElement elem of
    Just card -> toElement <$> askCard time card
    Nothing   -> return elem

askCard :: UTCTime -> Card -> MaybeT (InputT IO) Card
askCard time card = do
  if isDue time card
    then do
      (asked, unasked) <- spanM askSide $ sides card
      mapM_ showSide $ drop 1 unasked
      if null unasked
        then lift $ lift $ update time card
        else return $ reset card
    else do
      return card

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

learn :: UTCTime -> [Element] -> InputT IO [Element]
learn time = mapWhileJustM (runMaybeT . askElement time)

stats :: [Element] -> InputT IO ()
stats = undefined -- TODO: Use tierName

run :: UTCTime -> [Element] -> InputT IO [Element]
run time elem = do
  cmd <- getInputLine "%> "
  case (map toLower) <$> cmd of
    Nothing      -> return elem
    Just "quit"  -> return elem
    Just "q"     -> return elem
    Just "learn" -> learn time elem >>= run time
    Just "l"     -> learn time elem >>= run time
    Just "show"  -> stats elem >> run time elem
    Just "s"     -> stats elem >> run time elem
    Just x       -> do
      outputStrLn $ "Unknown command " ++ show x ++ "."
      run time elem
    -- Maybe save cards?

main :: IO ()
main = do
  time <- getCurrentTime
  elems <- runInputT inputSettings $ run time testElements
  mapM_ (putStrLn . show) elems
