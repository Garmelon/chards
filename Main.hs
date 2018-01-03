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

{-
 - Dealing with Elements/Cards.
 -}

countDueCards :: UTCTime -> [Element] -> Int
countDueCards time elms = length $ filter isDueCard elms
  where isDueCard e = fromMaybe False (isDue time <$> toCard e)

askCountdown :: UTCTime -> Int -> [Element] -> InputT IO [Element]
askCountdown _ _ [] = return []
askCountdown time left elms@(e:es) = do
  case toCard e of
    Nothing   -> (e :) <$> askCountdown time left es
    Just card -> defaultTo elms $
      if isDue time card
        then do
          card' <- askNthCard time card (left - 1)
          (fromCard card' :) <$> liftedAsk time (left - 1) es
        else do
          (e :) <$> liftedAsk time left es
  where defaultTo what monad = fromMaybe what <$> runMaybeT monad
        liftedAsk t l e' = lift $ askCountdown t l e'

rjust :: Char -> Int -> String -> String
rjust c l s = replicate (max 0 $ l - length s) c ++ s

askNthCard :: UTCTime -> Card -> Int -> MaybeT (InputT IO) Card
askNthCard time card left = do
  let t = rjust ' ' 9 $ tierName $ tier card
      l = rjust ' ' 3 $ show left
  lift $ outputStrLn ""
  lift $ outputStrLn $ "-----< tier: " ++ t ++ ", left: " ++ l ++ " >-----"
  askCard time card

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
  askCountdown time (countDueCards time elms) elms

stats :: [Element] -> InputT IO ()
stats = undefined -- TODO: Use tierName

run :: [Element] -> InputT IO [Element]
run elms = do
  cmd <- getInputLine "%> "
  case (map toLower) <$> cmd of
    Nothing      -> return elms
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
