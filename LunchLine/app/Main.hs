{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Database.Persist.Sqlite hiding (get)
import Control.Monad.Reader
import Control.Monad.State
import Database.Persist.TH
import Control.Monad.Logger
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental hiding (get, (==.))
import Data.Time.Clock
import Data.Time.Calendar.WeekDate

-- REMAINING
-- Change state to instead be a new table that stores a budget for each week

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
  amount Int
  year Int
  week Int
  day Int
  deriving Show

WeeklyBudget
  year Int
  week Int
  amount Int
  deriving Show
|] -- LineItem is an arbitrary type

-- 1 for Monday, 7 for Sunday
resetDay :: Int
resetDay = 1

newtype AppM a = AppM (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

data Env = Env { envConn :: SqlBackend }

runAppM :: MonadIO m => Env -> AppM a -> m a
runAppM env (AppM x) = liftIO $ runReaderT x env

runDB :: ReaderT SqlBackend IO a -> AppM a
runDB body = do
  conn <- asks envConn
  liftIO $ runSqlConn body conn

spend :: Int -> LineItem -> Int
spend n e = n - (lineItemAmount e)

getLineItemTotal :: (MonadIO m) => SqlPersistT m Int
-- This function BUILDS a query, and appMain runs it with the runDB call
getLineItemTotal = selectSum $ do
  items <- from $ table @LineItem -- table doesn't take arguments, so you provide a type application to specify
  -- coalesceDefault takes in a list of sql expressions and a default value and automatically gives default in Nothing case
  pure $ coalesceDefault [sum_ $ items ^. LineItemAmount] (val 0) -- LineItemAmount 
 where
  -- Zeroes are for Nothing cases - if we have a Nothing in the case of the two Maybes, the sum is zero
  selectSum = fmap (maybe 0 unValue) . selectOne
  -- Note: in Haskell >= 9, due to simplified subsumption, you will need to do replace selectOne with (\q -> selectOne q)

getLineItemTotalWeek :: (MonadIO m) => Int -> Int -> SqlPersistT m Int
getLineItemTotalWeek year week = do
    items <- selectList [LineItemYear ==. year, LineItemWeek ==. week] []
    let totalAmount = sum $ map (lineItemAmount . entityVal) items
    return totalAmount

getCurrentWeek :: Int -> IO (Int, Int, Int)
getCurrentWeek resetDay = do
    currentDay <- getCurrentTime
    let (year, week, day) = toWeekDate $ utctDay currentDay
    let adjustedWeek = if day < resetDay then week - 1 else week
    return (fromIntegral year, adjustedWeek, resetDay)

setBudget :: Int -> AppM ()
setBudget budget = do
  (year, week, _) <- liftIO $ getCurrentWeek resetDay
  runDB $ do
    deleteWhere [WeeklyBudgetYear ==. year, WeeklyBudgetWeek ==. week]
    insert_ $ WeeklyBudget year week budget
  liftIO . putStrLn $ "Set budget to " <> show budget

viewBudget :: AppM ()
viewBudget = do
  (year, week, _) <- liftIO $ getCurrentWeek resetDay
  total <- runDB $ do
    getLineItemTotalWeek year week
  budget <- runDB $ do
    maybeWeeklyBudget <- selectFirst [WeeklyBudgetYear ==. year, WeeklyBudgetWeek ==. week] []
    case maybeWeeklyBudget of
      Nothing -> return 0
      Just weeklyBudget -> return $ weeklyBudgetAmount $ entityVal weeklyBudget
  let remainingBudget = budget - total
  liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget

addLineItem :: String -> Int -> AppM ()
addLineItem label price = do
  (year, week, day) <- liftIO $ getCurrentWeek resetDay
  runDB $ insert_ $ LineItem label price year week day
  liftIO . putStrLn $ "Added new transaction: " <> label <> " for " <> show price

viewLineItems :: AppM ()
viewLineItems = do
  lineItems <- runDB $ do
    selectList @LineItem @_ @IO [] []
  liftIO $ print lineItems

data Command
  = AddLineItem String Int
  | ViewLineItems
  | SetBudget Int
  | ViewBudget
  | Exit
  deriving Show

parseCommand :: String -> Maybe Command
parseCommand command = case words command of
  ["add",label,price] -> Just (AddLineItem label (read price))
  ["view"] -> Just ViewLineItems
  ["set",budget] -> Just (SetBudget (read budget))
  ["budget"] -> Just ViewBudget
  ["exit"] -> Just Exit
  _ -> Nothing

runCommand :: Command -> AppM ()
runCommand Exit = pure ()
runCommand ViewBudget = viewBudget
runCommand ViewLineItems = viewLineItems
runCommand (SetBudget budget) = setBudget budget
runCommand (AddLineItem label price) = addLineItem label price

appMain :: AppM()
appMain = do
  command <- liftIO $ getLine
  case parseCommand command of
    Nothing -> liftIO (putStrLn "Invalid command.") >> appMain
    Just Exit -> pure ()
    Just parsedCommand -> runCommand parsedCommand >> appMain

main :: IO ()
main =
  runNoLoggingT $
    withSqliteConn ":memory:" $ \conn -> do -- :memory: means it writes to memory, anything else will be a sqlite file
      runAppM (Env conn) (runDB (runMigration migrateAll) >>= \_ -> appMain)

  -- case fst words command
  -- main should load up config, database, etc
  -- AppMain is what should run the actual app

-- ReaderT Env IO a ~~ Env -> IO a
-- StateT s IO a ~~ s -> IO (a, s) -- evalStateT -- s -> IO a
-- StateT Int (ReaderT Env IO) a -- evalStateT -- s -> ReaderT Env IO a
-- x is StateT Int (ReaderT Env IO) a

-- OLD CODE
-- body' :: ReaderT SqlBackend IO a
-- body' = undefined

-- runDB' :: AppM a
-- runDB' = do
--   liftIO $ flip runReaderT () $ do
--     (pure 5 :: Maybe Int) -- FAILS BECAUSE MONAD IS WRONG

--   (asks envConn >>= (\conn -> liftIO $ runSqlConn body' conn) :: AppM a)


-- OLD CODE
-- appMain :: AppM ()
-- appMain = do
--   total <- runDB $ do
--     insert_ $ LineItem "Pizza" 11
--     insert_ $ LineItem "Burger" 12
--     getLineItemTotal
--   let remainingBudget = weeklyBudget - total
--   liftIO .  putStrLn $ "Remaining Budget: " <> show remainingBudget

-- make commands their own data types (Command data type)
-- write something that parses the commands


-- ask :: MonadReader r m => m r
-- asks :: MonadReader r m => (r -> a) -> m a
-- envConn :: r -> a ~ Env -> SqlBackend
-- conn :: SqlBackend
-- body :: ReaderT SqlBackend IO a

-- Intuition:
-- Read -> Eval -> Print -> Loop
-- enter command -> evaluate it -> print the output -> allow for next command
-- enter a new transaction
-- view existing budget
-- set budget
-- last thing: reset budget weekly
-- optional: daily, total, aggregate spend

-- v2
-- appMain :: AppM ()
-- appMain = do
--   lineItems <- runDB $ do
--     -- removed migrateAll since we've added it to main
--     insert_ $ LineItem "Pizza" 11
--     insert_ $ LineItem "Burger" 12
--     selectList @LineItem [] []
--   let remainingBudget = foldl' spend weeklyBudget $ map entityVal lineItems -- entityVal turns the entity into vals, lineItems are [Entity LineItem] so we convert to [LineItem]
--   liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget

-- v1
-- appMain :: AppM ()
-- appMain = do
--   lineItems <- runDB $ do
--     insert_ $ LineItem "Pizza" 11 -- insert_ just returns unit / nothing as opposed to the id returned by insert
--     insert_ $ LineItem "Burger" 12
--     selectList @LineItem @_ @IO [] [] -- args: first are filters, and second are ops like ascending, descending, etc
--     -- Why does this @LineItem work? forall accepts three hidden arguments: the type applications of the record, backend, and monad
--     -- By passing @LineItem to selectList, we specify the type of record, which is LineItem, and thus print doesn't need it
--   liftIO $ print lineItems
