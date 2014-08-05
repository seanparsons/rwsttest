import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Concurrent
import Control.Applicative
import Data.Time.Clock
import Control.Concurrent.Async
import Data.Functor.Identity

data Database = Database { databaseName :: String } deriving (Show)

data User = User { userId :: Int, userName :: String } deriving (Show)

type ReqRWST m a = RWST Database [String] (Map.Map Int User) m a

loadUser :: Int -> ReqRWST IO User
loadUser userId = do
  let name = "User " ++ (show userId)
  Database(dbName) <- ask
  liftIO $ threadDelay 3000000
  tell ["Loading user " ++ name ++ " from " ++ dbName]
  return $ User userId name

getUser :: Int -> ReqRWST IO User
getUser userId = do
  state <- get
  let cachedUser = Map.lookup userId state
  user <- maybe (loadUser userId) (\cached -> return cached) cachedUser
  modify' (Map.insert userId user)
  return user

sendPush :: Int -> ReqRWST IO ()
sendPush userId = do
  user <- getUser userId
  liftIO $ threadDelay 2000000
  liftIO $ putStrLn $ "Sent push to " ++ (show user)

sendEvent :: Int -> ReqRWST IO ()
sendEvent userId = do
  user <- getUser userId
  liftIO $ threadDelay 2000000
  liftIO $ putStrLn $ "Event for " ++ (show user)

userAction :: Int -> ReqRWST IO User
userAction userId = do
  pushSend <- sendPush userId
  eventSend <- sendEvent userId
  getUser userId

runAction :: String -> ReqRWST IO User -> IO ()
runAction title request = do
  liftIO $ putStrLn title
  startTime <- liftIO $ getCurrentTime
  (result, stateResult, writerResult) <- runRWST request (Database "userdb") Map.empty
  endTime <- liftIO $ getCurrentTime
  liftIO $ putStrLn $ "Retrieved " ++ (show result) ++ " in " ++ (show $ diffUTCTime endTime startTime)
  liftIO $ putStrLn "State Result:"
  liftIO $ print stateResult
  liftIO $ putStrLn "Writer Result:"
  liftIO $ print writerResult

main :: IO ()
main = do
  runAction "=====EXAMPLE======" $ userAction 1
--  runAction "====SEQUENTIAL====" $ userAction 1
--  runAction "=====PARALLEL=====" $ userAction 2
