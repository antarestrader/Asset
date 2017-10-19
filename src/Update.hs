{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Update where

import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Data.Aeson
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX
import GHC.Generics
import Data.Typeable
import Text.Printf

import Asset

data Update = Update {
    clock :: IO (Int)
  , updaters :: Map String (Int -> Value -> IO())
  , source :: Int -> IO(Maybe (String, Value))
  }

data UpdateAsset = UpdateAsset {
    uaIdent  :: Ident
  , updateAt :: Int
  , target   :: Reference
  } deriving (Generic, Typeable)

instance ToJSON   UpdateAsset
instance FromJSON UpdateAsset

instance Asset UpdateAsset where
  ident = uaIdent
  updateIdent i a = a{uaIdent=i}
  name a = printf "Update for '%s' at t=%i" (label $ target a) (updateAt a)

updateWorker :: Update -> IO(IO(){- end -},IO(){- runme -})
updateWorker u = f `fmap` newMVar True
  where
    f :: MVar Bool -> (IO(),IO())
    f mv = (halt mv, run mv)
    halt mv = swapMVar mv False >> return ()
    run :: MVar Bool -> IO()
    run mv = loop mv >>= (\c -> if c then run mv else return ())
    loop mv = withMVar mv $ \r-> do
      case r of
        False -> return False
        True -> do
          t <- clock u
          mkv <- source u t
          case mkv of
            Nothing -> threadDelay 100000 >> return True
            Just (k,v) -> case lookup k (updaters u) of
                Nothing -> return True
                Just f -> f t v >> return True


-- Clock Stuff --
newTicker :: IO(IO(Int),IO())
newTicker = f `fmap` newMVar 0
  where
    f mv = (readMVar mv,tick mv)
    tick mv = takeMVar mv >>= (\t -> putMVar mv (t+1))

data Clock = Paused Int
           | Running {start :: POSIXTime, offset :: Int}

newClock :: Double ->
            IO(
                 IO(Int) -- read
               , IO() -- stop / start
               )
newClock scale = f `fmap` newMVar (Paused 0)
  where
    f mv = (read mv, toggle mv)
    time curr start offset = round diff + offset
      where
       diff = realToFrac(curr-start) / scale
    read mv = do
      clock <- readMVar mv
      case clock of
        Paused i -> return i
        Running {..} -> do
          curr <- getPOSIXTime
          return $ time curr start offset
    toggle mv = do
      clock <- takeMVar mv
      case clock of
        Paused i -> do
          curr <- getPOSIXTime
          putMVar mv Running{start = curr, offset = i}
        Running {..} -> do
          curr <- getPOSIXTime
          putMVar mv $ Paused $ time curr start offset

