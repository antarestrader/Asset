{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Update 
  ( Update(..)
  , UpdateAsset(..)
  , updateWorker
  )
where

import Prelude hiding (lookup)
import Data.Aeson
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import GHC.Generics
import Data.Typeable
import Text.Printf

import Asset 

data Update as = Update {
    clock       :: IO (Int)
  , assetStore  :: as
  , eval        :: as -> Reference -> Int -> String -> IO()
  , except      :: SomeException -> IO()
  }

data UpdateAsset = UpdateAsset {
    uaIdent  :: Ident
  , updateAt :: Int
  , target   :: Reference
  , action   :: String
  } deriving (Generic, Typeable)

instance ToJSON   UpdateAsset
instance FromJSON UpdateAsset

instance Asset UpdateAsset where
  ident = uaIdent
  updateIdent i a = a{uaIdent=i}
  name a = printf "Update for '%s' at t=%i" (label $ target a) (updateAt a)

updateWorker :: AssetStore as 
             => Update as 
             -> IO(IO(){- end -},IO(){- runme -})
updateWorker u = f `fmap` newMVar True
  where
    f :: MVar Bool -> (IO(),IO())
    f mv = (halt mv, run mv)
    halt mv = swapMVar mv False >> return ()
    run :: MVar Bool -> IO()
    run mv = loop mv >>= (\c -> if c then run mv else return ())
    source u t = findAsset (assetStore u) (Filter [("updateAt", LTE (toJSON t))])
    loop mv = withMVar mv $ \r-> do
      case r of
        False -> return False
        True -> do
          t <- clock u
          xs <- source u t
          case xs of
            [] -> threadDelay 100000 >> return True
            xs -> do mapM_ 
                       (\x->handle 
                              (except u) 
                              (eval u (assetStore u) (target x) t (action x))
                       ) 
                       xs
                     return True 
