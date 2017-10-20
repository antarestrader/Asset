{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module    : Update
Description : Scheduled updated of assets
Copyright   : John F. Miller, 2016-2017
Maintainer  : john@jfmjourney.com
Stability   : development

While the Asset module addresses the storing and retrieving of assets from
some type of store, this module addresses the common need to modify those
assets at some point in the future.  This module allow a flexable scheduling
of updates to target assets.

There are two parts needed to accomplish this task: 1) a way to schedule an
update, and 2) a worker thread that will perform the update at the appropriate
time. There are done with the 'schedule' and 'updateWorker' functions
respectivally.
-}
module Update
  ( Update(..)
  , UpdateAsset(..)
  , updateWorker
  , schedule
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

-- | The structure needed to run an update worker
data Update as = Update {
    -- | an IO action which returns the current time as an Int. Time must
    --   never decrease. This is not necessarly a length in seconds.  The
    --   amount of time per turn can be scaled based on the needs of the
    --   application.
    clock       :: IO (Int)
    -- | the 'AssetStore' where target values are found and 'UpdateAsset's are
    --   stored.
  , assetStore  :: as
    -- | A function to update the target asset
  , eval        :: as -> Reference -> Int -> String -> IO()
    -- | This function will be called if 'eval' generates an error.
  , except      :: SomeException -> IO()
  }

-- | An asset describing a scheduled update.  Generally use 'schedule' instead
--   of directly building an UpdateAsset.
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

-- | This function builds the thread responsible for running updated at the
--   appropriate time. To start multiple workers, call this function multiple
--   times.  If the same action is run in multiple threaded the threads will
--   block each other.
--
--   The function takes an 'Update' structure and returns a tuple of two
--   actions.  The first action is the @halt@ action which will stop the update
--   process and cause it to return.  The second is the process action itself.
--   The assumption is that this action would be kicked off in its own thread.
--   It will not terminate until the @halt@ action is called.
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


-- | Schedule and update within a 'AssetClass' monad.
schedule :: (Asset a, AssetClass m)
         => a -- ^ The target of this update. It must be an 'Asset'.
         -> Int -- ^ Run the update at or after this time.
         -> String
            -- ^ An extra string argument to describe how the assed shoul be
            --   updated.  This allows the same target to be updated in
            --   multiple ways.
         -> m (Maybe Reference)
            -- ^ if successful, a reference to the update that can be used to
            --   cancel the action. (Simply 'remove' the reference)
schedule asset time action = do
  ua <- store (UpdateAsset
    { uaIdent  = newIdent
    , updateAt = time
    , target = ref asset
    , action = action
    })
  return (ref <$> ua)
