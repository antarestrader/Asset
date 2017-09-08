module Asset.MemoryStore where

import Prelude hiding (lookup)
import Control.Concurrent.MVar
import Data.Map (Map, lookup, insertWith, empty, delete)
import Data.Aeson
import Data.Maybe
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Asset hiding (AssetClass(..))

data Table a = Table { contents :: MVar (Map Int (MVar a)), idx :: MVar Int}

data MemoryStore = MS (MVar (Map String (Table Value)))

newMemoryStore = MS <$> newMVar empty

newTable :: IO (Table a)
newTable = do
  mp <- newMVar empty
  i <- newMVar 0
  return $ Table {contents = mp, idx=i}

nextIndex :: Table a -> IO Int
nextIndex tbl = modifyMVar (idx tbl) (\i -> return (i+1,i)) 

newRow ::(Ord k) => k -> v -> (MVar (Map k v)) -> IO()
newRow k v mv = modifyMVar_ mv (return . insertWith (flip const) k v)

lookup' :: (Ord k) => k -> MVar (Map k v) -> IO (Maybe v)
lookup' k mv = lookup k `fmap` readMVar mv

lookupRef :: Reference -> MemoryStore -> IO (Maybe (MVar Value))
lookupRef ref (MS mv1) =  runMaybeT $ do
  tbl <- MaybeT $ lookup' (assetType ref) mv1
  MaybeT $ lookup' (assetIndex ref) (contents tbl) 


instance AssetStore MemoryStore where
  loadAsset ms ref = do
    mmv <- lookupRef ref ms
    case mmv of
      Nothing -> return $ Left "Not Found"
      Just mv2 -> do
        val <- readMVar mv2
        case fromJSON val of 
          Error s -> return $ Left s
          Success a -> return $ Right a

  storeAsset (MS mv1) a =do 
    case ident a of
      Ident Nothing -> do
        ms <- readMVar mv1
        case lookup (show $ typeOf a) ms of
            Nothing -> do
              tbl <- newTable
              newRow (show $ typeOf a) tbl mv1
              storeAsset (MS mv1) a
            Just tbl -> do
              i <- nextIndex tbl
              let a' = setIdent i a
              ma' <- newMVar (toJSON a')
              newRow i ma' (contents tbl)
              return $ Just a'
      Ident (Just ref) -> runMaybeT $ do
        mv <- MaybeT $ lookupRef ref (MS mv1)
        liftIO $ modifyMVar mv (\_ -> return (toJSON a,a))

  deleteRef (MS mv1) ref = fmap isJust $ runMaybeT $ do
    tbl <- MaybeT $ lookup' (assetType ref) mv1
    let ix = assetIndex ref
    MaybeT $ modifyMVar (contents tbl) (\mp -> return (delete ix mp,lookup ix mp) )
      
    

