{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, GADTs #-}

module Asset where

import Data.Aeson
import Data.Either
import Data.Typeable
import Data.Kind
import GHC.Generics
import Control.Monad.Reader

data Reference = Ref {
    assetType:: String
  , assetIndex :: Int
  , label :: String
  } deriving (Eq, Show, Generic)

newtype Ident = Ident (Maybe Reference)
  deriving (Eq, Show, Generic)

instance ToJSON Reference
instance FromJSON Reference
instance ToJSON Ident
instance FromJSON Ident

class (Typeable a,ToJSON a, FromJSON a) => Asset a where
  ident :: a -> Ident
  updateIdent :: Ident -> a -> a
  name :: a -> String

data SortOrder = Ascending | Descending

data Query where
 All   :: Query
 First :: Query
 Field :: String -> Value -> Query
 Sort  :: [(String, SortOrder)] -> Query -> Query

newIdent = Ident Nothing

setIdent :: Asset a => Int -> a -> a
setIdent i a = updateIdent (Ident $ Just (Ref (show $ typeOf a) i (name a))) a 

isNew :: Asset a => a -> Bool
isNew a = case (ident a) of
  Ident Nothing -> True
  otherwise -> False

ref :: Asset a => a -> Reference
ref a = case (ident a) of
  Ident Nothing  -> error "new value"
  Ident (Just r) -> r

class AssetStore s where
  loadAsset     :: Asset a => s -> Reference -> IO (Either String a)
  loadAllAssets :: Asset a => s -> [Reference] -> IO ([String], [a])
  loadAllAssets s xs = fmap partitionEithers (mapM (loadAsset s) xs)
  findAsset     :: Asset a => s -> Query -> IO [a]
  storeAsset    :: Asset a => s -> a -> IO (Maybe a)
  deleteAsset   :: Asset a => s -> a -> IO Bool
  deleteAsset ms a = case ident a of
    Ident Nothing    -> return False
    Ident (Just ref) -> deleteRef ms ref
  deleteRef     :: s -> Reference -> IO Bool

class (Monad m) => AssetClass m where
  load   ::  Asset a => Reference -> m (Either String a)
  find   ::  Asset a => Query -> m [a]
  store  ::  Asset a => a -> m (Maybe a)
  delete ::  Asset a =>  a -> m Bool
  remove ::  Reference -> m Bool

newtype AssetT as m  a = AssetM { unAsset :: ReaderT as m a} 
  deriving (Functor, Applicative, Monad, MonadReader as, MonadIO)

type AssetM as = AssetT as IO

instance (AssetStore as, MonadIO m) => AssetClass (AssetT as m ) where
  load r = do
    s <- ask
    liftIO $ loadAsset s r
  find q = do
    s <- ask
    liftIO $ findAsset s q
  store a = do
    s <- ask
    liftIO $ storeAsset s a
  delete a = do
    s <- ask
    liftIO $ deleteAsset s a
  remove r = do
    s <- ask
    liftIO $ deleteRef s r

runAssetT :: (AssetStore as, Monad m) =>  AssetT as m a -> as -> m a
runAssetT = runReaderT . unAsset

runAssetM :: AssetStore as => as -> AssetM as a -> IO a
runAssetM = flip runAssetT
