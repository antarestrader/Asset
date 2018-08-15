{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, GADTs, 
    RecordWildCards, RankNTypes, OverloadedStrings #-}

module Asset (
    Reference (..)
  , SomeAsset (..)
  , withSomeAsset
  , Asset (..)
  , AssetStore (..)
  , AssetClass (..)
  , Void
  , voidReference
  , Ident (..)
  , newIdent
  , setIdent
  , getIdent
  , SortOrder (..)
  , Comparison (..)
  , Query (..)
  , isNew
  , ref
  , AssetT
  , AssetM
  , runAssetT
  , runAssetM
  )  where

import Data.Aeson
import Data.Either
import Data.Monoid ((<>))
import Data.Typeable
import Data.Kind
import GHC.Generics
import Control.Monad.Reader

data Reference a = Ref {
    assetType:: String
  , assetIndex :: Int
  , label :: String
  } deriving (Show, Generic)

instance Eq (Reference a) where
  a == b = (assetType a == assetType b) && (assetIndex a == assetIndex b)

instance Ord (Reference a) where
  compare a b = compare (assetType a) (assetType b) <> compare (assetIndex a) (assetIndex b)

data SomeAsset
  where Some :: Asset a => a -> SomeAsset

withSomeAsset :: (forall a. Asset a => a -> r) -> (SomeAsset -> r)
withSomeAsset f (Some a) = f a

data Void = Void

voidReference :: Reference a -> Reference Void
voidReference (Ref {..}) = Ref {..}

newtype Ident a = Ident (Maybe  (Reference a))
  deriving (Eq, Show, Generic)

instance ToJSON (Reference a) where
  toJSON = object . set
  toEncoding = pairs . mconcat . set

set :: (KeyValue kv) => Reference a -> [kv]  
set r = [ "type"  .= assetType r
        , "index" .= assetIndex r
        ,  "label" .= label r
        ]

instance FromJSON (Reference a) where
  parseJSON = withObject "Reference" $ \v -> Ref
    <$> v .:  "type"
    <*> v .:  "index"
    <*> v .:? "label" .!= ""

instance ToJSON (Ident a)
instance FromJSON (Ident a)

class (Typeable a,ToJSON a, FromJSON a) => Asset a where
  ident :: a -> Ident a
  updateIdent :: Ident a -> a -> a
  name :: a -> String

data SortOrder = Ascending | Descending

data Comparison a =
    GT a | LT a | EQ a | GTE a | LTE a
  | AND (Comparison a) (Comparison a)
  | OR  (Comparison a) (Comparison a)

data Query where
 All    :: Query
 First  :: Query -> Query
 Field  :: String -> Value -> Query
 Sort   :: [(String, SortOrder)] -> Query -> Query
 Filter :: [(String, Comparison Value)] -> Query

newIdent = Ident Nothing

setIdent :: Asset a => Int -> a -> a
setIdent i a = updateIdent (Ident $ Just (Ref (show $ typeOf a) i (name a))) a

getIdent :: Asset a => a -> Int
getIdent a = assetIndex $ ref a

isNew :: Asset a => a -> Bool
isNew a = case (ident a) of
  Ident Nothing -> True
  otherwise -> False

ref :: Asset a => a -> Reference a
ref a = case (ident a) of
  Ident Nothing  -> error "new value"
  Ident (Just r) -> r

class AssetStore s where
  loadAsset     :: Asset a => s -> Reference a -> IO (Either String a)
  loadAssetGeneric :: Asset a => (a -> b) -> s -> Reference Void ->  IO (Either String b)
  loadAllAssets :: Asset a => s -> [Reference a] -> IO ([String], [a])
  loadAllAssets s xs = fmap partitionEithers (mapM (loadAsset s) xs)
  findAsset     :: Asset a => s -> Query -> IO [a]
  storeAsset    :: Asset a => s -> a -> IO (Maybe a)
  deleteAsset   :: Asset a => s -> a -> IO Bool
  deleteAsset ms a = case ident a of
    Ident Nothing    -> return False
    Ident (Just ref) -> deleteRef ms ref
  deleteRef     :: s -> Reference a -> IO Bool

class (Monad m) => AssetClass m where
  load   ::  Asset a => Reference a -> m (Either String a)
  find   ::  Asset a => Query -> m [a]
  store  ::  Asset a => a -> m (Maybe a)
  delete ::  Asset a =>  a -> m Bool
  remove ::  Reference a -> m Bool

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
