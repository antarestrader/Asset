{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Asset.MemoryStore
  ( newMemoryStore
  , MemoryStore
  )

where

import Prelude hiding (lookup, EQ, GT, LT, LTE, GTE)
import Control.Monad
import Control.Concurrent.MVar
import Data.Map (Map, lookup, insertWith, empty, delete, elems)
import Data.Aeson
import Data.Text (pack, unpack)
import qualified Data.HashMap.Lazy as H
import Data.Maybe
import Data.Typeable
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Asset hiding (AssetClass(..))

type TableName = String

data Table a = Table { contents :: MVar (Map Int (MVar a)), idx :: MVar Int}

data MemoryStore = MS (MVar (Map TableName (Table Value)))

newMemoryStore = MS <$> newMVar empty

tableName :: Typeable a => a -> TableName
tableName a = show $ typeOf a

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

lookupRef :: Reference a -> MemoryStore -> IO (Maybe (MVar Value))
lookupRef ref (MS mv1) =  runMaybeT $ do
  tbl <- MaybeT $ lookup' (assetType ref) mv1
  MaybeT $ lookup' (assetIndex ref) (contents tbl)

allOfType ::  MemoryStore -> String -> IO [Value]
allOfType (MS mv1) tblname = do
  mmv <- lookup' tblname mv1
  case mmv of
    Nothing -> return []
    Just table -> do
      withMVar
        (contents table)
        (\table ->mapM readMVar (elems table))

test :: Bool -> a -> Maybe a
test True = Just
test False = const Nothing

extract :: FromJSON a => Value -> Maybe a
extract value =
  case fromJSON value of
    Error _ -> Nothing
    Success x -> Just x

findMS :: forall a. Asset a => MemoryStore -> Query -> IO [a]
findMS ms All = (mapMaybe extract) <$> allOfType ms tblname
  where
    typ :: a
    typ = undefined  --Type Hackery
    tblname = tableName typ
findMS ms (Filter qs) =  (mapMaybe (filter qs)) <$> allOfType ms tblname
  where
    typ :: a
    typ = undefined  --Type Hackery
    tblname = tableName typ
    filter ::  [(String, Comparison Value)] -> Value -> Maybe a
    filter [] v = extract v
    filter ((k,comp):qs) (Object obj) = do
      v <- H.lookup (pack k) obj
      guard $ eval v comp
      filter qs (Object obj)
    filter _ _ = Nothing
    eval :: Value -> Comparison Value -> Bool
    eval v (AND c1 c2) = eval v c1 && eval v c2
    eval v (OR  c1 c2) = eval v c1 || eval v c2
    eval v (EQ a) = v == a
    eval v (GT a) = compair (>) v a
    eval v (LT a) = compair (<) v a
    eval v (GTE a) = compair (>=) v a
    eval v (LTE a) = compair (<=) v a
    compair :: (forall b. Ord b => b -> b -> Bool) 
            -> Value 
            -> Value 
            -> Bool
    compair f (String x) (String y) = unpack x `f` unpack y
    compair f (Number x) (Number y) = x `f` y
    -- Todo Array (?)
    compair _ _ _ = False 
findMS ms (Field k val) =  (mapMaybe filter) <$> allOfType ms tblname
  where
    typ :: a
    typ = undefined  --Type Hackery
    tblname = tableName typ
    filter :: Value -> Maybe a
    filter (Object obj) = do
      v <- H.lookup (pack k) obj
      guard (v == val)
      extract (Object obj)
    filter _ = Nothing
findMS ms (First q) = take 1 <$> findMS ms q




instance AssetStore MemoryStore where
  loadAsset ms ref = 
    loadAssetGeneric id ms (voidReference ref)
  loadAssetGeneric f ms ref = do
    mmv <- lookupRef ref ms
    case mmv of
      Nothing -> return $ Left "Not Found"
      Just mv2 -> do
        val <- readMVar mv2
        return $ case fromJSON val of
              Error s -> Left s
              Success a -> Right (f a)
  findAsset = findMS

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



