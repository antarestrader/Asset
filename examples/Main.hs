{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import GHC.Generics
import Data.Typeable
import Text.Printf
import System.IO
import Control.Monad.IO.Class

import Asset
import Asset.MemoryStore
import Update

main :: IO ()
main = do
  putStrLn "Asset Game Sketch"
  as <- newMemoryStore 
  runAssetM as mainprog
  return ()

mainprog :: (AssetStore as) => AssetM as ()
mainprog = do
  user <- (maybe (error "User write failed") id ) <$> store 
          (User { username = "Player 1"
                , uIdent = newIdent
                , cash = 10000
                })
  liftIO $ printf "welcome %s\n" (name user)
  mainloop $ ref user
  return ()

mainloop :: (AssetStore as) => Reference -> AssetM as ()
mainloop u = do
  cmd <- prompt " > " -- $ printf " %s > " (label u) 
  mu' <- eval u cmd
  case mu' of
    Nothing -> liftIO $ putStrLn "Game Over"
    Just u' -> mainloop u'

prompt :: String -> AssetM as String
prompt s = liftIO $ do
 putStr s
 hFlush stdout
 getLine

eval :: (AssetStore as) => Reference -> String -> AssetM as (Maybe Reference)
eval _ "quit" = return Nothing
eval u "sing" = do
  eu <- load u
  case eu of
    Left str -> liftIO $ putStrLn str >> return Nothing
    Right user -> do
      user' <- (maybe (error "User write failed") id )<$> (store $ user{cash=(cash user - 1000)})
      liftIO $ printf "Happy Birthday to user\nHappy Birthday to user\nHappy Birthday dear %s ...\nHappy Birthday to user!\n" (name user')
      liftIO $ printf "You have %d left.\n" (cash user')
      return $ Just $ ref user'
eval u cmd = liftIO $ do
  putStrLn $ "Unrecognized command: " ++ cmd
  putStrLn "Try 'sing'"
  return $ Just $ u



data User = User {
    username   :: String
  , uIdent :: Ident
  , cash   :: Int
  } deriving (Generic, Typeable, Show)

instance ToJSON User
instance FromJSON User

instance Asset User where
  ident = uIdent
  updateIdent i a = a{uIdent=i}
  name = username

data Message = Message {
    mIdent    :: Ident
  , recipient :: Reference
  , text      :: String
  } deriving (Generic, Typeable, Show)

instance ToJSON Message
instance FromJSON Message

instance Asset Message where
  ident = mIdent
  updateIdent i a = a{mIdent=i}
  name a = printf "Message for %s" (label $ recipient a)


