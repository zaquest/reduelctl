-- vim: cc=70
{-# LANGUAGE OverloadedStrings #-}
module Config
  ( Config(..)
  , AppCfg(..)
  , ServerCfg(..)
  , parseConfig
  ) where

import Data.Semigroup ((<>))
import Data.List ((\\), sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (Reader, decimal)
import Data.Ini

data Config = Config
  { app :: AppCfg
  , servers :: [ServerCfg] }
  deriving Show

data ServerCfg = ServerCfg
  { host :: String
  , port :: Int
  , start :: String
  , stop :: String }
  deriving Show

data AppCfg = AppCfg
  { delay :: Int
  , maxPlayers :: Int }
  deriving Show

-- | Find `key` in `section` falling back to `common` section.
lookupServer :: Ini -> Text -> Text -> Either String Text
lookupServer ini section key =
  case lookupValue section key ini of
    val@(Right _) -> val
    Left _ ->
      case lookupValue "common" key ini of
        val@(Right _) -> val
        Left _ -> Left ("No '" <> T.unpack key <>
                        "' value is specified for '" <>
                        T.unpack section <> "'")

readInt :: String -> Text -> Either String Int
readInt key str =
  case decimal str of
    Right (val, "") -> Right val
    _ -> Left ("Invalue '" <> key <> "' value '" <> T.unpack str <>
               "'")

parseServer :: Ini -> Text -> Either String ServerCfg
parseServer ini section =
  ServerCfg <$> (T.unpack <$> get "host")
            <*> (get "port" >>= readInt "port")
            <*> (T.unpack <$> get "start")
            <*> (T.unpack <$> get "stop")
    where render = T.replace "{name}" section
          get key = render <$> lookupServer ini section key

lookupApp :: Ini -> Text -> Either String Text
lookupApp ini key =
  case lookupValue "app" key ini of
    val@(Right _) -> val
    Left _ -> Left ("No '" <> T.unpack key <> "' is specified in \
                    \section 'app'")

parseApp :: Ini -> Either String AppCfg
parseApp ini = AppCfg <$> get "delay" <*> get "maxplayers"
  where get key = lookupApp ini key >>= readInt (T.unpack key)

parseConfig :: Text -> Either String Config
parseConfig text = do
  ini <- parseIni text
  let servers = (sections ini) \\ ["common", "app"]
  Config <$> parseApp ini
         <*> mapM (parseServer ini) (sort servers)
