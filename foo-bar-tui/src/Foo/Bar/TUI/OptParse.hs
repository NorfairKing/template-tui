{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Foo.Bar.TUI.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { setPort :: !Int -- Just an example
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let setPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { confPort :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration <$> optionalField "port" "Port" .= confPort

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|foo-bar|])
  resolveFile xdgConfigDir "config.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FOO_BAR_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.auto) "PORT" (Env.def Nothing <> Env.help "Port")

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int)
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                help "Port",
                metavar "PORT"
              ]
          )
      )
