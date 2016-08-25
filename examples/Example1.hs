{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Either
import Data.List
import Data.List (unfoldr,nub)
import Data.Typeable
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings,defaultSettings)
import qualified Network.Wai.Handler.Warp as W
import Servant.API
import Servant.Client as C
import Servant.Server

import Network.HTTP.Types

import System.Console.Haskeline as H
-- import System.Console.Haskeline.Completion (CompletionType(..))
-- import System.Console.Haskeline.Prefs
--
import Text.InterpolatedString.Perl6 (qq)
import System.Environment
import Text.PrettyPrint.ANSI.Leijen as P

-- import System.IO
-- import System.Console.Terminfo.PrettyPrint
-- import System.Console.Terminfo.PrettyPrint as Term
-- import Text.PrettyPrint.Free

import CLI.Recons
import Data.Attoparsec.Recons
import Data.Recons.Dict as Dict
import Servant.Recons
import Servant.Recons.Client as Client
import Servant.Recons.Dict (makeDict,PrefixPart(..))

type API1  = "some"  :> QueryParam "test" Int :> Get '[JSON] String
type API11 = "some"  :> "shit" :> QueryParam "test" String :> Get '[JSON] String
type API2  = "other" :> QueryParam "test" Double :> Get '[JSON] String
type API3  = "rest"  :> "shit" :> QueryParam "test" Double :> "skip" :> QueryParam "q" String :> Get '[JSON] [String]

type SumAPI = API1 :<|> API11 :<|> API2 :<|> API3

serveAPI1 :: Server API1
serveAPI1 a = return "API1"

serveAPI11 :: Server API11
serveAPI11 a = return "API11"

serveAPI2 :: Server API2
serveAPI2 a = return "API2"

serveAPI3 :: Server API3
serveAPI3 a b = return $ replicate 200 "TEST STRING FROM API3"

server :: Server SumAPI
server = serveAPI1
    :<|> serveAPI11
    :<|> serveAPI2
    :<|> serveAPI3

app :: Application
app = serve (Proxy :: Proxy SumAPI) server

data ReplSettings = ReplSettings { complDict  :: GTrieDictMap (TrieDictSkipKey String) [PrefixPart]
                                 , clientDict :: DictType CliResult 
                                 , manager    :: Manager
                                 , baseUrl    :: BaseUrl
                                 }


class (Pretty v) => CliResult v

instance CliResult String where

-- instance Pretty [String] where
--   pretty ss = vcat $ fmap text ss

instance CliResult [String] where
--   pretty ss = vcat $ fmap text ss

newtype CliError e = CliError e 

instance Pretty e => Pretty (CliError e) where
  pretty (CliError e) = red $ text "*** error: " <> pretty e 

instance Pretty (ServantError) where
  pretty e@(FailureResponse {C.responseStatus = s}) = red $ text [qq|*** server error {statusCode s}: {statusMessage s} |]
                                                        <$$> text [qq|*** {responseBody e}|]
                                                        <$$> P.empty

  pretty e@(ConnectionError _) = red $ text "*** connection error:"
                                     <$$> text (show e)
                                     <$$> P.empty

  pretty other = red $ text "*** some communication error:"
                     <$$> text (show other)
                     <$$> P.empty
          

instance Pretty (ServantErr) where
  pretty err = red $ text "*** server error, code: " <> int (errHTTPCode err)
                   <$$> indent 2 (text $ errReasonPhrase err)
                   <$$> P.empty

inputTokens :: String -> [String]
inputTokens = either (const []) id . tokenize

settings :: Settings (ReaderT ReplSettings IO)
settings = setComplete (completeWordWithPrev (Just '\\')  " \t" compl) def
  where
    def = defaultSettings { historyFile = Just ".example1-hist" }

    compl prev w = do
      cDict <- asks complDict
      let tokens = inputTokens (reverse prev)
      let key = makeKey tokens
-- -       TODO: parameters completion
      let ws = [ x | W x <- concat $ Dict.lookup key cDict ]
      let ws' = filter (isPrefixOf w) ws
      let cc = fmap (\w -> Completion w w True) ws'

      return cc


main :: IO ()
main = do

  forkIO $ run 8081 (serve (Proxy ::Proxy SumAPI) server)

  let clientDict = Client.fromAPI (Proxy :: Proxy CliResult) (Proxy :: Proxy SumAPI)
  let replDict = makeDict ( Proxy :: Proxy SumAPI)

  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" 8081 ""
 
  let prefs = defaultPrefs

  let bhw = preferTerm

  let rsets = ReplSettings replDict
                           clientDict
                           manager
                           baseUrl

  _ <- runReaderT (runInputTBehavior bhw settings loopBegin) rsets 

  return ()

  where

    loopBegin = do

      let wtf = underline $ black $ onwhite $ string "WTF"
      let banner = string (replicate 80 '*') 
                   <$$> P.empty 
                   <$$> indent 16 (string "Welcome to Recons CLI interface example")
                   <$$> indent 16 (string "Press some keys to have fun")
                   <$$> P.empty
                   <$$> string (replicate 80 '*') 
                   <$$> P.empty

      outputStrLn $ show $ pretty $ green $ banner     

      loop

    loop = getInputLine "% " >>= maybe loop process . trim

    process input  = do

      actions <- lift $ asks clientDict
      manager <- lift $ asks manager
      baseUrl <- lift $ asks baseUrl

      let toks = inputTokens input
      let key = makeKey toks

      let !mclient = Dict.lookup key actions >>= \ra -> parse ra toks

      case mclient of
        Nothing -> outputStrLn $ show $ pretty $ CliError "^^^ bad command"
        (Just client) -> do
          res <- liftIO $ perform pretty manager baseUrl client
          case res of
            Left err   -> outputStrLn $ show $ pretty err
            Right smth -> outputStrLn $ show smth

      loop

    trim s = maybe Nothing (nt . unwords . words) s

    nt [] = Nothing
    nt x  = Just x


