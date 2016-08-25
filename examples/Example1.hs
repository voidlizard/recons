{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Either
import Data.List
import Data.Monoid
import Data.Typeable
import Servant.API
import Servant.Client
import Servant.Server
import System.Console.Haskeline as H

import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings,defaultSettings)
import qualified Network.Wai.Handler.Warp as W

import Data.List (unfoldr,nub)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)


import CLI.Recons
import Data.Attoparsec.Recons
import Data.Recons.Dict as Dict
import Servant.Recons
import Servant.Recons.Client as Client
import Servant.Recons.Dict (makeDict,PrefixPart(..))

type API1  = "some"  :> QueryParam "test" Int :> Get '[JSON] (String,Maybe Int)
type API11 = "some"  :> "shit" :> QueryParam "test" String :> Get '[JSON] (String,Maybe String)
type API2  = "other" :> QueryParam "test" Double :> Get '[JSON] (String,Maybe Double)
type API3  = "rest"  :> "shit" :> QueryParam "test" Double :> "skip" :> QueryParam "q" String :> Get '[JSON] (String,Maybe Double, Maybe String)

type SumAPI = API1 :<|> API11 :<|> API2 :<|> API3

serveAPI1 :: Server API1
serveAPI1 a = return ("API1",a)

serveAPI11 :: Server API11
serveAPI11 a = return ("API11",a)

serveAPI2 :: Server API2
serveAPI2 a = return ("API2",a)

serveAPI3 :: Server API3
serveAPI3 a b = return ("API3",a,b)

server :: Server SumAPI
server = serveAPI1
    :<|> serveAPI11
    :<|> serveAPI2
    :<|> serveAPI3

app :: Application
app = serve (Proxy :: Proxy SumAPI) server



data ReplSettings = ReplSettings { complDict  :: GTrieDictMap (TrieDictSkipKey String) [PrefixPart]
                                 , clientDict :: DictType Show
                                 , manager    :: Manager
                                 , baseUrl    :: BaseUrl
                                 }


inputTokens :: String -> [String]
inputTokens = either (const []) id . tokenize

settings :: Settings (ReaderT ReplSettings IO)
settings = setComplete (completeWordWithPrev (Just '\\')  " \t" compl) defaultSettings
  where
    compl prev w = do
      cDict <- asks complDict
      let tokens = inputTokens (reverse prev)
      let key = makeKey tokens
-- -       TODO: parameters completion
      let ws = [x|W x <- concat $ Dict.lookup key cDict]
      let ws' = filter (isPrefixOf w) ws
      let cc = fmap (\w -> Completion w w True) ws'

      return cc


main :: IO ()
main = do

  forkIO $ run 8081 (serve (Proxy ::Proxy SumAPI) server)

  let clientDict = Client.fromAPI (Proxy :: Proxy Show) (Proxy :: Proxy SumAPI)
  let replDict = makeDict ( Proxy :: Proxy SumAPI)

  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" 8081 ""

  _ <- runReaderT (runInputT settings loop) $ ReplSettings replDict
                                                           clientDict
                                                           manager
                                                           baseUrl

  return ()

  where
    loop = getInputLine "% " >>= maybe loop process . trim

    process input  = do

      actions <- lift $ asks clientDict
      manager <- lift $ asks manager
      baseUrl <- lift $ asks baseUrl

      let toks = inputTokens input
      let key = makeKey toks

      let !mclient = Dict.lookup key actions >>= \ra -> parse ra toks

      case mclient of
        Nothing -> outputStrLn "** bad command"
        (Just client) -> liftIO $ do
          res <- perform show manager baseUrl client
          print res

      loop

    trim s = maybe Nothing (nt . unwords . words) s

    nt [] = Nothing
    nt x  = Just x


