{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Control.Concurrent
import Control.Monad.Trans.Either
import Criterion.Main
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import System.Random

import Routes.RoutesTH

main :: IO ()
main = mainWith 50

mainWith :: Int -> IO ()
mainWith n = do
    forkIO $ run 8010 $ serve api10 server10
    forkIO $ run 8050 $ serve api50 server50
    forkIO $ run 8250 $ serve api250 server250
    gen' <- getStdGen
    let rs10 = randomRs (0::Int, 9) gen'
    let rs50 = randomRs (0::Int, 49) gen'
    let rs250 = randomRs (0::Int, 249) gen'
    let ep10  = [ f (BaseUrl Http "localhost" 8010) | f <- endpoints10 ]
    let ep50  = [ f (BaseUrl Http "localhost" 8050) | f <- endpoints50 ]
    let ep250  = [ f (BaseUrl Http "localhost" 8250) | f <- endpoints250 ]
    let act10 = [ ep10 !! n' | n' <- take n rs10 ]
    let act50 = [ ep50 !! n' | n' <- take n rs50 ]
    let act250 = [ ep250 !! n' | n' <- take n rs250 ]
    act10 `seq` act50 `seq` act250 `seq` defaultMain [
       bench "10" $ nfIO . runEitherT $ sequence act10
       , bench "50" $ nfIO . runEitherT $ sequence act50
       , bench "250" $ nfIO . runEitherT $ sequence act250
       ]


type Api10   = [gen| 10 |]
type Api50  = [gen| 50 |]
type Api250 = [gen| 250 |]

api10 :: Proxy Api10
api10 = Proxy

api50 :: Proxy Api50
api50 = Proxy

api250 :: Proxy Api250
api250 = Proxy

server10 :: Server Api10
server10 = [gen| 10 |]

server50 :: Server Api50
server50 = [gen| 50 |]

server250 :: Server Api250
server250 = [gen| 250 |]
-- Endpoint unrolling

endpoints10, endpoints50, endpoints250 :: [BaseUrl -> EitherT String IO Bool]
endpoints10   = unroll $ client api10
endpoints50  = unroll $ client api50
endpoints250 = unroll $ client api250

class Unrolls a b where
    unroll :: a -> [b]

instance (Unrolls a c, Unrolls b c) => Unrolls (a :<|> b) c where
    unroll (a :<|> b) = unroll a ++ unroll b

instance Unrolls a a where
    unroll a = [a]


