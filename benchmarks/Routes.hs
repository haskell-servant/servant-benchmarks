{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import Data.Proxy
import Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Media            as M
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server

import Routes.RoutesTH

main :: IO ()
main = run 8010 $ serve api server


type Api = ApiDeep2

api :: Proxy Api
api = Proxy

server :: Server Api
server = serverDeep

data BS

instance Accept BS where
   contentType _ = "text" M.// "plain" M./: ("charset", "utf-8")

instance MimeRender BS ByteString where
    mimeRender _ = id

type Get' = Get '[BS] ByteString

type ApiDeep = "deep" :> "foo" :> "bar" :> "baz" :> "0" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "1" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "2" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "3" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "4" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "5" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "6" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "7" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "8" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "9" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "10" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "11" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "12" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "13" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "14" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "15" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "16" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "17" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "18" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "19" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "20" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "21" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "22" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "23" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "24" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "25" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "26" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "27" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "28" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "29" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "30" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "31" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "32" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "33" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "34" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "35" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "36" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "37" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "38" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "39" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "40" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "41" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "42" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "43" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "44" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "45" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "46" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "47" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "48" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "49" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "50" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "51" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "52" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "53" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "54" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "55" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "56" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "57" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "58" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "59" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "60" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "61" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "62" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "63" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "64" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "65" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "66" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "67" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "68" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "69" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "70" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "71" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "72" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "73" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "74" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "75" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "76" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "77" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "78" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "79" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "80" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "81" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "82" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "83" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "84" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "85" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "86" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "87" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "88" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "89" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "90" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "91" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "92" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "93" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "94" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "95" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "96" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "97" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "98" :> Get'
          :<|> "deep" :> "foo" :> "bar" :> "baz" :> "99" :> Get'

type ApiDeep2 = "deep" :> "foo" :> "bar" :> "baz" :> ("0" :> Get'
          :<|>  "1" :> Get'
          :<|>  "2" :> Get'
          :<|>  "3" :> Get'
          :<|>  "4" :> Get'
          :<|>  "5" :> Get'
          :<|>  "6" :> Get'
          :<|>  "7" :> Get'
          :<|>  "8" :> Get'
          :<|>  "9" :> Get'
          :<|>  "10" :> Get'
          :<|>  "11" :> Get'
          :<|>  "12" :> Get'
          :<|>  "13" :> Get'
          :<|>  "14" :> Get'
          :<|>  "15" :> Get'
          :<|>  "16" :> Get'
          :<|>  "17" :> Get'
          :<|>  "18" :> Get'
          :<|>  "19" :> Get'
          :<|>  "20" :> Get'
          :<|>  "21" :> Get'
          :<|>  "22" :> Get'
          :<|>  "23" :> Get'
          :<|>  "24" :> Get'
          :<|>  "25" :> Get'
          :<|>  "26" :> Get'
          :<|>  "27" :> Get'
          :<|>  "28" :> Get'
          :<|>  "29" :> Get'
          :<|>  "30" :> Get'
          :<|>  "31" :> Get'
          :<|>  "32" :> Get'
          :<|>  "33" :> Get'
          :<|>  "34" :> Get'
          :<|>  "35" :> Get'
          :<|>  "36" :> Get'
          :<|>  "37" :> Get'
          :<|>  "38" :> Get'
          :<|>  "39" :> Get'
          :<|>  "40" :> Get'
          :<|>  "41" :> Get'
          :<|>  "42" :> Get'
          :<|>  "43" :> Get'
          :<|>  "44" :> Get'
          :<|>  "45" :> Get'
          :<|>  "46" :> Get'
          :<|>  "47" :> Get'
          :<|>  "48" :> Get'
          :<|>  "49" :> Get'
          :<|>  "50" :> Get'
          :<|>  "51" :> Get'
          :<|>  "52" :> Get'
          :<|>  "53" :> Get'
          :<|>  "54" :> Get'
          :<|>  "55" :> Get'
          :<|>  "56" :> Get'
          :<|>  "57" :> Get'
          :<|>  "58" :> Get'
          :<|>  "59" :> Get'
          :<|>  "60" :> Get'
          :<|>  "61" :> Get'
          :<|>  "62" :> Get'
          :<|>  "63" :> Get'
          :<|>  "64" :> Get'
          :<|>  "65" :> Get'
          :<|>  "66" :> Get'
          :<|>  "67" :> Get'
          :<|>  "68" :> Get'
          :<|>  "69" :> Get'
          :<|>  "70" :> Get'
          :<|>  "71" :> Get'
          :<|>  "72" :> Get'
          :<|>  "73" :> Get'
          :<|>  "74" :> Get'
          :<|>  "75" :> Get'
          :<|>  "76" :> Get'
          :<|>  "77" :> Get'
          :<|>  "78" :> Get'
          :<|>  "79" :> Get'
          :<|>  "80" :> Get'
          :<|>  "81" :> Get'
          :<|>  "82" :> Get'
          :<|>  "83" :> Get'
          :<|>  "84" :> Get'
          :<|>  "85" :> Get'
          :<|>  "86" :> Get'
          :<|>  "87" :> Get'
          :<|>  "88" :> Get'
          :<|>  "89" :> Get'
          :<|>  "90" :> Get'
          :<|>  "91" :> Get'
          :<|>  "92" :> Get'
          :<|>  "93" :> Get'
          :<|>  "94" :> Get'
          :<|>  "95" :> Get'
          :<|>  "96" :> Get'
          :<|>  "97" :> Get'
          :<|>  "98" :> Get'
          :<|>  "99" :> Get')

serverDeep :: Server ApiDeep
serverDeep = [gen| 100 |]



