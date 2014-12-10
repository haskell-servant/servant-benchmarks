{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Util for generating a type with an arbitrary number of routes,
--
module Routes.RoutesTH where

import Data.List
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.String.Interpolate (i)
import Servant

mkTyp :: Int -> Type
mkTyp n = foldr1 union list
  where
    list = [ AppT (AppT (ConT ''(:>)) (LitT $ StrTyLit $ "path" ++ show n'))
                  (AppT (ConT ''Get) (ConT ''Bool))
           | n' <- [0..n]
           ]
    union a = AppT (AppT (ConT ''(:<|>)) a)

mkServe :: Int -> ExpQ
mkServe n = foldr1 (liftM2 union) list
  where
    list = replicate (n + 1) [| return True |]
    union a = AppE (AppE (ConE '(:<|>)) a)


gen :: QuasiQuoter
gen = QuasiQuoter { quoteType = return . mkTyp . read
                  , quoteExp  = mkServe . read
                  }
