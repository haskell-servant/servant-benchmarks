{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Util for generating a type with an arbitrary number of routes,
--
module Routes.RoutesTH where

import Data.List
import Data.List.Split
import Data.Text (Text)
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.String.Interpolate (i)
import Servant.API

-- | Given a string and an int N, generate a type for N routes based on the
-- string as pattern, replacing any '#' with the number of the current route.
mkTyp :: (String, Int) -> Type
mkTyp (path, n) = foldr1 union list
  where
    list = [ AppT (AppT (ConT ''(:>)) (rep n))
                  (AppT (AppT (ConT ''Get) listT) (ConT ''Text))
           | n' <- [0..n]
           ]
    union a = AppT (AppT (ConT ''(:<|>)) a)
    listT = AppT (AppT PromotedConsT (ConT ''PlainText)) PromotedNilT
    rep n = segmentStr $ replace (show n) "#" path

mkServe :: Int -> ExpQ
mkServe n = foldr1 (liftM2 union) list
  where
    list = replicate n [| return "string" |]
    union a = AppE (AppE (ConE '(:<|>)) a)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace new old = intercalate new . splitOn old

-- | "a/b/c" -> "a" :> "b" :> "c"
segmentStr :: String -> Type
segmentStr = foldr1 union . fmap tLit . splitOn "/"
  where
    union a = AppT (AppT (ConT ''(:>)) a)

tLit :: String -> Type
tLit = LitT . StrTyLit

gen :: QuasiQuoter
gen = QuasiQuoter { quoteType = return . mkTyp . read
                  , quoteExp  = mkServe . read
                  }


