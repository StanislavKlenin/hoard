{-# LANGUAGE 
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    TemplateHaskell,
    TypeOperators #-}

module Sitemap where

import Prelude              hiding ((.))
import Control.Category     (Category((.)))
import Data.Data            (Data, Typeable)
import Data.Text            (Text)
import Text.Boomerang.TH    (makeBoomerangs)
import Web.Routes.Boomerang
import Web.Routes.TH        (derivePathInfo)

import Messages

-- make Parent and Section instances of PathInfo, retroactively
$(derivePathInfo ''Parent)
$(derivePathInfo ''Section)

data Sitemap = Home 
             | Board Text
             | Thread Text Int
             | File Text Text Text
    deriving (Eq, Ord, Data, Typeable)

$(derivePathInfo ''Sitemap)
$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap = 
    (  rHome
    <> rBoard . anyText
    <> rThread . (anyText </> int)
    <> rFile . (anyText </> anyText </> anyText)
    )
