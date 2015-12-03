{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception    (bracket)
import Control.Monad        (msum)
import Data.Acid            (AcidState, openLocalState)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Text            (pack, empty)
import Happstack.Server
import Web.Routes.Happstack (implSite)

import Routes
import Storage



routes :: AcidState Board -> ServerPart Response
routes acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ implSite (pack "http://localhost:8000") empty (site acid)
            , notFound $ toResponse (pack "not found\n")
            ]

main :: IO ()
main =
    bracket (openLocalState initialBoardState)
            (createCheckpointAndClose)
                (\acid -> simpleHTTP nullConf (routes acid))
