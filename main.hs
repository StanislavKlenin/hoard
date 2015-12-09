module Main where

import Control.Exception    (bracket)
import Control.Monad        (msum)
import Data.Acid            (AcidState, openLocalState)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Configurator    (Worth(..), load, lookupDefault)
import Data.Text            (Text, pack, empty)
import Happstack.Server
import System.Environment   (getArgs)
import Text.Lucius          (renderCss)
import Web.Routes.Happstack (implSite)

import Routes
import Render               (stylesheet)
import Storage

routes :: AcidState Board -> BodyPolicy -> Text -> ServerPart Response
routes acid decodePolicy homeUrl = do
    decodeBody decodePolicy
    msum [ do dirs "style.css" $ nullDir
              setHeaderM "Content-Type" "text/css"
              ok $ toResponse $ renderCss stylesheet
         , implSite homeUrl empty (site acid)
         , notFound $ toResponse (pack "not found\n")
         ]

main :: IO ()
main = do
    args   <- getArgs
    config <- load [ Optional (head' args) ]
    tmpdir <- lookupDefault "/tmp" config (pack "tmp")
    h      <- lookupDefault "localhost" config (pack "host")
    p      <- lookupDefault (8000 :: Int) config (pack "port")
    
    -- TODO: other policy parameters must be configurable too
    let policy = defaultBodyPolicy tmpdir 0 1000000 1000000
        home   = pack $ appRoot h p
        conf   = nullConf { port = p }
    
    bracket (openLocalState initialBoardState)
            (createCheckpointAndClose)
                (\acid -> simpleHTTP conf (routes acid policy home))
    where
        head' :: [String] -> String
        head' [] = ""
        head' (x:_) = x
        
        appRoot h p =
            case p of
                80  -> "http://"  ++ h
                443 -> "https://" ++ h
                _   -> "http://"  ++ h ++ ":" ++ (show p)
