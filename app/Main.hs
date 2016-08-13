module Main where

import Control.Exception    (bracket)
import Control.Monad        (msum)
import Data.Acid            (AcidState, openLocalStateFrom)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Configurator    (Worth(..), load, lookupDefault)
import Data.Text            (Text, empty, pack)
import Happstack.Server
import System.Environment   (getArgs)
import Text.Lucius          (renderCss)
import Web.Routes.Happstack (implSite)

import Routes
import Render               (stylesheet)
import Storage

routes :: AcidState Board ->
          BodyPolicy ->
          Text ->
          Text ->
          Text-> ServerPart Response
routes acid decodePolicy prefix homeUrl static = do
    decodeBody decodePolicy
    msum [ do dirs "style.css" $ nullDir
              setHeaderM "Content-Type" "text/css"
              ok $ toResponse $ renderCss stylesheet
         , implSite homeUrl prefix (site acid static)
         -- , do dirs "" $ serveDirectory DisableBrowsing [] (unpack static)
         , notFound $ toResponse (pack "not found\n")
         ]

main :: IO ()
main = do
    args      <- getArgs
    config    <- load [ Optional (head' args) ]
    tmpdir    <- lookupDefault "/tmp"        config (pack "tmp")
    static    <- lookupDefault "/tmp"        config (pack "static")
    h         <- lookupDefault "localhost"   config (pack "host")
    p         <- lookupDefault (8000 :: Int) config (pack "port")
    state     <- lookupDefault "/tmp"        config (pack "storage")
    prefix    <- lookupDefault empty         config (pack "prefix")
    maxFile   <- lookupDefault (1024*1024)   config (pack "max_file")
    maxFields <- lookupDefault (1024*1024)   config (pack "max_fields")
    maxHdr    <- lookupDefault (10*1024)     config (pack "max_header")
    
    let policy = defaultBodyPolicy tmpdir maxFile maxFields maxHdr
        home   = pack $ appRoot h p
        conf   = nullConf { port = p }
        st     = pack static
    
    bracket
        (openLocalStateFrom state initialBoardState)
        (createCheckpointAndClose)
            (\acid -> simpleHTTP conf (routes acid policy prefix home st))
    where
        head' :: [String] -> String
        head' [] = ""
        head' (x:_) = x
        
        appRoot h p =
            case p of
                80  -> "http://"  ++ h
                443 -> "https://" ++ h
                _   -> "http://"  ++ h ++ ":" ++ (show p)
