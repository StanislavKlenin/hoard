{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception    (bracket)
import Control.Monad        (msum)
import Data.Acid            (AcidState, Query, Update, openLocalState)
import Data.Acid.Advanced   (query', update')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Text            (Text, pack, empty)
import Data.Time.Clock
import Happstack.Server
import Text.Read            (readMaybe)
import Web.Routes.Happstack (implSite)

import Messages
import Render
import Routes
import Storage



routes :: AcidState Board -> ServerPart Response
routes acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ implSite (pack "http://localhost:8000") empty (site acid)
            , notFound $ toResponse (pack "not found\n")
            ]


runServer :: IO ()
runServer =
    bracket (openLocalState initialBoardState)
            (createCheckpointAndClose)
                (\acid -> simpleHTTP nullConf (routes acid))

--listEm :: AcidState Board -> IO ()
--listEm acid = do
--    let b = Section "b"
--    let root = Parent 0
--    posts <- query' acid (listPosts )

runConsole :: IO ()
runConsole = do
    currTime <- getCurrentTime
    let message = Message { messageId = PostId 42 -- will be ignored anyway
                          , parent    = Parent 1
                          , section   = Section "b"
                          , created   = currTime
                          , author    = Author "Bill"
                          , subject   = Subject ""
                          , contents  = Contents "answer to post 1"
                          }
    let b = Section "b"
    let root = Parent 0
    bracket (openLocalState initialBoardState)
            (createCheckpointAndClose)
                (\acid -> do
                    --u <- update' acid (AddPost message)
                    lst <- query' acid (ListPosts b root)
                    putStrLn $ "thread count: " ++ show (length lst))
    putStrLn "done"

main :: IO ()
main = runServer
--main = runConsole
