{-# LANGUAGE 
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    TemplateHaskell,
    TypeOperators #-}

module Routes where

import Control.Applicative  (optional)
import Control.Monad        (msum)
import Control.Monad.Trans  (MonadIO(liftIO))
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query', update')
import Data.Maybe           (fromMaybe)
import Data.Text            (empty)
import Data.Time.Clock      (getCurrentTime)
import Happstack.Server
import Web.Routes           (RouteT, runRouteT, Site(..), setDefault)
import Web.Routes.Boomerang
import Web.Routes.Happstack

import Messages
import Render
import Sitemap
import Storage

route :: AcidState Board -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route acid url = do
    urlf <- renderFunction
    case url of
        Sitemap.Home         -> ok $ toResponse "home page will be here\n"
        (Sitemap.Board b)    -> msum
            [ do method GET
                 --messages <- query' acid (ListThreads $ Section b)
                 --ok $ toResponse $ renderSection b messages urlf
                 preview <- query' acid (ListThreads' $ Section b)
                 ok $ toResponse $ renderSection' b preview urlf
            , post b 0
            --, do method POST
            --     ok $ toResponse "board page POST\n"
            ]
        (Sitemap.Thread b t) -> msum
            [ do method GET
                 messages <- query' acid (ListThreadPosts (Section b)
                                                          (Parent t))
                 --tz       <- liftIO $ getCurrentTimeZone
                 ok $ toResponse $ renderThread b messages urlf
            , post b t
            --, do method POST
            --     ok $ toResponse "thread page POST\n"
            ]
    -- TODO: 404 if messages is empty (nonexistent threads and boards)
    
    where 
        post sec thread =
             do method POST
                urlf     <- renderFunction
                currTime <- liftIO   $ getCurrentTime
                name     <- optional $ lookText' "author"
                subj     <- optional $ lookText' "subject"
                text     <- optional $ lookText' "contents"
                let message = Messages.Message {
                      messageId = PostId 0 -- ignored
                    , parent    = Parent thread
                    , section   = Section sec
                    , created   = currTime
                    , author    = Author $ fetch name
                    , subject   = Subject $ fetch subj
                    , contents  = Contents $ fetch text
                    }
                posted <- update' acid (AddPost message)
                let tid = case thread of
                        -- new thread, its id is first post id:
                        0 -> newId where PostId newId = messageId posted
                        -- old thread:
                        _ -> thread
                -- finally, redirect to the thread
                -- generate redirect url using url rendering function
                -- (second argument is array of url parameters)
                let u = urlf (Sitemap.Thread sec tid) []
                seeOther u (toResponse ())
                
                where
                    fetch = fromMaybe empty


-- yet another wrapper
site :: AcidState Board -> Site Sitemap (ServerPartT IO Response)
site acid =
    --setDefault Home $ mkSitePI (runRouteT $ route acid)
    setDefault Home $ boomerangSite (runRouteT $ route acid) sitemap
