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
import Data.Text            (pack, concat, empty)
import Data.Time.Clock      (getCurrentTime)
import Happstack.Server
import Web.Routes           (RouteT, showURL, runRouteT, Site(..), setDefault)
import Web.Routes.Boomerang
import Web.Routes.Happstack (implSite)

import Messages
import Render
import Sitemap
import Storage

route :: AcidState Board -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route acid url =
    case url of
        Sitemap.Home         -> ok $ toResponse "home page will be here\n"
        (Sitemap.Board b)    -> msum
            [ do method GET
                 messages <- query' acid (ListThreads $ Section b)
                 ok $ toResponse $ renderSection b messages undefined
            , post b 0
            --, do method POST
            --     ok $ toResponse "board page POST\n"
            ]
        (Sitemap.Thread b t) -> msum
            [ do method GET
                 messages <- query' acid (ListThreadPosts (Section b)
                                                          (Parent t))
                 --tz       <- liftIO $ getCurrentTimeZone
                 ok $ toResponse $ renderThread b messages undefined
            , post b t
            --, do method POST
            --     ok $ toResponse "thread page POST\n"
            ]
    -- TODO: 404 if messages is empty (nonexistent threads and boards)
    
    where 
        post sec thread =
             do method POST
                currTime <- liftIO   $ getCurrentTime
                name     <- optional $ lookText' "author"
                subj     <- optional $ lookText' "subject"
                text     <- optional $ lookText' "contents"
                let message = Messages.Message {
                      messageId = PostId 0 -- ignored
                    , parent    = Parent thread
                    , section   = Section sec
                    , created   = currTime
                    , author    = Author $ extrct name
                    , subject   = Subject $ extrct subj
                    , contents  = Contents $ extrct text
                    }
                posted <- update' acid (AddPost message)
                let tid = case thread of
                        0 -> do     -- new thread, its id is first post id
                            let PostId newId = messageId posted
                            newId
                        _ -> thread -- old thread
                let slash = pack "/"
                let u = Data.Text.concat [slash, sec, slash, pack $ show tid]
                -- finally, redirect to the thread
                seeOther u (toResponse ())
                
                where
                    extrct = fromMaybe empty


-- yet another wrapper
site :: AcidState Board -> Site Sitemap (ServerPartT IO Response)
site acid =
    --setDefault Home $ mkSitePI (runRouteT $ route acid)
    setDefault Home $ boomerangSite (runRouteT $ route acid) sitemap
