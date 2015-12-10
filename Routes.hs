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
import Data.Text            (Text, empty, unpack)
import Data.Time            (defaultTimeLocale)
import Data.Time.Clock      (getCurrentTime)
import Data.Time.Format     (formatTime)
import Happstack.Server
import System.Directory     (renameFile, createDirectoryIfMissing)
import System.FilePath      (joinPath)
import Web.Routes           (RouteT, runRouteT, Site(..), setDefault)
import Web.Routes.Boomerang
import Web.Routes.Happstack

import Messages
import Render
import Sitemap
import Storage

route :: AcidState Board ->
         Text ->
         Sitemap ->
         RouteT Sitemap (ServerPartT IO) Response
route acid dir url = do
    urlf <- renderFunction
    case url of
        Sitemap.Home         -> ok $ toResponse "home page will be here\n"
        (Sitemap.Board b)    -> msum
            [ do method GET
                 --messages <- query' acid (ListThreads $ Section b)
                 --ok $ toResponse $ renderSection b messages urlf
                 -- listing 5 most recent posts for each thread
                 -- (hardcoded for now)
                 preview <- query' acid (ListThreadPreviews (Section b) 5)
                 ok $ toResponse $ renderSection b preview urlf
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
                upload   <- optional $ lookFile  "image"
                if not $ acceptableFile upload
                    then badRequest $ toResponse "file type not supported"
                else do
                    liftIO $ rename upload currTime (unpack dir) (unpack sec)
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
                    
                    rename Nothing _ _ _ = return ()
                    rename (Just (tmpPath, _, ctype)) t dir sec = do
                        let ts = formatTime defaultTimeLocale "%s%q" t
                            newName = (ts ++ "." ++ (ctSubtype ctype))
                            newDir  = joinPath [dir, sec, "src"]
                            newPath = joinPath [newDir, newName]
                        createDirectoryIfMissing True newDir
                        renameFile tmpPath newPath
                    
                    acceptableFile Nothing = True
                    acceptableFile (Just (_, _, ctype)) =
                        s == "png" || s == "jpeg" || s == "gif"
                        where
                            s = ctSubtype ctype


-- yet another wrapper
site :: AcidState Board -> Text -> Site Sitemap (ServerPartT IO Response)
site acid static =
    --setDefault Home $ mkSitePI (runRouteT $ route acid)
    setDefault Home $ boomerangSite (runRouteT $ route acid static) sitemap
