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
import Data.Text            (Text, empty, pack, unpack)
import Data.Time            (defaultTimeLocale)
import Data.Time.Clock      (getCurrentTime)
import Data.Time.Format     (formatTime)
import Graphics.GD          (loadGifFile, loadJpegFile, loadPngFile,
                             saveJpegFile, imageSize, resizeImage)
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
route acid static url = do
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
                del <- optional $ lookText' "delete"
                if fetch del /= empty
                    then do
                        let delId = case reads (unpack (fetch del)) of
                                    [(x, "")] -> x
                                    _ -> 0
                        if delId /= 0
                            then remove (PostId delId)
                            else croak "invalid id\n"
                    else do
                        upload'  <- optional $ lookFile "image"
                        let upload = case upload' of
                                Just (_, "", _) -> Nothing
                                _               -> upload'
                        if not $ acceptableFile upload
                            then croak "file type not supported\n"
                            else do
                                store upload
                where
                    croak :: String -> RouteT Sitemap (ServerPartT IO) Response
                    croak what = do
                        badRequest $ toResponse what
                    
                    store :: Maybe (FilePath, FilePath, ContentType) ->
                             RouteT Sitemap (ServerPartT IO) Response
                    store ul = do
                        urlf     <- renderFunction
                        currTime <- liftIO   $ getCurrentTime
                        name     <- optional $ lookText' "author"
                        subj     <- optional $ lookText' "subject"
                        text     <- optional $ lookText' "contents"
                        pass     <- optional $ lookText' "password"
                        liftIO $ rename ul currTime (unpack static) (unpack sec)
                        let message = Messages.Message {
                              messageId = PostId 0 -- ignored
                            , parent    = Parent thread
                            , section   = Section sec
                            , created   = currTime
                            , author    = Author $ fetch name
                            , subject   = Subject $ fetch subj
                            , contents  = Contents $ fetch text
                            , origFile  = pack $ origName ul
                            , imageName = pack $ imgName ul currTime
                            , imageExt  = pack $ imgExt ul
                            , password  = fetch $ pass
                            , status    = Present
                            }
                        posted <- update' acid (AddPost message)
                        let tid = case thread of
                                -- new thread, its id is first post id:
                                0 -> newId where PostId newId = messageId posted
                                -- old thread:
                                _ -> thread
                        -- generate redirect url using url rendering function
                        -- (second argument is array of url parameters)
                        let u = urlf (Sitemap.Thread sec tid) []
                        -- finally, redirect to the thread
                        seeOther u (toResponse ())
                    
                    remove :: PostId -> RouteT Sitemap (ServerPartT IO) Response
                    remove p@(PostId n) = do
                        pass <- optional $ lookText' "password"
                        removed <- update' acid (MarkDeleted p (fetch pass))
                        if removed
                            then do
                                urlf <- renderFunction
                                let u = if (n == thread) || thread == 0
                                    then urlf (Sitemap.Board sec) []
                                    else urlf (Sitemap.Thread sec thread) []
                                seeOther u (toResponse ())
                            else croak "could not delete\n"
                    
                    fetch = fromMaybe empty
                    
                    origName Nothing = ""
                    origName (Just (_, s, _)) = s
                    
                    imgName Nothing _ = ""
                    imgName (Just (_, _, _)) t =
                        -- %q returns picoseconds, 12 characters
                        -- so s will be long enough
                        let s = formatTime defaultTimeLocale "%s%q" t
                            l = length s
                        in take (l - 6) s
                    
                    imgExt Nothing = ""
                    imgExt (Just (_, _, ctype)) = ctSubtype ctype
                    
                    rename Nothing _ _ _ = return ()
                    rename f@(Just (tmpPath, _, _)) t docroot s = do
                        let name     = (imgName f t)
                            ext      = (imgExt f)
                            baseName = name ++ "." ++ ext
                            newDir   = joinPath [docroot, s, "src"]
                            newPath  = joinPath [newDir, baseName]
                        createDirectoryIfMissing True newDir
                        renameFile tmpPath newPath
                        resize newDir name ext 200
                    
                    resize :: FilePath -> FilePath -> String -> Int -> IO ()
                    resize d name ext width = do
                        let full  = joinPath [d, name ++ "."  ++ ext]
                            small = joinPath [d, name ++ "s." ++ "jpeg"]
                        img <- case ext of
                                "jpeg" -> loadJpegFile full
                                "png"  -> loadPngFile  full
                                "gif"  -> loadGifFile  full
                        (origWidth, origHeight) <- imageSize img
                        let ratio = if origWidth <= width
                                        then 1.0 :: Double
                                        else fromIntegral origWidth /
                                             fromIntegral width
                            height = round (fromIntegral origHeight / ratio)
                        img' <- resizeImage width height img
                        saveJpegFile (-1) small img'
                    
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
