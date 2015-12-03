{-# LANGUAGE 
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    TemplateHaskell,
    TypeOperators #-}

module Routes where

import Prelude              hiding (head, id, (.))
import Control.Applicative  (optional)
import Control.Category     (Category(id, (.)))
import Control.Monad        (msum)
import Control.Monad.Trans  (MonadIO(liftIO))
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query', update')
import Data.Data            (Data, Typeable)
import Data.Maybe           (fromMaybe)
import Data.Text            (Text, pack, concat, empty)
import Data.Time.Clock      (getCurrentTime)
import Happstack.Server
import Text.Boomerang.TH    (makeBoomerangs)
import Web.Routes           (PathInfo(..), RouteT, showURL, runRouteT,
                             Site(..), setDefault, mkSitePI)
import Web.Routes.Boomerang
import Web.Routes.TH        (derivePathInfo)
import Web.Routes.Happstack (implSite)

import Messages
import Render
import Storage

-- make Parent and Section instances of PathInfo, retroactively
$(derivePathInfo ''Parent)
$(derivePathInfo ''Section)

data Sitemap = 
    Home |
    Board Text |
    Thread Text Int
    deriving (Eq, Ord, Data, Typeable)
    
$(derivePathInfo ''Sitemap)
$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap = 
    (  rHome
    <> rBoard . anyText
    <> rThread . (anyText </> int)
    )

route :: AcidState Board -> Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route acid url =
    case url of
        Routes.Home         -> ok $ toResponse "home page will be here\n"
        (Routes.Board b)    -> msum
            [ do method GET
                 messages <- query' acid (ListThreads $ Section b)
                 ok $ toResponse $ renderSection messages
            , post b 0
            --, do method POST
            --     ok $ toResponse "board page POST\n"
            ]
        (Routes.Thread b t) -> msum
            [ do method GET
                 messages <- query' acid (ListThreadPosts $ Parent t)
                 ok $ toResponse $ renderThread messages
            , post b t
            ---, do method POST
            ---     name     <- lookText "author"
            ---     seeOther name (toResponse ())
            ---     --ok $ toResponse name
            ---     --ok $ toResponse "thread page POST\n"
            ]
    
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
