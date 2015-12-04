{-# LANGUAGE QuasiQuotes #-}
module Render where

import Control.Monad     (liftM)
import Data.Monoid       (mappend, mconcat)
import Data.Text         (Text, pack, unpack, reverse, toUpper)
import Data.Time
import Data.Time.Format  ()
--import System.Locale
import Text.Hamlet
import Web.Routes

import Messages

-- rendering function for urls in templates (not used yet)
--
convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF =
  (\url params -> maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params)

renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = liftM convRender $ askRouteFn
--
type UrlRender url = url -> [(Text, Text)] -> Text

-- where does that Html type come from, I wonder
-- should be from Blaze
renderMessage :: Message -> UrlRender url -> Html
renderMessage message urlf =
    let PostId   postId = messageId message
        Subject  subj   = subject message
        Contents text   = contents message
        Author   name   = author message
        ts              = created message
        time            = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
        -- TODO: pass current timezone to this function?
        --tz              = hoursToTimeZone 0
        --zts             = utcToZonedTime tz ts
    in [hamlet|
<div>
    <a name=#{postId}>
    <label>
        <span>#{postId}
        <span>#{name}
        <span>#{subj}
        <span>#{time}
    <div>#{text}
|] urlf

renderMessages :: [Message] -> UrlRender url -> Html
renderMessages messages urlf =
    -- need to reverse arguments order, hence lambda
    mconcat $ map (\msg -> renderMessage msg urlf) messages

-- TODO: specify type
renderPage title inner  = [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
    <body>
        ^{inner}
|] 

-- TODO: specify type
renderSection messages urlf =
    renderPage (pack "List of threads") (renderMessages messages) urlf

-- TODO: specify type
renderThread messages urlf =
    renderPage (pack "Thread") (renderMessages messages) urlf
