{-# LANGUAGE QuasiQuotes #-}
module Render where

import Control.Monad     (liftM)
import Data.Monoid       ((<>), mappend, mconcat)
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
        Parent   owner  = parent message
        Section  sec    = section message
        Author   name   = author message
        Subject  subj   = subject message
        Contents text   = contents message
        slash           = pack "/"
        pustUrl         = slash <> sec <> slash <> pack (show $ postId)
        ts              = created message
        time            = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
        -- TODO: pass current timezone to this function?
        --tz              = hoursToTimeZone 0
        --zts             = utcToZonedTime tz ts
    in [hamlet|
<div>
    $case owner
        $of 0
            <a href=#{pustUrl} name=#{postId}>#{postId}</a>
        $of _
            <a name=#{postId}>#{postId}</a>
    <label>
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
renderForm actionUrl = [hamlet|
<form method=post action=#{actionUrl}>
    <table>
        <tr>
            <td>Subject
            <td><input type=text name=subject>
        <tr>
            <td>Name
            <td><input type=text name=author>
        <tr>
            <td>Message
            <td>
                <textarea name=contents>
        <tr>
            <td>
            <td><input type=submit>
|]

-- TODO: specify type
renderPage title sec thread inner =
    let slash = pack "/"
        url = case thread of
                0 -> slash <> sec
                _ -> slash <> sec <> slash <> pack (show thread)
        form = renderForm url
    in [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
    <body>
        ^{form}
        ^{inner}
|] 

-- TODO: specify type
renderSection sec messages urlf =
    renderPage (pack "List of threads") sec 0 (renderMessages messages) urlf

-- TODO: specify type
renderThread sec messages urlf = do
    let thread = case messages of
            [] -> 0
            _  -> i where
                    Message { messageId = PostId i} = head messages
    let title = pack $ "Thread #" ++ show thread
    renderPage title sec thread (renderMessages messages) urlf
