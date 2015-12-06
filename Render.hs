{-# LANGUAGE QuasiQuotes #-}
module Render where

import Control.Monad     (liftM)
import Data.Monoid       (mconcat)
import Data.Text         (Text, pack)
import Data.Time
import Data.Time.Format  ()
import Text.Hamlet
import Web.Routes

import Messages
import Sitemap

-- rendering function for urls in templates
--
convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF =
  (\url params -> maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params)

renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = liftM convRender $ askRouteFn
--
--type UrlRender url = url -> [(Text, Text)] -> Text

-- note: all of the functions below return HtmlUrl Sitemap
-- which expects url rendering function to be supplied
-- client code (in Routes.hs) uses renderFunction defined above
renderMessage :: Message -> HtmlUrl Sitemap
renderMessage message =
    let PostId   postId = messageId message
        Parent   owner  = parent message
        Section  sec    = section message
        Author   name   = author message
        Subject  subj   = subject message
        Contents text   = contents message
        ts              = created message
        time            = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
        -- TODO: pass current timezone to this function?
        --tz              = hoursToTimeZone 0
        --zts             = utcToZonedTime tz ts
    in [hamlet|
<div>
    $case owner
        $of 0
            <a href=@{Thread sec postId} name=#{postId}>#{postId}
        $of _
            <a name=#{postId}>#{postId}
    <label>
        <span>#{name}
        <span>#{subj}
        <span>#{time}
    <div>#{text}
|]

renderMessages :: [Message] -> HtmlUrl Sitemap
renderMessages messages =
    -- need to reverse arguments order, hence lambda
    mconcat $ map (\msg -> renderMessage msg) messages

renderForm :: Text -> Int -> HtmlUrl Sitemap
renderForm sec thread =
    let (action, desc) = if thread == 0
                            then (Board sec,
                                  "new thread")
                            else (Thread sec thread,
                                  "reply to thread #" ++ show thread)
    in [hamlet|
<form method=post action=@{action}>
    <table>
        <tr>
            <td>Subject
            <td><input type="text" name="subject"/>
        <tr>
            <td>Name
            <td><input type="text" name="author"/>
        <tr>
            <td>Message
            <td>
                <textarea name="contents">
        <tr>
            <td>
            <td>
                <input type="submit"/>
                (#{desc})
|]

renderPage :: Text -> Text -> Int -> HtmlUrl Sitemap -> HtmlUrl Sitemap
renderPage title sec thread inner =
    let form = renderForm sec thread
    in [hamlet|
$doctype 5
<html>
    <head>
        <title>#{title}
    <body>
        ^{form}
        ^{inner}
|]

renderSection :: Text -> [Message] -> HtmlUrl Sitemap
renderSection sec messages =
    renderPage (pack "List of threads") sec 0 (renderMessages messages)

renderThread :: Text -> [Message] -> HtmlUrl Sitemap
renderThread sec messages = do
    let thread = case messages of
            [] -> 0
            _  -> i where
                    Message { messageId = PostId i} = head messages
    let title = pack $ "Thread #" ++ show thread
    renderPage title sec thread (renderMessages messages)
