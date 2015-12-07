{-# LANGUAGE QuasiQuotes #-}
module Render where

import Control.Monad     (liftM)
import Data.Monoid       (mconcat)
import Data.Text         (Text, pack)
import Data.Time
import Data.Time.Format  ()
import Text.Hamlet
import Text.Lucius
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
<div class="container">
    <div class="post">
        $case owner
            $of 0
                <a href=@{Thread sec postId} name=#{postId}>#{postId}
            $of _
                <a name=#{postId}>#{postId}
        <label>
            <span class="subject">#{subj}
            <span class="author">#{name}
            <span class="time">#{time}
        <div class="message">#{text}
|]

renderMessages :: [Message] -> HtmlUrl Sitemap
renderMessages messages =
    mconcat $ map renderMessage messages

renderForm :: Text -> Int -> HtmlUrl Sitemap
renderForm sec thread =
    let (action, desc) = if thread == 0
                            then (Board sec,
                                  "new thread")
                            else (Thread sec thread,
                                  "reply to thread #" ++ show thread)
    in [hamlet|
<form method=post action=@{action} enctype="multipart/form-data">
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
        <link rel="stylesheet" type="text/css" href="/style.css"/>
    <body class="dark">
        ^{form}
        ^{inner}
|]

renderSection :: Text -> [Message] -> HtmlUrl Sitemap
renderSection sec messages =
    renderPage (pack "List of threads") sec 0 (renderMessages messages)

renderThread :: Text -> [Message] -> HtmlUrl Sitemap
renderThread sec messages =
    let thread = case messages of
            [] -> 0
            _  -> i where
                    Message { messageId = PostId i} = head messages
        title = pack $ "Thread #" ++ show thread
    in renderPage title sec thread (renderMessages messages)

stylesheet :: Css
stylesheet = [lucius|
.dark {
    background-color: #202020;
    color: #BBBBBB;
    font-family: sans-serif;
}
.dark a:link {
    color: #0099FF;
}
.dark a:visited {
    color: #CC33FF;
}
.dark input[type="text"], textarea {
    color: #EEEEEE;
    background-color :#333333;
    border: 1px solid #555555;
}
.dark .post {
    margin: 2px;
    padding: 4px;
    background-color: #282828;
    display: inline-block;
}
.dark .author {
    color: #00FF99;
}
.dark .subject {
    color: #FF4040;
}
.dark .time {
    color: #808080;
}
|] undefined
