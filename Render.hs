{-# LANGUAGE QuasiQuotes #-}
module Render where

import Control.Monad     (liftM)
import Data.List         (intersperse)
import Data.Monoid       (mconcat)
import Data.Text         (Text, empty, pack)
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
        imgName         = imageName message
        imgExt          = imageExt message
        ts              = created message
        time            = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
        -- TODO: pass current timezone to this function?
        --tz              = hoursToTimeZone 0
        --zts             = utcToZonedTime tz ts
        img = image sec imgName imgExt
    in [hamlet|
<div class="container">
    <div class="post">
        <label>
            <input type="checkbox" name="delete" value="#{postId}"/>
            $case owner
                $of 0
                    <a href=@{Thread sec postId} name=#{postId}>#{postId}
                $of _
                    <a name=#{postId}>#{postId}
            <span class="subject">#{subj}
            <span class="author">#{name}
            <span class="time">#{time}
        <div class="message">
            ^{img}
            <blockquote>#{text}
|]
    where
        image sec name ext =
            let largeName = if name /= empty
                            then mconcat [name, pack ".", ext]
                            else empty
                smallName = if name /= empty
                            then mconcat [name, pack "s.jpeg"]
                            else empty
                src = pack "src" -- hardcoded location for images
            in if name == empty
                then [hamlet||]
                else [hamlet|
<span>
    <a href=@{File sec src largeName}>#{largeName}</a>
    <br>
    <a href=@{File sec src largeName}>
        <img src=@{File sec (pack "src") smallName} class="preview">
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
            <td>Image
            <td><input type="file" name="image"/>
        <tr>
            <td>Password
            <td><input type="password" name="password"/>
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
        <hr .separator>
        <form method=post action=@{Thread sec thread}>
            ^{inner}
            <hr .separator>
            <div class="del">
                Password
                <input type="password" name="password" size="8"/>
                <input value="Delete" type="submit"/>
|]

renderSectionLite :: Text -> [Message] -> HtmlUrl Sitemap
renderSectionLite sec messages =
    renderPage (pack "List of threads") sec 0 (renderMessages messages)

renderSection :: Text -> [[Message]] -> HtmlUrl Sitemap
renderSection sec threads =
    let sep = [hamlet|<hr .separator>|]
        lst = intersperse sep $ map renderMessages threads
    in renderPage (pack "List of threads") sec 0 $ mconcat lst

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
.dark input[type="text"], input[type="file"], input[type="password"], textarea {
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
.dark .separator {
    border: 0;
    height: 1px;
    color: #404040;
    background-color: #404040;
}
.dark .message > blockquote {
    margin: 4px;
}
.dark .preview {
    float: left;
    margin-right: 16px;
}
.dark .del {
    float: right;
    //margin-left: auto;
    //margin-right: 0;
}
|] undefined
