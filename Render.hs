{-# LANGUAGE QuasiQuotes #-}
module Render where

import Control.Monad     (liftM)
import Data.Monoid       (mappend, mconcat)
import Data.Text         (Text, pack, unpack, reverse, toUpper)
import Data.Time
import Data.Time.Format
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

-- where does that Html type come from, I wonder
-- should be from Blaze
renderMessage :: Message -> Html
renderMessage message =
    let PostId   postId = messageId message
        Subject  subj   = subject message
        Contents text   = contents message
        Author   name   = author message
        ts              = created message
        time            = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" ts
    in [hamlet|
<div>
    <a name=#{postId}>
    <label>
        <span>#{postId}
        <span>#{name}
        <span>#{subj}
        <span>#{time}
    <div>#{text}
|] undefined -- no url rendering

renderTwo :: Message -> Message -> Html
renderTwo msgLeft msgRight =
    let html1 = renderMessage msgLeft
        html2 = renderMessage msgRight
    in  html1 `mappend` html2
    --in html1 <> html2

renderMessages :: [Message] -> Html
renderMessages messages = mconcat $ map renderMessage messages

renderSection :: [Message] -> Html
renderSection = renderMessages

-- TODO: this will render the form
renderThread :: [Message] -> Html
renderThread = renderMessages

-- TODO: common page elements (head, body, ...)
