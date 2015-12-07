{-# LANGUAGE
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    OverloadedStrings,
    TemplateHaskell,
    TypeFamilies #-}

module Storage where

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            (Update, Query, makeAcidic)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable(..), IxSet, (@=), (|||), (&&&),
                             Proxy(..), getOne, ixFun, ixSet,
                             toAscList, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (base, deriveSafeCopy)
import Data.Time.Clock      (UTCTime)

import Messages

-- Message defined elsewhere but deriveSafeCopy is here
$(deriveSafeCopy 0 'base ''Message)

instance Indexable Message where
    empty = ixSet
        [ ixFun $ \bp -> [ messageId bp ]
        , ixFun $ \bp -> [ parent bp ]
        , ixFun $ \bp -> [ author bp ]
        , ixFun $ \bp -> [ subject bp ]
        , ixFun $ \bp -> [ contents bp ]
        , ixFun $ \bp -> [ created bp ]
        , ixFun $ \bp -> [ section bp ]
        ]

-- Board: collection of messages with (semi)auto-incremented ids
data Board = Board { nextPostId :: PostId
                   , posts      :: IxSet Message
                   } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Board)

initialBoardState :: Board
initialBoardState =
    Board { nextPostId = PostId 1
          , posts      = empty
          }

-- low level store function performing id increment
addPost :: Message -> Update Board Message
addPost message = do
    board <- get
    let PostId latest = nextPostId board
    let incremented = succ latest
    -- make a new post out of message, replacing an id:
    -- (there is no need to list all fields, this suffices)
    let post = message { messageId = PostId latest }
    -- and update the whole board in the storage
    put Board { nextPostId = PostId incremented
              , posts      = IxSet.insert post (posts board)
              }
    return post
    -- TODO: forbid adding posts to nonexistent threads

postById :: PostId -> Query Board (Maybe Message)
postById postId = do
    board <- ask
    return $ getOne $ (posts board) @= postId

-- list posts in a given section (board) with given parent
-- not sure if it works
listPosts :: Section -> Parent -> Query Board [Message]
listPosts sec starter = do
    board <- ask
    return $
         toDescList (Proxy :: Proxy UTCTime) $
             (posts board) @= sec @= starter

-- list of first posts (OPs)
listThreads :: Section -> Query Board [Message]
listThreads sec = do
    board <- ask
    let root = Parent 0 -- make it a module constant? how?
    return $ toDescList (Proxy :: Proxy UTCTime) $ (posts board) @= sec @= root
    -- TODO: order by last post time (not thread post time)

-- list of "thread previews",
-- where each preview is he first post and several latest ones
listThreadPreviews :: Section -> Int -> Query Board [[Message]]
listThreadPreviews sec count = do
    board <- ask
    let messages = posts board
        root = Parent 0
        firstPosts =
            toDescList (Proxy :: Proxy UTCTime) $ messages @= sec @= root
        threads =
            map (\msg ->
                       let PostId op = messageId msg
                           thr = Parent op
                           thread = toDescList (Proxy :: Proxy UTCTime) $
                                               (messages @= thr @= sec)
                           latest = reverse $ take count thread
                       in msg : latest)
                firstPosts
    return threads


threadExists :: Parent -> Query Board Bool
threadExists (Parent t) = do
    post  <- postById $ PostId t
    return $ case post of
            Just Message {parent = Parent p} -> p == 0
            Nothing -> False

listThreadPosts :: Section -> Parent -> Query Board [Message]
listThreadPosts sec thr@(Parent p) = do
    board <- ask
    let op = PostId p
    let messages = posts board
    let thread = toAscList (Proxy :: Proxy UTCTime) $
            (messages @= sec &&& (messages @= thr |||
                                  messages @= op))
    return $ case thread of
            -- if the only item in a list has non-zero parent,
            -- then it is not a thread, so return an empty list
            [Message {parent = Parent t}] -> if t == 0 then thread else []
            -- in any other case, return our list as is
            _ -> thread

$(makeAcidic ''Board [ 'addPost
                     , 'postById
                     , 'listPosts
                     , 'listThreads
                     , 'listThreadPosts
                     , 'listThreadPreviews
                     ])
