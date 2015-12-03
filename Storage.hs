{-# LANGUAGE
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    OverloadedStrings,
    TemplateHaskell,
    TypeFamilies #-}

module Storage where

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            (AcidState, Update, Query,
                             makeAcidic, openLocalState)
import Data.Data            (Data, Typeable)
import Data.IxSet           (Indexable(..), IxSet(..), (@=), (|||),
                             Proxy(..), getOne, ixFun, ixSet,
                             toAscList, toDescList)
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Time.Clock      (UTCTime )

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
    -- have to copy the message here, this is not good / need another way
    -- we are already within monadic code, need to leverage that somehow
    --let post = Message { messageId = PostId incremented
    --                   , parent    = parent message
    --                   , section   = section message
    --                   , created   = created message
    --                   , author    = author message
    --                   , subject   = subject message
    --                   , contents  = contents message
    --                   }
    -- actually, there is no need to repeat all fields, this suffices:
    let post = message { messageId = PostId latest }
    -- on the other hand, the following just makes a new board
    -- so maybe making a new message above is standard practice?
    put Board { nextPostId = PostId incremented
              , posts      = IxSet.insert post (posts board)
              }
    return post

postById :: PostId -> Query Board (Maybe Message)
postById postId = do
    board <- ask
    return $ getOne $ (posts board) @= postId

-- TODO: list posts (with some conditions) and pagination

-- list posts in a given section (board) with given parent
-- not sure if it works
listPosts :: Section -> Parent -> Query Board [Message]
listPosts sec starter = do
    board <- ask
    return $
         toDescList (Proxy :: Proxy UTCTime) $
             (posts board) @= sec @= starter

listThreads :: Section -> Query Board [Message]
listThreads sec = do
    board <- ask
    let root = Parent 0 -- make it a module constant? how?
    return $ toDescList (Proxy :: Proxy UTCTime) $ (posts board) @= sec @= root

listThreadPosts :: Parent -> Query Board [Message]
listThreadPosts thread@(Parent p) = do
    board <- ask
    let op = PostId p
    let messages = posts board
    return $ toAscList (Proxy :: Proxy UTCTime) $ (messages @= thread |||
                                                   messages @= op)
    -- TODO: order by last post time (not thread post time)

$(makeAcidic ''Board [ 'addPost
                     , 'postById
                     , 'listPosts
                     , 'listThreads
                     , 'listThreadPosts
                     ])
