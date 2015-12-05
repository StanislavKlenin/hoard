{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Messages where

import Data.Data            (Data, Typeable)
import Data.SafeCopy        (SafeCopy)
import Data.Time.Clock      (UTCTime)
import Data.Text            (Text)


newtype PostId   = PostId Int     deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Parent   = Parent Int     deriving (Eq, Ord, Data, Typeable, SafeCopy)

-- section is a board name such as b or aa
newtype Section  = Section Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype Author   = Author Text    deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Subject  = Subject Text   deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Contents = Contents Text  deriving (Eq, Ord, Data, Typeable, SafeCopy)



data Message = Message { messageId :: PostId
                       , parent    :: Parent
                       , section   :: Section
                       , created   :: UTCTime
                       , author    :: Author
                       , subject   :: Subject
                       , contents  :: Contents
                       } deriving (Eq, Ord, Data, Typeable)


