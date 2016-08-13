{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Messages where

import Data.Data            (Data, Typeable)
import Data.Time.Clock      (UTCTime)
import Data.Text            (Text)


newtype PostId   = PostId Int     deriving (Eq, Ord, Data, Typeable)
newtype Parent   = Parent Int     deriving (Eq, Ord, Data, Typeable)

-- section is a board name such as b or aa
newtype Section  = Section Text   deriving (Eq, Ord, Data, Typeable)

newtype Author   = Author Text    deriving (Eq, Ord, Data, Typeable)
newtype Subject  = Subject Text   deriving (Eq, Ord, Data, Typeable)
newtype Contents = Contents Text  deriving (Eq, Ord, Data, Typeable)

-- message status; could be a Bool but we might need more states
data Status = Present | Removed   deriving (Eq, Ord, Data, Typeable)

data Message = Message { messageId :: PostId
                       , parent    :: Parent
                       , section   :: Section
                       , created   :: UTCTime
                       , author    :: Author
                       , subject   :: Subject
                       , contents  :: Contents
                       , imageName :: Text
                       , imageExt  :: Text
                       , origFile  :: Text
                       , password  :: Text
                       , status    :: Status
                       } deriving (Eq, Ord, Data, Typeable)


