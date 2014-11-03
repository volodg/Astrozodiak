{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Data.Text              (Text)
import           Data.Typeable          (Typeable)
import           Database.Persist.Quasi
import           Prelude
import           Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON Dream where
    toJSON (Dream word context _ dreamTypeFld) = object
        [ "word"    .= word
        , "context" .= context
        , "type"    .= dreamTypeFld
        ]

instance ToJSON Quest where
    toJSON (Quest name _) = object
        [ "name" .= name ]

instance ToJSON TestResult where
    toJSON (TestResult minV maxV result _) = object
        [ "min"    .= minV
        , "max"    .= maxV
        , "result" .= result
        ]

instance ToJSON Answer where
    toJSON (Answer text points _) = object
        [ "text"   .= text
        , "points" .= points
        ]
