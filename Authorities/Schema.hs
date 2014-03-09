{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Authorities.Schema where

import Database.Persist.TH
import Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    UniquePerson name
    deriving Show

Group
    name Text
    UniqueGroup name
    deriving Show

Authority
    name Text
    UniqueAuthority name
    deriving Show

PersonGroup
    personId PersonId
    groupId  GroupId
    UniquePersonGroup personId groupId

GroupAuthority
    groupId     GroupId
    authorityId AuthorityId
    UniqueGroupAuthority groupId authorityId
|]
