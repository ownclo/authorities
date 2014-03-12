{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Authorities.Schema where

import Database.Persist.TH
import Data.Text
import Authorities.Join

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

instance LeftJoined Person PersonGroup where
        leftId = PersonGroupPersonId

instance RightJoined PersonGroup Group where
        rightId = PersonGroupGroupId

instance Joined Person PersonGroup Group


instance LeftJoined Group GroupAuthority where
        leftId = GroupAuthorityGroupId

instance RightJoined GroupAuthority Authority where
        rightId = GroupAuthorityAuthorityId

instance Joined Group GroupAuthority Authority
