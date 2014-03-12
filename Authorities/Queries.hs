{-# LANGUAGE ScopedTypeVariables #-}
module Authorities.Queries where

import Authorities.Schema
import Authorities.Join( join, EntityExpr )
import Database.Esqueleto

-- TODO: The following queries need to be defined:
--  + List of all groups that a person belongs to
--  + List of all people belonging to given group
--  + List of all groups that have given authority
--  + List of all authorities that a given group has
--  - List of all people that has given authority
--  - List of all authorities for particular person

-- XXX: Need to support referential integrity of the
-- database while removing entities. If one removes a
-- group, one also need to remove matching groupId
-- from both GroupAuthority and PersonGroup join tables.
-- Same goes for Person and Authority (one appropriate
-- join table is to be considered).

-- NOTE: that '_gp :: EntityExpr PersonGroup' is used
-- to disambiguate the join table used. Can I do better?

-- TODO: That join query is still too repetitive. Abstract
-- that out.
groupsForPerson :: Entity Person -> SqlPersistM [Entity Group]
groupsForPerson (Entity pkey _) = select . from . join $
    \person (_pg :: EntityExpr PersonGroup) group -> do
    where_ (person ^. PersonId ==. val pkey)
    return group

peopleInGroup :: Entity Group -> SqlPersistM [Entity Person]
peopleInGroup (Entity gkey _) = select . from . join $
    \person (_pg :: EntityExpr PersonGroup) group -> do
    where_ (group  ^. GroupId ==. val gkey)
    return person

authoritiesForGroup :: Entity Group -> SqlPersistM [Entity Authority]
authoritiesForGroup (Entity gkey _) = select . from . join $
    \group (_ga :: EntityExpr GroupAuthority) authority -> do
    where_ (group ^. GroupId ==. val gkey)
    return authority

groupsWithAuthority :: Entity Authority -> SqlPersistM [Entity Group]
groupsWithAuthority (Entity akey _) = select . from . join $
    \group (_ga :: EntityExpr GroupAuthority) authority -> do
    where_ (authority ^. AuthorityId ==. val akey)
    return group

authoritiesForPerson :: Entity Person -> SqlPersistM [Entity Authority]
authoritiesForPerson (Entity pkey _) = undefined

peopleWithAuthority :: Entity Authority -> SqlPersistM [Entity Person]
peopleWithAuthority (Entity akey _) = undefined
