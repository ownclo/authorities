{-# LANGUAGE ScopedTypeVariables #-}
module Authorities.Queries where

import Authorities.Schema
import Authorities.Join( join, EntityExpr )
import Database.Esqueleto

-- NOTE: that '_gp :: EntityExpr PersonGroup' is used
-- to disambiguate the join table used. Can I do better?

-- TODO: That join query is still too repetitive. Abstract
-- that out if possible.
groupsForPerson :: Entity Person -> SqlQuery (EntityExpr Group)
groupsForPerson (Entity pkey _) = from . join $
    \person (_pg :: EntityExpr PersonGroup) group -> do
    where_ (person ^. PersonId ==. val pkey)
    return group

peopleInGroup :: Entity Group -> SqlQuery (EntityExpr Person)
peopleInGroup (Entity gkey _) = from . join $
    \person (_pg :: EntityExpr PersonGroup) group -> do
    where_ (group  ^. GroupId ==. val gkey)
    return person

authoritiesForGroup :: Entity Group -> SqlQuery (EntityExpr Authority)
authoritiesForGroup (Entity gkey _) = from . join $
    \group (_ga :: EntityExpr GroupAuthority) authority -> do
    where_ (group ^. GroupId ==. val gkey)
    return authority

groupsWithAuthority :: Entity Authority -> SqlQuery (EntityExpr Group)
groupsWithAuthority (Entity akey _) = from . join $
    \group (_ga :: EntityExpr GroupAuthority) authority -> do
    where_ (authority ^. AuthorityId ==. val akey)
    return group

authoritiesForPerson :: Entity Person -> SqlQuery (EntityExpr Authority)
authoritiesForPerson person = from . join $
    \group (_ga :: EntityExpr GroupAuthority) authority -> do
    group' <- groupsForPerson person
    where_ (group' ^. GroupId ==. group ^. GroupId)
    return authority

peopleWithAuthority :: Entity Authority -> SqlQuery (EntityExpr Person)
peopleWithAuthority authority = from . join $
    \person (_pg :: EntityExpr PersonGroup) group -> do
    group' <- groupsWithAuthority authority
    where_ (group' ^. GroupId ==. group ^. GroupId)
    return person
