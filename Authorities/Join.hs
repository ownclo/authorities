{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Authorities.Join where

import Database.Esqueleto

type EntityExpr a = SqlExpr (Entity a)
type JoinExpr a b = (EntityExpr a, EntityExpr b)

-- TODO: Is it possible to generalize 'groupPersonJoin'
-- in order to support all pairs of tables joined in that way?
class (SqlEntity left, SqlEntity lr)
      => LeftJoined left lr where
    leftId  :: EntityField lr (Key left)

class (SqlEntity right, SqlEntity lr)
      => RightJoined lr right where
    rightId :: EntityField lr (Key right)

class (LeftJoined left lr,
       RightJoined lr right)
      => Joined left lr right where

join :: (Joined left lr right)
     => (EntityExpr left -> EntityExpr lr -> EntityExpr right -> SqlQuery a)
     -> (EntityExpr left `InnerJoin` EntityExpr lr `InnerJoin` EntityExpr right)
     -> SqlQuery a
join selector (left `InnerJoin` lr `InnerJoin` right) = do
    -- XXX: 'on's must go in the reverse order. Still didn't get the
    -- reason.
    on (right ^. persistIdField ==. lr ^. rightId)
    on (left  ^. persistIdField ==. lr ^. leftId)
    selector left lr right

manyToMany :: (Joined left lr right)
           => Entity left
           -> (EntityExpr left `InnerJoin` EntityExpr lr `InnerJoin` EntityExpr right)
           -> SqlQuery (JoinExpr right lr)
manyToMany (Entity key _) = join $
    \first (jTable :: EntityExpr lr) second -> do
    match first key persistIdField -- undefined -- leftId
    return (second, jTable)
  where match entity aKey anId = where_ (entity ^. anId ==. val aKey)
