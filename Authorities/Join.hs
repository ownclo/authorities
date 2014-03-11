{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Authorities.Join where

import Database.Esqueleto

type EntityExpr a = SqlExpr (Entity a)

-- TODO: Is it possible to generalize 'groupPersonJoin'
-- in order to support all pairs of tables joined in that way?
class (SqlEntity left, SqlEntity lr)
      => LeftJoined left lr where
    leftId  :: EntityField lr (Key left)

class (SqlEntity right, SqlEntity lr)
      => RightJoined lr right where
    rightId :: EntityField lr (Key right)

class (LeftJoined left lr, RightJoined lr right)
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
