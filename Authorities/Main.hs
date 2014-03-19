{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Authorities.Schema
import Authorities.Queries
import Database.Esqueleto

import Database.Persist.Postgresql
import Control.Monad.IO.Class( liftIO )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as TIO

import Control.Error( runMaybeT, MaybeT(..) )
import Control.Monad( void )
import Control.Arrow( (***) )
import Data.Monoid
-- import Control.Monad.Trans( lift )

-- typically provided as YAML config file.
connstr :: ConnectionString
connstr = BS.unwords
            [ "host=localhost"
            , "dbname=authorities"
            , "user=authorities"
            , "password=authorities"
            , "port=5432"
            ]

runDB :: SqlPersistM a -> IO a
runDB = withPostgresqlPool connstr 10 . runSqlPersistMPool

main :: IO ()
main = runDB $ do
    runMigration migrateAll

    -- Insert test relations if not present (abort on first fail).
    void . runMaybeT $ do
      johnId <- MaybeT . insertUnique $ Person "John Doe"
      janeId <- MaybeT . insertUnique $ Person "Jane Doe"

      students <- MaybeT . insertUnique $ Group "Students"
      stuff    <- MaybeT . insertUnique $ Group "Stuff"

      thoughts  <- MaybeT . insertUnique $ Authority "Thought Control"
      education <- MaybeT . insertUnique $ Authority "Hard Partying"

      void . MaybeT . insertUnique $ PersonGroup johnId students
      void . MaybeT . insertUnique $ PersonGroup janeId stuff
      void . MaybeT . insertUnique $ GroupAuthority stuff thoughts
      void . MaybeT . insertUnique $ GroupAuthority students education

      -- XXX: These inserts are made for testing for repeated
      -- authorities for a person. And vice-versa
      void . MaybeT . insertUnique $ PersonGroup johnId stuff
      void . MaybeT . insertUnique $ GroupAuthority students thoughts
      void . MaybeT . insertUnique $ GroupAuthority stuff education

      -- void . lift . deleteCascade $ janeId
      -- void . lift . deleteCascade $ students

    people <- selectList [] [Asc PersonName]
    groups <- selectList [] [Asc GroupName]
    authorities <- selectList [] [Asc AuthorityName]

    Just person    <- selectFirst [] []
    Just group     <- selectFirst [] []
    Just authority <- selectFirst [] []

    -- Heart of the app, queries
    personGroups      <- select $ groupsForPerson person
    groupPeople       <- select $ peopleInGroup group
    groupAuthorities  <- select $ authoritiesForGroup group
    authorityGroups   <- select $ groupsWithAuthority authority
    personAuthorities <- selectDistinct $ authoritiesForPerson person
    authorityPeople   <- selectDistinct $ peopleWithAuthority authority

    let showPerson = personName . entityVal
        showGroup = groupName . entityVal
        showAuthority = authorityName . entityVal
        printPT (a, b) = TIO.putStrLn $ a <> " - " <> b

    liftIO $ putStrLn "Simple one-table queries:"
    liftIO $ mapM_ (TIO.putStrLn . showPerson) people
    liftIO $ mapM_ (TIO.putStrLn . showGroup) groups
    liftIO $ mapM_ (TIO.putStrLn . showAuthority) authorities
    liftIO $ putStrLn ""

    liftIO $ putStrLn "Two-table JOINs: Person-Group"
    liftIO $ mapM_ (printPT . (showPerson *** showGroup) . (person,)) personGroups
    liftIO $ mapM_ (printPT . (showGroup *** showPerson) . (group,)) groupPeople
    liftIO $ putStrLn ""

    liftIO $ putStrLn "Two-table JOINs: Group-Authority"
    liftIO $ mapM_ (printPT . (showGroup *** showAuthority) . (group,)) groupAuthorities
    liftIO $ mapM_ (printPT . (showAuthority *** showGroup) . (authority,)) authorityGroups
    liftIO $ putStrLn ""

    liftIO $ putStrLn "Three-table JOINs: Person-Authority"
    liftIO $ mapM_ (printPT . (showPerson *** showAuthority) . (person,)) personAuthorities
    liftIO $ mapM_ (printPT . (showAuthority *** showPerson) . (authority,)) authorityPeople
