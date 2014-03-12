{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Authorities.Schema
import Authorities.Queries

import Database.Persist.Postgresql
import Control.Monad.IO.Class( liftIO )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as TIO

import Control.Error( runMaybeT, MaybeT(..) )
import Control.Monad( void )
import Control.Arrow( (***) )
import Data.Monoid

-- typically provided as YAML config file.
connstr :: ConnectionString
connstr = BS.unwords
            [ "host=localhost"
            , "dbname=authorities"
            , "user=authorities"
            , "password=authorities"
            , "port=5432"
            ]

main :: IO ()
main = withPostgresqlPool connstr 10 $ \pool ->
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        -- Insert test relations if not present (abort on first fail).
        _ <- runMaybeT $ do
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

        people <- selectList [] [Asc PersonName]
        groups <- selectList [] [Asc GroupName]
        authorities <- selectList [] [Asc AuthorityName]

        Just person    <- selectFirst [] []
        Just group     <- selectFirst [] []
        Just authority <- selectFirst [] []

        -- Heart of the app, queries
        personGroups <- groupsForPerson person
        groupPeople <- peopleInGroup group
        groupAuthorities <- authoritiesForGroup group
        authorityGroups <- groupsWithAuthority authority

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
