{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


module Main where

import Authorities.Schema
import Authorities.Queries

import Database.Persist.Postgresql
import Control.Monad.IO.Class( liftIO )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as TIO

import Control.Error( runMaybeT, MaybeT(..) )
import Control.Monad( void )

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

        Just person <- selectFirst [] []
        personGroups <- groupsForPerson person

        Just group <- selectFirst [] []
        groupPeople <- peopleForGroup group

        liftIO $ mapM_ (TIO.putStrLn . personName . entityVal) people
        liftIO $ mapM_ (TIO.putStrLn . groupName . entityVal) groups
        liftIO $ mapM_ (TIO.putStrLn . authorityName . entityVal) authorities
        liftIO $ putStrLn ""

        liftIO $ mapM_ (TIO.putStrLn . groupName . entityVal) personGroups
        liftIO $ mapM_ (TIO.putStrLn . personName . entityVal) groupPeople
