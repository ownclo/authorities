{-# LANGUAGE OverloadedStrings #-}

module Main where

import Authorities.Schema

import Database.Persist.Postgresql
import Control.Monad.IO.Class( liftIO )
import qualified Data.ByteString.Char8 as BS

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

        -- _johnId <- insert $ Person "John Doe"
        -- _janeId <- insert $ Person "Jane Doe"

        people <- selectList [] [Asc PersonName]
        liftIO $ print people
