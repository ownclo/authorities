{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Authorities.Schema
import Database.Persist.Postgresql as PSQL
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.Monoid( (<>) )
import Control.Applicative( (<$>) )

data Authorities = Authorities {
        connPool :: PSQL.ConnectionPool
    }

mkYesod "Authorities" [parseRoutes|
/ HomeR GET
/people PeopleR GET POST
/person/#PersonId PersonR GET
|]

instance Yesod Authorities

instance YesodPersist Authorities where
    type YesodPersistBackend Authorities = PSQL.SqlPersistT
    runDB f = PSQL.runSqlPool f =<< connPool `fmap` getYesod

instance YesodPersistRunner Authorities where
    getDBRunner = defaultGetDBRunner connPool


getHomeR :: Handler Value
getHomeR = returnJson $ Person "John Doe"

getPeopleR :: Handler TypedContent
getPeopleR = respondSourceDB typeJson $
    selectSource [] [Asc PersonName]
    $= CL.map (JSON.encode . entityVal)
    $= awaitForever (sendChunkLBS . (<> "\n"))

postPeopleR :: Handler ()
postPeopleR = do
    mperson <- JSON.fromJSON <$> requireJsonBody
    case mperson of
        JSON.Error _ -> {- TODO: HANDLE WRONG JSON -} undefined
        JSON.Success person -> insertPerson person

insertPerson :: Person -> Handler ()
insertPerson person = do
    mpid <- runDB $ insertUnique person
    case mpid of
        Nothing -> {- TODO: HANDLE DOUBLE INSERT -} undefined
        Just personId -> redirect $ PersonR personId

getPersonR :: PersonId -> Handler Value
getPersonR personId = do
    mperson <- runDB $ get personId
    case mperson of
        Nothing -> {- TODO: No such value in the DB -} undefined
        Just person -> returnJson person

connstr :: ConnectionString
connstr = BS.unwords
            [ "host=localhost"
            , "dbname=authorities"
            , "user=authorities"
            , "password=authorities"
            , "port=5432"
            ]

main :: IO ()
main = withPostgresqlPool connstr 10 $ \pool -> do
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ Authorities pool
