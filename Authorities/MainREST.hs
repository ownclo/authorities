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

data Authorities = Authorities {
        connPool :: PSQL.ConnectionPool
    }

mkYesod "Authorities" [parseRoutes|
/ HomeR GET
/people PeopleR GET
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
