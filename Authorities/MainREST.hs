{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod
import Authorities.Schema
import Database.Persist.Postgresql as PSQL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Blaze.ByteString.Builder
import Data.Aeson

import Data.Conduit
import qualified Data.Conduit.List as CL

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
getPeopleR = respondSourceDB "application/json" $
    selectSource [] [Asc PersonName]
    $= CL.map entityVal
    $= sendJson

sendJson :: (ToJSON v, Monad m) => Conduit v m (Flush Builder)
sendJson = do
    x <- await
    sendLBS $ (encode x) `LBS.append` "\n"
    yield Flush
 where sendLBS = yield . Chunk . fromLazyByteString

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
