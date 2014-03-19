{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger( logStdoutDev )

import Control.Monad.IO.Class( MonadIO, liftIO )

import qualified Database.Persist.Postgresql as PSQL
import qualified Data.ByteString.Char8 as BS

import Authorities.Schema

-- typically provided as YAML config file.
connstr :: PSQL.ConnectionString
connstr = BS.unwords
            [ "host=localhost"
            , "dbname=authorities"
            , "user=authorities"
            , "password=authorities"
            , "port=5432"
            ]

runDBPool :: MonadIO m => PSQL.ConnectionPool -> PSQL.SqlPersistM a -> m a
runDBPool pool action = liftIO $ PSQL.runSqlPersistMPool action pool

-- TODO: Switch to conduits. Otherwise the whole list of people gets
-- loaded into memory. Cannot marry together PSQL.selectSource and
-- Scotty.source. How does Pool and SqlPersistM and Source live together?
getPeople :: PSQL.ConnectionPool -> ActionM ()
getPeople pool = do
    people <- runDBPool pool $ PSQL.selectList [] [PSQL.Asc PersonName]
    json $ PSQL.entityVal `map` people

getGroups :: PSQL.ConnectionPool -> ActionM ()
getGroups pool = do
    groups <- runDBPool pool $ PSQL.selectList [] [PSQL.Asc GroupName]
    json $ PSQL.entityVal `map` groups

getAuthorities :: PSQL.ConnectionPool -> ActionM ()
getAuthorities pool = do
    auths <- runDBPool pool $ PSQL.selectList [] [PSQL.Asc AuthorityName]
    json $ PSQL.entityVal `map` auths


getPerson :: PSQL.ConnectionPool -> ActionM ()
getPerson _pool = undefined

getGroup :: PSQL.ConnectionPool -> ActionM ()
getGroup _pool = undefined

getAuthority :: PSQL.ConnectionPool -> ActionM ()
getAuthority _pool = undefined

main :: IO ()
main = PSQL.withPostgresqlPool connstr 10 $ \pool -> do
    runDBPool pool $ PSQL.runMigration migrateAll

    scotty 3000 $ do
        middleware logStdoutDev


        {- GET REQUESTS -}
        -- get-all queries
        get "/people"      $ getPeople pool
        get "/groups"      $ getGroups pool
        get "/authorities" $ getAuthorities pool

        -- simple queries by ID (or by unique constraint?)
        get "/person/:id"    $ getPerson pool
        get "/group/:id"     $ getGroup pool
        get "/authority/:id" $ getAuthority pool

        -- one-time join queries
        get "/person/:id/groups"     $ undefined pool
        get "/group/:id/people"      $ undefined pool
        get "/group/:id/authorities" $ undefined pool
        get "/authority/:id/groups"  $ undefined pool

        -- two-times join queries
        get "/person/:id/authorities" $ undefined pool
        get "/authority/:id/people"   $ undefined pool


        -- XXX: PUT is IDEMPOTENT, while POST is not.
        -- "Use PUT when you can update a resource completely
        -- through a specific resource location. ... If you do
        -- not know the actual resource location, i.e. you do
        -- not know where to store the new data, use POST to URL and
        -- let the server decide where to store it".

        {- PUT REQUESTS -}
        -- put new top-level entity. Will id be returned?
        -- Is modification allowed? Use POST for that. For now,
        -- there is no data in top-level entities to modify.
        post "/person"    $ undefined pool
        post "/group"     $ undefined pool
        post "/authority" $ undefined pool

        -- put relationships. will REST God accept that?
        put "/person/:pid/group/:gid"    $ undefined pool
        put "/group/:gid/authority/:aid" $ undefined pool


        {- DELETE REQUESTS -}
        -- top-level deletions will result into cascading deletes.
        delete "/person"    $ undefined pool
        delete "/group"     $ undefined pool
        delete "/authority" $ undefined pool

        delete "/person/:pid/group/:gid"    $ undefined pool
        delete "/group/:gid/authority/:aid" $ undefined pool

        -- Will 'ensure that person has no that particular
        -- authority' be useful? Delete person from all groups
        -- that have such an authority? Probably that's too risky.
