--------------------------------------------------------------------------------
-- | Implements a basic static file server for previewing options
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Preview.Server
    ( staticServer
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import qualified Snap.Core           as Snap
import qualified Snap.Http.Server    as Snap
import qualified Snap.Util.FileServe as Snap
import qualified Data.HashMap.Strict as M
import Data.Text as T
import Text.HTML.TagSoup


--------------------------------------------------------------------------------
-- | Serve a given directory
static :: FilePath             -- ^ Directory to serve
       -> (FilePath -> IO ())  -- ^ Pre-serve hook
       -> Snap.Snap ()
static directory preServe =
    Snap.serveDirectoryWith directoryConfig directory
  where
    directoryConfig :: Snap.DirectoryConfig Snap.Snap
    directoryConfig = Snap.fancyDirectoryConfig
        { Snap.preServeHook = liftIO . preServe
        , Snap.dynamicHandlers = M.fromList [ (".html", insertAutoReload)
                                            , (".htm", insertAutoReload)
                                            ]
        }
    insertAutoReload file = do
        file' <- liftIO $ readFile file
        Snap.writeText . T.pack . insertScript $ file'
    insertScript = renderTags . insertScript' . parseTags
    insertScript' [] = []
    insertScript' (x@(TagOpen "head" _):xs) = x:(script++xs)
    insertScript' (x:xs) = x:(insertScript' xs)
    script = parseTags ("<script async type=\"text/javascript\">console.log(\"Hello World\");</script>" :: String)


--------------------------------------------------------------------------------
-- | Main method, runs a static server in the given directory
staticServer :: FilePath             -- ^ Directory to serve
             -> (FilePath -> IO ())  -- ^ Pre-serve hook
             -> Int                  -- ^ Port to listen on
             -> IO ()                -- ^ Blocks forever
staticServer directory preServe port =
    Snap.httpServe config $ static directory preServe
  where
    -- Snap server config
    config = Snap.setPort      port
           $ Snap.setAccessLog Snap.ConfigNoLog
           $ Snap.setErrorLog  Snap.ConfigNoLog
           $ Snap.emptyConfig
