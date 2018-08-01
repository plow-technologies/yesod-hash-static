{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
module Static.Transform (staticFilesListWithXForm
                        ,staticFilesWithXForm) where

import qualified Yesod.Static as Static

         

import System.Directory (getDirectoryContents
                        ,doesFileExist
                        ,doesDirectoryExist )
import Control.Monad (filterM)

import Yesod.Core (liftIO)

import Data.List (intercalate)

import Language.Haskell.TH.Syntax as TH
import Data.Text (pack)
import Crypto.Hash.Conduit (hashFile )
import Crypto.Hash (MD5, Digest)
import Control.Monad.Trans.State (StateT
                                 ,evalStateT
                                 ,get
                                 ,put)

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Char (isLower, isDigit)
import Data.List (foldl')
import qualified Data.ByteString as S

import qualified Data.Text.Lazy as TL
import Data.Default (Default(def))



notHidden :: FilePath -> Bool
notHidden "tmp" = False
notHidden s =
    case s of
        '.':_ -> False
        _ -> True

getFileListPieces :: FilePath -> IO [[String]]
getFileListPieces = flip evalStateT M.empty . flip go id
  where
    go :: String
       -> ([String] -> [String])
       -> StateT (M.Map String String) IO [[String]]
    go fp front = do
        allContents <- liftIO $ filter notHidden `fmap` getDirectoryContents fp
        let fullPath :: String -> String
            fullPath f = fp ++ '/' : f
        files <- liftIO $ filterM (doesFileExist . fullPath) allContents
        let files' = map (front . return) files
        files'' <- mapM dedupe files'
        dirs <- liftIO $ filterM (doesDirectoryExist . fullPath) allContents
        dirs' <- mapM (\f -> go (fullPath f) (front . (:) f)) dirs
        return $ concat $ files'' : dirs'

    -- Reuse data buffers for identical strings
    dedupe :: [String] -> StateT (M.Map String String) IO [String]
    dedupe = mapM dedupe'

    dedupe' :: String -> StateT (M.Map String String) IO String
    dedupe' s = do
        m <- get
        case M.lookup s m of
            Just s' -> return s'
            Nothing -> do
                put $ M.insert s s m
                return s

-- | Template Haskell function that automatically creates routes
-- for all of your static files.
--
-- For example, if you used
--
-- > staticFiles "static/"
--
-- and you had files @\"static\/style.css\"@ and
-- @\"static\/js\/script.js\"@, then the following top-level
-- definitions would be created:
--
-- > style_css    = StaticRoute ["style.css"]    []
-- > js_script_js = StaticRoute ["js", "script.js"] []
--
-- Note that dots (@.@), dashes (@-@) and slashes (@\/@) are
-- replaced by underscores (@\_@) to create valid Haskell
-- identifiers.
--  With Transform 
staticFilesWithXForm :: FilePath -> (FilePath -> FilePath) -> Q [Dec]
staticFilesWithXForm dir xForm = mkStaticFiles  dir xForm   

-- | Same as 'staticFiles', but takes an explicit list of files
-- to create identifiers for. The files path given are relative
-- to the static folder. For example, to create routes for the
-- files @\"static\/js\/jquery.js\"@ and
-- @\"static\/css\/normalize.css\"@, you would use:
--
-- > staticFilesList "static" ["js/jquery.js", "css/normalize.css"]
--
-- This can be useful when you have a very large number of static
-- files, but only need to refer to a few of them from Haskell.
staticFilesListWithXForm :: FilePath -> (FilePath -> FilePath) -> [FilePath] -> Q [Dec]
staticFilesListWithXForm dir xForm fs =
    mkStaticFilesList dir xForm (map split fs) True
  where
    split :: FilePath -> [String]
    split [] = []
    split x =
        let (a, b) = break (== '/') x
         in a : split (drop 1 b)



pathFromRawPieces :: FilePath -> [String] -> FilePath
pathFromRawPieces =
    foldl' append
  where
    append a b = a ++ '/' : b



mkStaticFiles :: FilePath -> 
                   (String -> String)  -> 
                   Q [Dec]
mkStaticFiles fp xForm = mkStaticFiles' fp xForm  True

mkStaticFiles' :: FilePath -- ^ static directory
               -> (String -> String )
               -> Bool     -- ^ append checksum query parameter
               -> Q [Dec]
mkStaticFiles' fp xForm makeHash = do
    fs <- qRunIO $ getFileListPieces fp
    mkStaticFilesList fp xForm fs makeHash

mkStaticFilesList
    :: FilePath -- ^ static directory
    -> (String -> String)
    -> [[String]] -- ^ list of files to create identifiers for
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList fp nameXForm fs makeHash = mkStaticFilesList' fp nameXForm (zip fs fs) makeHash

mkStaticFilesList'
    :: FilePath -- ^ static directory
    -> (String -> String)
    -> [([String], [String])] -- ^ list of files to create identifiers for, where
                              -- the first argument of the tuple is the identifier
                              -- alias and the second is the actual file name
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList' fp nameXform  fs makeHash = do
    concat `fmap` mapM mkRoute fs
  where
    replace' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
          
    mkRoute (alias, f) = do
        let name' = nameXform $ intercalate "_" $ map (map replace') alias            
            routeName = mkName $
                case () of
                    ()
                        | null name' -> error "null-named file"
                        | isDigit (head name') -> '_' : name'
                        | isLower (head name') -> name'
                        | otherwise -> '_' : name'

        f' <- [|map pack $(TH.lift f)|]

        qs <- if makeHash
                    then do hash <- qRunIO $ base64md5File $ pathFromRawPieces fp f
                            [|[(pack "etag", pack $(TH.lift hash))]|]
                    else return $ ListE []
        return
            [ SigD routeName $ ConT ''Static.StaticRoute
            , FunD routeName
                [ Clause [] (NormalB $ (ConE 'Static.StaticRoute) `AppE` f' `AppE` qs) []
                ]
            ]


base64md5File :: FilePath -> IO String
base64md5File = fmap (base64 . encode) . hashFile
    where encode d = ByteArray.convert (d :: Digest MD5)


base64 :: S.ByteString -> String
base64 = map tr
       . take 8
       . S8.unpack
       . Data.ByteString.Base64.encode
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c

-- $combining
--
-- A common scenario on a site is the desire to include many external CSS and
-- Javascript files on every page. Doing so via the Widget functionality in
-- Yesod will work, but would also mean that the same content will be
-- downloaded many times. A better approach would be to combine all of these
-- files together into a single static file and serve that as a static resource
-- for every page. That resource can be cached on the client, and bandwidth
-- usage reduced.
--
-- This could be done as a manual process, but that becomes tedious. Instead,
-- you can use some Template Haskell code which will combine these files into a
-- single static file at compile time.

-- | Data type for holding all settings for combining files.
--
-- This data type is a settings type. For more information, see:
--
-- <http://www.yesodweb.com/book/settings-types>
--
-- Since 1.2.0
data CombineSettings = CombineSettings
    { csStaticDir :: FilePath
    -- ^ File path containing static files.
    --
    -- Default: static
    --
    -- Since 1.2.0
    , csCssPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
    -- ^ Post processing to be performed on CSS files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csJsPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
    -- ^ Post processing to be performed on Javascript files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csCssPreProcess :: TL.Text -> IO TL.Text
    -- ^ Pre-processing to be performed on CSS files.
    --
    -- Default: convert all occurences of /static/ to ../
    --
    -- Since 1.2.0
    , csJsPreProcess :: TL.Text -> IO TL.Text
    -- ^ Pre-processing to be performed on Javascript files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csCombinedFolder :: FilePath
    -- ^ Subfolder to put combined files into.
    --
    -- Default: combined
    --
    -- Since 1.2.0
    }

instance Default CombineSettings where
    def = CombineSettings
        { csStaticDir = "static"
        {- Disabled due to: https://github.com/yesodweb/yesod/issues/623
        , csCssPostProcess = \fps ->
              either (error . (errorIntro fps)) (return . TLE.encodeUtf8)
            . flip luciusRTMinified []
            . TLE.decodeUtf8
        -}
        , csCssPostProcess = const return
        , csJsPostProcess = const return
           -- FIXME The following borders on a hack. With combining of files,
           -- the final location of the CSS is no longer fixed, so relative
           -- references will break. Instead, we switched to using /static/
           -- absolute references. However, when served from a separate domain
           -- name, this will break too. The solution is that, during
           -- development, we keep /static/, and in the combining phase, we
           -- replace /static with a relative reference to the parent folder.
        , csCssPreProcess =
              return
            . TL.replace "'/static/" "'../"
            . TL.replace "\"/static/" "\"../"
        , csJsPreProcess = return
        , csCombinedFolder = "combined"
        }


         
