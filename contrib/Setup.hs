-- ------------------------------------------------------ --
-- Copyright © 2013, 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

import Data.List
import Data.Maybe

import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Text
import Distribution.InstalledPackageInfo
import Distribution.Simple.PackageIndex
import System.FilePath
import System.Directory

import Control.Monad

hasteFlag = "haste"
jsffiProg = simpleProgram "haste-ffi-parser"

-- -------------------------------------------------------------------------- --
-- extract information from LocalBuildInfo

-- | package identitifer
--
pname :: LocalBuildInfo -> PackageIdentifier
pname = package . localPkgDescr

-- | The path to the javascript libray in the build dir
--
buildJsLib :: LocalBuildInfo -> FilePath
buildJsLib lbi = pkgBuildJsLib (pname lbi) lbi

pkgBuildJsLib :: PackageIdentifier -> LocalBuildInfo -> FilePath
pkgBuildJsLib pkg lbi = jsBuildDir lbi </> js (display pkg)

-- | The path to the javascript library in the install dir
--
instJsLib :: LocalBuildInfo -> FilePath
instJsLib lbi = pkgInstJsLib (pname lbi) lbi

pkgInstJsLib :: PackageIdentifier -> LocalBuildInfo -> FilePath
pkgInstJsLib pkg lbi = jsInstDir lbi </> js (display pkg)

jsBuildDir :: LocalBuildInfo -> FilePath
jsBuildDir lbi = buildDir lbi </> "javascript-sources"

jsInstDir :: LocalBuildInfo -> FilePath
jsInstDir lbi = libPref </> "javascript-sources"
    where
    InstallDirs{ libdir = libPref } = absoluteInstallDirs (localPkgDescr lbi) lbi NoCopyDest

extraSources :: LocalBuildInfo -> [FilePath]
extraSources = extraSrcFiles . localPkgDescr

js :: FilePath -> FilePath
js = (<.> "js")

-- | copy javascript file of given package to the build directory
--
-- Returns the target Path
--
copyInstJsLibPkg :: Verbosity -> LocalBuildInfo ->  InstalledPackageInfo -> IO (Maybe FilePath)
copyInstJsLibPkg verbosity lbi InstalledPackageInfo{ sourcePackageId = pkgId, libraryDirs = libDirs } = do
    let files = map (</> "javascript-sources" </> display pkgId <.> "js") libDirs
    info verbosity $ "check for javascript libs: " ++ (display pkgId)

    filterM doesFileExist files >>= \x -> case x of
        (h:_) -> do
            createDirectoryIfMissingVerbose verbosity True (jsBuildDir lbi)
            copyFileVerbose verbosity h (pkgBuildJsLib pkgId lbi)
            return . Just $ pkgBuildJsLib pkgId lbi
        _ -> return Nothing

-- | Javascript files within extra sources
--
extraJsSources :: LocalBuildInfo -> [FilePath]
extraJsSources lbi = filter (isSuffixOf ".js") (extraSources lbi)

getBuildJsSources :: LocalBuildInfo -> IO [FilePath]
getBuildJsSources lbi = doesDirectoryExist (jsBuildDir lbi) >>= \x -> case x of
    True -> (map (jsBuildDir lbi </>) . filter (isSuffixOf ".js")) `liftM` getDirectoryContents (jsBuildDir lbi)
    False -> return []

-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = defaultMainWithHooks
    simpleUserHooks
        { hookedPreProcessors = [("jsffi", jsffi)]
        , hookedPrograms = [jsffiProg]

        -- We choose the build hooks based on whether the "haste" flag is set
        , instHook = \a b c d -> if hasFlag b hasteFlag
            then installJsffi a b c d
            else instHook simpleUserHooks a b c d
        , buildHook = \a b c d -> if hasFlag b hasteFlag
            then buildHaste a b c d
            else buildHook simpleUserHooks a b c d
        }
  where
    hasFlag lbi f = fromMaybe False $ lookup (FlagName f) (configConfigurationsFlags $ configFlags lbi)

-- | haste-ffi-parser prepprocessor
--
jsffi :: BuildInfo -> LocalBuildInfo -> PreProcessor
jsffi _ lbi = PreProcessor
    { platformIndependent = False,
      runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
          info verbosity $ "run preprocessor on " ++ inFile
          createDirectoryIfMissingVerbose maxBound True (jsBuildDir lbi)
          runDbProgram verbosity jsffiProg (withPrograms lbi)
              ["-i", inFile, "-o", outFile, "-j", jsBuildDir lbi </> js (takeBaseName outFile)]
    }

-- | Installation For javascript sources
--
installJsffi :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags  -> IO ()
installJsffi desc lbi hooks flags = do
    let verbosity = fromFlag $ installVerbosity flags

    -- append extra javscript sources
    createDirectoryIfMissingVerbose verbosity True (jsBuildDir lbi)
    whenM (doesFileExist (buildJsLib lbi)) $ removeFile (buildJsLib lbi)
    jsSources <- getBuildJsSources lbi
    forM_ jsSources $ \s -> do
        c <- readFile s
        appendFile (buildJsLib lbi) c
    forM_ (extraJsSources lbi) $ \s -> do
        c <- readFile s
        appendFile (buildJsLib lbi) c

    -- install javascript library
    createDirectoryIfMissingVerbose verbosity True (jsInstDir lbi)
    copyFileVerbose' verbosity (buildJsLib lbi) (instJsLib lbi)

    -- Run default install hook
    (instHook simpleUserHooks) desc lbi hooks flags

-- | Build with haste-compiler
--
buildHaste :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHaste pkgDescr lbi hooks flags = do
    let verbosity = fromFlag $ buildVerbosity flags

    -- Copy javascript library files from executable dependencies to build tree
    info verbosity "Copy javascript library files for executable dependencies to build tree"
    -- withExeLBI pkgDescr lbi $ \_ (ExeComponentLocalBuildInfo clbi) ->
    --    forM_ clbi $ \(_, name) ->
    --        getJsLibPkg verbosity lbi (display name)

    externalJsSources <- liftM catMaybes $
        forM (allPackages $ installedPkgs lbi) $ copyInstJsLibPkg verbosity lbi

    -- Reconfigure ghc options
    info verbosity "Collect javascript libraries"
    let jsSources = extraJsSources lbi ++ externalJsSources
    info verbosity $ "Register compiler options for linking javascript libraries: " ++ show jsSources
    let flags' = flags
            { buildProgramArgs = ("ghc",["--with-js=" ++ intercalate "," jsSources]) : (buildProgramArgs flags)
            }

    progs <- reconfigurePrograms verbosity
                 (buildProgramPaths flags')
                 (buildProgramArgs flags')
                 (withPrograms lbi)
    let lbi' = lbi { withPrograms = progs }

    -- run default build hook
    info verbosity "start build"
    (buildHook simpleUserHooks) pkgDescr lbi' hooks flags'

    withExeLBI pkgDescr lbi $ \exe _ ->
        case lookup "x-worker" . customFieldsBI . buildInfo $ exe of
            Just n -> rawSystemExit verbosity "sed"
                ["-i", "-e", "s/window\\.setTimeout/setTimeout/g", n]
            Nothing -> return ()

-- -------------------------------------------------------------------------- --
-- Utils

whenM :: IO Bool -> (IO ()) -> IO ()
whenM c a = c >>= \x -> when x a

copyFileVerbose' :: Verbosity -> FilePath -> FilePath -> IO ()
copyFileVerbose' verbosity src trg =
    whenM (doesFileExist src) $ copyFileVerbose verbosity src trg

