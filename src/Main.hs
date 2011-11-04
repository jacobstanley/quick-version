module Main where

import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Prelude hiding (mod)
import System.Environment (getArgs)

-- GHC API
import qualified DynFlags as G
import qualified DataCon as G
import           GHC.Paths (libdir)
import qualified GHC as G
import qualified Module as G
import qualified Outputable as G
import qualified Packages as G
import qualified TyCon as G
import qualified Var as G

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    mapM_ (showExports . G.stringToPackageId) args

------------------------------------------------------------------------

showExports :: G.PackageId -> IO ()
showExports pkgId = do
    putStrLn pkgName
    putStrLn $ replicate (length pkgName) '='

    xs <- withGhc $ do
        mods  <- getModules pkgId
        infos <- mapM G.getModuleInfo mods
        mapM exports $ catMaybes infos

    mapM_ (putStrLn . G.showSDoc . G.ppr) xs
  where
    pkgName = G.packageIdString pkgId

------------------------------------------------------------------------

withGhc :: G.Ghc a -> IO a
withGhc ghc =
    G.defaultErrorHandler G.defaultLogAction $
    G.runGhc (Just libdir) $ do
        G.getSessionDynFlags >>= G.setSessionDynFlags
        ghc

------------------------------------------------------------------------

getModules :: G.GhcMonad m => G.PackageId -> m [G.Module]
getModules pkgId = do
    flags <- G.getSessionDynFlags
    return $ lookupModules pkgId $ G.pkgState flags

lookupModules :: G.PackageId -> G.PackageState -> [G.Module]
lookupModules pkgId pkgState = mods
  where
    pkgs = G.pkgIdMap pkgState
    pkg  = lookupPackage pkgId pkgs
    mods = map (G.mkModule $ G.packageConfigId pkg) (G.exposedModules pkg)

lookupPackage :: G.PackageId -> G.PackageConfigMap -> G.PackageConfig
lookupPackage pkgId pkgs =
    fromMaybe (error msg) (G.lookupPackage pkgs pkgId)
  where
    msg = "lookupModules: could not find " ++ G.packageIdString pkgId

------------------------------------------------------------------------

exports :: G.GhcMonad m => G.ModuleInfo -> m [(G.Name, G.Type)]
exports info = do
    tyThings <- mapM G.lookupName (G.modInfoExports info)

    let names = map (fmap G.getName) tyThings
        types = map (>>= extractType) tyThings

    return $ mapMaybe promote $ zip names types
  where
    promote :: (Maybe G.Name, Maybe G.Type) -> Maybe (G.Name, G.Type)
    promote (Just n, Just t) = Just (n, t)
    promote (_, _)           = Nothing

    extractType :: G.TyThing -> Maybe G.Type
    extractType (G.AnId var)      = Just (G.varType var)
    extractType (G.ATyCon tycon)  = Just (G.tyConKind tycon)
    extractType (G.ADataCon dcon) = Just (G.dataConRepType dcon)
    extractType (G.ACoAxiom _)    = Nothing
    extractType (G.AClass _)      = Nothing
