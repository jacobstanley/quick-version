module Main where

import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intercalate)
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
    mapM_ (printPackageExports . G.stringToPackageId) args

------------------------------------------------------------------------

type Name = String
type Type = String

data Package = Package Name [Module]
    deriving (Show, Eq, Ord)

data Module = Module Name [Export]
    deriving (Show, Eq, Ord)

data Export = Export Name Type
    deriving (Show, Eq, Ord)

------------------------------------------------------------------------

printPackageExports :: G.PackageId -> IO ()
printPackageExports pkgId = do
    p <- withGhc (getPackage pkgId)
    putStrLn (showPackage p)

showPackage :: Package -> String
showPackage (Package n ms) = intercalate "\n" $ n : map showModule ms

showModule :: Module -> String
showModule (Module n xs) = intercalate "\n" $ (n ++ "\n") : map showExport xs

showExport :: Export -> String
showExport (Export n t) = indent $ n ++ " :: " ++ t
  where
    indent = unlines . map ("    " ++) . lines

------------------------------------------------------------------------

getPackage :: G.GhcMonad m => G.PackageId -> m Package
getPackage pkgId = do
    mods  <- getPackageModules pkgId
    infos <- mapM moduleExports mods

    return $ Package pkgName $ zipWith mkModule mods infos
  where
    pkgName = G.packageIdString pkgId
    modName = G.moduleNameString . G.moduleName

    mkModule mod exps = Module (modName mod) (map mkExport exps)
    mkExport (nam, typ)  = Export (showUnqual nam) (showQual typ)

    moduleExports :: G.GhcMonad m => G.Module -> m [(G.Name, G.Type)]
    moduleExports mod = do
        minfo <- G.getModuleInfo mod
        case minfo of
            Nothing   -> return []
            Just info -> getModuleExports info

------------------------------------------------------------------------

getPackageModules :: G.GhcMonad m => G.PackageId -> m [G.Module]
getPackageModules pkgId = do
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

getModuleExports :: G.GhcMonad m => G.ModuleInfo -> m [(G.Name, G.Type)]
getModuleExports info = do
    tyThings <- mapM G.lookupName (G.modInfoExports info)

    let names = map (fmap G.getName) tyThings
        types = map (>>= extractType) tyThings

    return $ mapMaybe promoteMaybe $ zip names types
  where
    extractType :: G.TyThing -> Maybe G.Type
    extractType (G.AnId var)      = Just (G.varType var)
    extractType (G.ATyCon tycon)  = Just (G.tyConKind tycon)
    extractType (G.ADataCon dcon) = Just (G.dataConRepType dcon)
    extractType (G.ACoAxiom _)    = Nothing
    extractType (G.AClass _)      = Nothing

------------------------------------------------------------------------

withGhc :: G.Ghc a -> IO a
withGhc ghc =
    G.defaultErrorHandler G.defaultLogAction $
    G.runGhc (Just libdir) $ do
        G.getSessionDynFlags >>= G.setSessionDynFlags
        ghc

showQual :: G.Outputable a => a -> String
showQual = G.showSDoc . G.ppr

showUnqual :: G.Outputable a => a -> String
showUnqual = G.showSDocUnqual . G.ppr

promoteMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
promoteMaybe (Just x, Just y) = Just (x, y)
promoteMaybe (_, _)           = Nothing
