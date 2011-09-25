module Main (main) where

import qualified DynFlags as G
import           Data.Maybe (catMaybes)
import qualified DataCon as G
import qualified GHC as G
import           GHC.Paths (libdir)
import qualified Outputable as G
import qualified TyCon as G
import qualified Var as G

main :: IO ()
main = do
   res <- example
   mapM_ (putStrLn . G.showSDoc . G.ppr) res

example :: IO [G.Type]
example =
    G.defaultErrorHandler G.defaultLogAction $ do
    G.runGhc (Just libdir) $ do
        dflags <- G.getSessionDynFlags
        G.setSessionDynFlags dflags
        target <- G.guessTarget "examples/A.hs" Nothing
        G.setTargets [target]
        G.load G.LoadAllTargets

        m <- G.findModule (G.mkModuleName "A") Nothing
        (Just info) <- G.getModuleInfo m

        exports <- mapM (G.lookupName) (G.modInfoExports info)

        return $ map extractType $ catMaybes exports

extractType :: G.TyThing -> G.Type
extractType (G.AnId var)      = G.varType var
extractType (G.ATyCon tycon)  = G.tyConKind tycon
extractType (G.ADataCon dcon) = G.dataConRepType dcon
extractType (G.ACoAxiom _)    = error "extractType: ACoAxiom"
extractType (G.AClass _)      = error "extractType: AClass"
