import Data.List          (intersect)
import Data.Maybe         (mapMaybe)
import Control.Monad      (liftM)
import System.Environment (getArgs)
import System.Exit        (ExitCode(..))
import System.Directory   (listDirectory)
import System.FilePath    (addExtension, stripExtension, (</>))
import System.Process     (readProcessWithExitCode)

main :: IO ()
main = getArgs >>= return . head >>= listUnrelativeDir >>= return . findTests >>= mapM_ runTest

-- | Gegeven een mapnaam, lijst alle bestanden op die zich in deze map
-- bevinden. De mapnaam wordt voorgevoegd aan de opgelijste bestanden.
listUnrelativeDir :: FilePath -> IO [FilePath]
listUnrelativeDir d = map (d </>) <$> listDirectory d

-- | Gegeven een mapnaam en de bestanden die zich in deze map bevinden, geeft
-- `findTests` de gevonden testen terug.
findTests :: [FilePath] -> [(String, FilePath)]
findTests listing = map replaceExts $ withExt "txt"
  where
    withExt ext = mapMaybe (stripExtension ext) listing
    replaceExts name = (name , addExtension name "txt")

-- | Voert een test uit en schrijft het resultaat naar stdout.
runTest :: (String, FilePath) -> IO ()
runTest (name, testFile) = do
    putStr $ "running test " ++ name ++ "...\t"
    result <- readProcessWithExitCode "stack" ["build", "--exec", "cogs --test " ++ testFile] ""
    case result of
        (ExitFailure i,   _ , _) -> putStrLn $ "failed; non-zero exit code (" ++ show i ++ ")  :-(  "
        (ExitSuccess, stdout, _) -> putStr stdout
