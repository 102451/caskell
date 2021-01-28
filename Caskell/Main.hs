import Options.Applicative
import Data.Semigroup ((<>))

import Caskell.Compile

data Arguments = Arguments
    { inputFiles :: [String]
    }

arguments :: Parser Arguments
arguments = Arguments
    <$> many (argument str (metavar "TARGET..."))

runCaskell :: Arguments -> IO ()
runCaskell (Arguments files) = do
    mapM compileFile files
    return ()

main :: IO ()
main = runCaskell =<< execParser opts
    where
        opts = info (arguments <**> helper)
            (fullDesc <> progDesc "Content-Addressed Haskell tool" <> header "caskell - a tool for content-addressed haskell")
