import Options.Applicative
import Data.Semigroup ((<>))

import Caskell.Bytes
import Caskell.Tests
import Caskell.Hash
import Caskell.CoreHash
import Caskell.Compile

data Arguments = Arguments
    { inputFiles :: [String]
    }

arguments :: Parser Arguments
arguments = Arguments
    <$> many (argument str (metavar "TARGET..."))

run_caskell :: Arguments -> IO ()
run_caskell (Arguments files) = do
    mapM compile_file files
    return ()

main :: IO ()
main = run_caskell =<< execParser opts
    where
        opts = info (arguments <**> helper)
            (fullDesc <> progDesc "Content-Addressed Haskell tool" <> header "caskell - a tool for content-addressed haskell")
