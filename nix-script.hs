-- | A shebang for running scripts inside nix-shell with defined dependencies
module Main where

import Control.Monad        (when)
import Data.Maybe           (fromMaybe)
import Data.List            (isSuffixOf, isPrefixOf, find)
import System.Environment   (lookupEnv, getProgName, getArgs)
import System.Process       (callProcess)
import System.Posix.IO      (createPipe, fdToHandle)
import System.IO            (hPutStrLn, hClose, hFlush)


-- | Information about a language
data Language = Language
  { name      :: String
    -- ^ Name of the language
  , depsTrans :: [String] -> [String]
    -- ^ Transform language-specific dependencies to nix packages
  , run       :: FilePath -> (String, [String])
    -- ^ Command to run the given file as script
  , repl      :: FilePath -> (String, [String])
    -- ^ Command to load the given file in an interpreter
  }


-- | Basic packages always present
basePackages :: [String]
basePackages = ["coreutils", "utillinux"]

-- | Preserved environment variables
baseEnv :: [String]
baseEnv = ["LOCALE_ARCHIVE", "LANG", "TERMINFO", "TERM"]


-- | List of supported language definitions
languages :: [Language]
languages = [haskell, python, javascript, perl, shell]
  where
    haskell = Language "haskell" d r i where
      d pkgs = pure ("haskellPackages.ghcWithPackages (hs: with hs; [" ++ 
                     unwords pkgs ++ "])")
      r script = ("runghc" , [script])
      i script = ("ghci"   , [script])

    python = Language "python" d r i where
      d pkgs   = "python" : map ("pythonPackages." ++) pkgs
      r script = ("python" , [script])
      i script = ("python" , ["-i", script])

    javascript = Language "javascript" d r i where
      d pkgs   = "node" : map ("nodePackages." ++) pkgs
      r script = ("node" , [script])
      i script = ("node" , [])

    perl = Language "perl" d r i where
      d pkgs   = "perl" : map ("perlPackages." ++) pkgs
      r script = ("perl" , [script])
      i script = ("perl" , ["-d", script])

    shell = Language "shell" d r i where
      d        = mappend ("bash" : basePackages)
      r script = ("bash", [script])
      i _      = ("bash", [])


-- | Create ad-hoc definitions for unknown languages
passthrough :: String -> Language
passthrough name = Language name d r i where
  d        = mappend basePackages
  r script = (name, [script])
  i _      = (name, [])


-- | Find the appropriate language definition
lookupLang :: String -> Language
lookupLang n =
  fromMaybe (passthrough n) (find ((n ==) . name) languages)

-- | Parse dependencies declaration line
parseHeader :: String -> [String]
parseHeader = uncurry trans . split . words
  where
    trans lang = depsTrans (lookupLang lang)
    split (lang : "|" : deps) = (lang, deps)
    split line = error ("Invalid dependency declaration: " ++ unwords line)


-- | Find command to run/load the script
interpreter :: String -> Bool -> String -> (String, [String])
interpreter lang interactive = 
  (if interactive then repl else run) (lookupLang lang)


-- | Create command to add the shell environment
makeCommand :: String -> [String] -> IO String
makeCommand program args = do
  (readFd, writeFd) <- createPipe
  writeH <- fdToHandle writeFd
  hPutStrLn writeH (unlines args)
  hFlush writeH >> hClose writeH

  definitions <- mapM format baseEnv
  return (env definitions ++ xargs readFd ++ program)
  where
    env defs   = "env " ++ unwords defs ++ " "
    xargs fd   = "xargs -a /proc/self/fd/" ++ show fd ++ " -d '\\n' "
    format var = maybe "" (\x -> var ++ "=" ++ x) <$> lookupEnv var


-- | run a script or load it in an interactive interpreter
main :: IO ()
main = do
  progName <- getProgName
  progArgs <- getArgs

  when (null progArgs) (fail $ "usage: " ++ progName ++ " <file>")

  let file    = head progArgs
      shebang = takeWhile (isPrefixOf "#!") . lines
      header  = drop 1  . map (drop 2) . shebang

  script <- readFile file
  case header script of
    (('>' : identifier) : lines) -> do
      let pkgs            = concatMap parseHeader lines
          language        = dropWhile (==' ') identifier
          interactive     = isSuffixOf "i" progName
          (program, args) = interpreter language interactive file

      cmd <- makeCommand program args
      putStrLn $ unwords ("--pure" : "--command" : cmd : "-p" : pkgs)
      callProcess "nix-shell" ("--pure" : "--command" : cmd : "-p" : pkgs)

    _ -> fail "missing or invalid header"