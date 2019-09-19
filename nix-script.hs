-- | A shebang for running scripts inside nix-shell with defined dependencies
module Main where

import Control.Monad                (when)
import Data.Maybe                   (fromMaybe)
import Data.Char                    (isSpace)
import Data.List                    (isPrefixOf, find, (\\))
import System.Environment           (lookupEnv, getProgName, getArgs)
import System.Process               (callProcess)
import System.Posix.Escape.Unicode  (escapeMany)

-- | Enviroment variables
type Env   = [String]

-- | Program arguments
type Args  = [String]

-- | interpreter name and arguments
type Inter = (String, Args)


-- | Information about a language
data Language = Language
  { name      :: String
    -- ^ Name of the language
  , depsTrans :: [String] -> [String]
    -- ^ Transform language-specific dependencies to nix packages
  , run       :: FilePath -> Inter
    -- ^ Command to run the given file as script
  , repl      :: FilePath -> Inter
    -- ^ Command to load the given file in an interpreter
  }


-- | Basic packages always present
basePackages :: [String]
basePackages = ["coreutils", "utillinux"]

-- | Preserved environment variables
baseEnv :: [String]
baseEnv = ["LOCALE_ARCHIVE", "SSL_CERT_FILE" ,"LANG", "TERMINFO", "TERM"]


-- | List of supported language definitions
languages :: [Language]
languages = [haskell, python 2, python 3, javascript, perl, shell]
  where
    haskell = Language "haskell" d r i where
      d pkgs = pure ("haskellPackages.ghcWithPackages (hs: with hs; [" ++ 
                     unwords pkgs ++ "])")
      r script = ("runghc" , [script])
      i script = ("ghci"   , [script])

    python v = Language ("python" ++ show v) d r i where
      d pkgs   = pure ("python" ++ (show v) ++
                       ".withPackages (py: with py; [" ++
                       unwords pkgs ++ "])")
      r script = ("python" ++ show v, [script])
      i script = ("python" ++ show v, ["-i", script])

    javascript = Language "javascript" d r i where
      d pkgs   = "node" : map ("nodePackages." ++) pkgs
      r script = ("node" , [script])
      i script = ("node" , [])

    perl = Language "perl" d r i where
      d pkgs   = pure ("perl.withPackages (pl: with pl; [" ++
                       unwords pkgs ++ "])")
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


-- | Extract environment declaration from the header
filterEnv :: [String] -> (Env, [String])
filterEnv header = (vars env, header \\ env)
  where
    vars = concatMap (drop 2 . words)
    env  = filter (isPrefixOf "env" . dropWhile isSpace) header


-- | Parse dependencies declaration line
parseHeader :: String -> [String]
parseHeader = uncurry trans . split . words
  where
    trans lang = depsTrans (lookupLang lang)
    split (lang : "|" : deps) = (lang, deps)
    split line = error ("Invalid dependency declaration: " ++ unwords line)


-- | Find command to run/load the script
makeInter :: String -> Bool -> String -> Inter
makeInter lang interactive = 
  (if interactive then repl else run) (lookupLang lang)


-- | Create command to add the shell environment
makeCmd :: Inter -> Args -> Env -> String
makeCmd (program, args) args' defs =
  env defs ++ interpreter ++ escapeMany args'
  where
    interpreter = program ++ " " ++ unwords args ++ " "
    env defs   = "env " ++ unwords defs ++ " "


-- | Create environment variable to run the script with
makeEnv :: Env -> IO Env
makeEnv extra = mapM format (baseEnv ++ extra) where
  format var = maybe "" (\x -> var ++ "=" ++ x) <$> lookupEnv var


-- | run a script or load it in an interactive interpreter
main :: IO ()
main = do
  progName <- getProgName
  progArgs <- getArgs

  when (null progArgs) (fail $ "usage: " ++ progName ++ " <file>")

  let shebang     = takeWhile (isPrefixOf "#!") . lines
      header      = drop 1  . map (drop 2) . shebang
      (file:args) = progArgs

  script <- readFile file
  case header script of
    (('>':identifier) : lines) -> do
      let (env, deps) = filterEnv lines
          pkgs        = concatMap parseHeader deps
          language    = dropWhile isSpace identifier
          interactive = last progName == 'i'
          interpreter = makeInter language interactive file

      cmd <- makeCmd interpreter args <$> makeEnv env
      callProcess "nix-shell" ("--pure" : "--command" : cmd : "-p" : pkgs)

    _ -> fail "missing or invalid header"