#!/usr/bin/env nix-script
#!> haskell
#! haskell | text lens
#! shell   | nix
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import Control.Monad
import Control.Applicative
import System.Environment
import Data.List
import Data.Text (Text)
import Data.Text.Lens (_Text)
import Control.Lens
import Control.Exception.Lens
import System.IO.Error.Lens
import System.Exit
import System.Posix.Process
import System.Posix.IO
import System.IO
import Data.Char
import Data.Monoid

import qualified Data.Text as Text

-- | Information about a languages
data LangDef = LangDef
  { name :: String                         -- ^ Name of this language
  , deps :: [Text]   -> [Text]             -- ^ Convert langunage-specific dependencies to nix packages
  , run  :: FilePath -> (String, [String]) -- ^ Command to run the given file as script
  , repl :: FilePath -> (String, [String]) -- ^ Command to load the given file in an interpreter
  }

languages :: [LangDef]
languages = [haskell, python, javascript, perl, shell]

haskell :: LangDef
haskell = LangDef "haskell" d r i where
  d pkgs = return $
    "haskellPackages.ghcWithPackages (hs: with hs; [" <> Text.unwords pkgs <> "])"
  r script = ("runhaskell" , [script])
  i script = ("ghci"       , [script])

python :: LangDef
python = LangDef "python" d r i where
  d pkgs = "python" : map ("pythonPackages." <>) pkgs
  r script = ("python" , [script])
  i script = ("python" , ["-i", script])

javascript :: LangDef
javascript = LangDef "javascript" d r i where
  d pkgs = "node" : map ("nodePackages." <>) pkgs
  r script = ("node" , [script])
  i script = ("node" , [])

perl :: LangDef
perl = LangDef "perl" d r i where
  d pkgs = "perl" : map ("perlPackages." <>) pkgs
  r script = ("perl" , [script])
  i script = ("perl" , ["-d", script])

shell :: LangDef
shell = LangDef "shell" (extraPackages ++) r i where
  r script = ("bash", [script])
  i _      = ("bash", [])
  extraPackages = ["bash", "coreutils", "utillinux", "gitAndTools.hub", "git"]

lookupLangDef :: String -> IO LangDef
lookupLangDef n
  | Just def <- find ((n ==) . name) languages = return def
  | otherwise = fail $ "Unknown language: " ++ n

makeDeps :: String -> [String] -> IO [String]
makeDeps lang ds = lookupLangDef lang <&> \def ->
  map (view _Text) $ deps def (map (review _Text) ds)

parseDepLine :: [String] -> IO (String, [String])
parseDepLine (lang:"|":deps) = return (lang, deps)
parseDepLine x = fail $ "Invalid dependency specification: " ++ unwords x

makeCommand :: String -> Bool -> String -> IO (String, [String])
makeCommand lang interactive file = lookupLangDef lang <&> \def ->
  (if interactive then repl else run) def file

makeEnvArg :: String -> IO String
makeEnvArg env = f $ getEnv env <&> \val -> env ++ "=" ++ val where
  f = handling_ (_IOException.errorType._NoSuchThing) $ return ""

makeXargsCommand :: String -> Int -> IO String
makeXargsCommand cmd fd = do
  let xargsFile = "/proc/self/fd/" ++ show fd
  envStr <- unwords <$> traverse makeEnvArg
    ["LOCALE_ARCHIVE", "LANG", "TERMINFO", "TERM"]
  return $ "env " ++ envStr ++ " xargs -a " ++ xargsFile ++ " -d '\\n' " ++ cmd ++ ""

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  let interactive = "i" `isSuffixOf` progName
  case args ^? _Cons of
    Nothing -> fail $ "usage: " ++ progName ++ " <file>" ++ "   [missing file name]"
    Just (file, args') -> do
      header <- drop 1 . map (drop 2) . takeWhile ("#!" `isPrefixOf`) . lines <$> readFile file
      case header ^? _Cons of
        Just ('>':lang, depHeader) -> do
          deps   <- concat <$> traverse (uncurry makeDeps <=< parseDepLine . words) depHeader
          let deps' = "findutils" : deps
          let depArgs = concatMap (\x -> ["-p", x]) deps'
          (cmd,cmdArgs) <- makeCommand (under _Text Text.strip lang) interactive file
          (readFd, writeFd) <- createPipe
          writeH <- fdToHandle writeFd
          hPutStrLn writeH (unlines cmdArgs) >> hFlush writeH
          hClose writeH
          xargsCmd <- makeXargsCommand cmd (fromIntegral readFd)
          let finalArgs = "--pure" : "--command" : xargsCmd : depArgs
          executeFile "nix-shell" True finalArgs Nothing
        _ -> fail "missing language to run as"
