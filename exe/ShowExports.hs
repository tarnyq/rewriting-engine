{-# LANGUAGE MultilineStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.List (intercalate)
import Language.Haskell.Exts
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


putErr :: String -> IO ()
putErr = hPutStrLn stderr

main :: IO ()
main = do
    args <- getArgs
    src <- case args of
                [filename] -> readFile filename
                _          -> do putErr "XXX BAD ARGS"
                                 exitFailure
    --  'let ast = fromParseResult $ parseModule src' is easier, but the
    --  exception output is not as nice to read as what we will eventually do.
--  let src = sampleModule
    ast <- case parseModule src of
                ParseOk ast         -> return ast
                ParseFailed loc str -> parseError loc str
    let (name, explist) = exports ast
    putStrLn $ name ++ ": " ++ (intercalate ", " explist)

    where
        parseError loc str = do
            putErr "Parse failed!"
            putErr $ "Location: " ++ (show loc)
            putErr $ "Error: " ++ str
            exitFailure


sampleModule :: String
sampleModule = """
    module Foo (bar, baz) where
    bar = 123
    baz = "fourfivesix"
"""

--  Give the module name and the list of exported names.
--  (If you want to experiment, adding a 'Show l =>' constraint will
--  let you more easily display the things this is taking apart.)
exports :: Module l -> (String, [String])
exports (Module _ Nothing            _       _        _) =
    error "Module without a head?!"
exports (Module _ (Just moduleHead) _pragmas _imports _decls) =
    let ModuleHead _ (ModuleName _ name) _ maybeExports = moduleHead in
    case maybeExports of
        Just (ExportSpecList _ specs) -> (name, exportNames specs)
        Nothing                       -> (name, [])
    where
        exportNames :: [ExportSpec l] -> [String]
        exportNames specs = concatMap exportToString specs
exports XmlPage{}   = error "What's an XmlPage module"
exports XmlHybrid{} = error "What's an XmlPage module"
{-
    case head of
        Nothing -> []   -- Actually means all top level defs exported.
        Just (ExportSpecList _ exports) -> concatMap exportToString exports
-}

exportToString :: ExportSpec l -> [String]
exportToString spec = case spec of
    EVar _ qname                        -> [prettyPrint qname]
    EAbs _ _namespace qname             -> [prettyPrint qname]
    EThingWith _ _wildcard qname cnames ->
        prettyPrint qname : map (prettyPrint . unCName) cnames
    EModuleContents _ modname           -> ["module " ++ prettyPrint modname]
    where
        unCName (VarName _ name) = name
        unCName (ConName _ name) = name
