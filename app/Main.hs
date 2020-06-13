module Main where

import qualified Build                        as Build
import qualified Options.Applicative          as Opts
import           System.Environment           (getArgs)
import           System.IO                    (IO)
import qualified System.IO                    as IO
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Version                      (versionString)


main :: IO ()
main = do
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8

  cmd <- Opts.handleParseResult . execParserPure opts =<< getArgs
  cmd

  where
    opts = Opts.info (versionInfo <*> optParser) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo = Opts.progDesc "PureScript PHP backend (purehp)"
    footerInfo = Opts.footerDoc (Just footer)

    footer =
      mconcat
      [ para $
         "For help using each individual command, run `purehp COMMAND --help`. " ++
         "For example, `purehp compile --help` displays options specific to the `compile` command."
      , Doc.hardline
      , Doc.hardline
      , Doc.text $ "purehp " ++ versionString
      ]

    para :: String -> Doc.Doc
    para = foldr (Doc.</>) Doc.empty . map Doc.text . words

    execParserPure :: Opts.ParserInfo a -> [String] -> Opts.ParserResult a
    execParserPure pinfo args = Opts.execParserPure Opts.defaultPrefs pinfo args

    versionInfo :: Opts.Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg versionString) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    optParser :: Opts.Parser (IO ())
    optParser = Build.parser
