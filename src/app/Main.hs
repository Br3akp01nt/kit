module Main where

import qualified Actions.Branch      as Branch
import           Actions.Grep        (PresentationMode (..))
import qualified Actions.Grep        as Grep
import qualified Actions.NewBranch   as NewBranch
import qualified Actions.Shove       as Shove
import           Control.Monad.Logger (runLoggerT)
import qualified Data.String          as S
import qualified Data.Text.IO.Class   as T
import qualified External.Git.Grep    as Grep (CaseSensitivity (..))
import           KitM                 (runKitM)
import           Options.Applicative  (command, execParser, helper, info,
                                       progDesc, strArgument, strOption,
                                       subparser)
import qualified Options.Applicative  as O
import           Text.Regex.PCRE      (Regex, RegexMaker (makeRegex))

type Command = O.ParserInfo (IO ())

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    cmd <- execParser $ parser `info` progDesc "Convenience CLI tool"
    cmd
  where
    parser = helper <*> commands

    commands = subparser $ mconcat [ command "grep" grep
                                   , command "branch" branch
                                   , command "shove" shove
                                   , command "new" new
                                   ]


grep :: Command
grep = (runGrep <$> parser) `info` grepInfo
  where
    runGrep cmdOpts = runKitM (Grep.grep cmdOpts) print

    grepInfo = O.progDesc "Run git grep and show the results"

    parser :: O.Parser Grep.GrepOptions
    parser = O.helper <*> opts

    opts = Grep.GrepOptions <$> pattern
                            <*> fileGlob
                            <*> fileRegex
                            <*> presentationMode
                            <*> includeFunctionContext
                            <*> ignoreCase
                            <*> fileExclusionRegex

    pattern = strArgument $ mconcat
      [ O.metavar "regex"
      , O.help "the git grep pattern"
      ]

    fileGlob = many $ strOption $ mconcat
      [ O.short 'g'
      , O.long "file-glob"
      , O.help "the file glob to match against"
      , O.showDefault
      , O.metavar "glob"
      ]

    fileRegex
      = many
      $ fmap (makeRegex :: String -> Regex)
      $ strOption
      $ mconcat
        [ O.metavar "file regex"
        , O.help $ S.unlines [ "an additional, stronger pattern to match files"
                             , "Use file-glob primarily for performance"
                             ]
        , O.short 'r'
        , O.long "file-regex"
        ]

    fileExclusionRegex
      = many
      $ fmap (makeRegex :: String -> Regex)
      $ strOption
      $ mconcat
        [ O.metavar "file exclusion regex"
        , O.help "Exclude results by file with a regex"
        , O.short 'e'
        , O.long "exclude"
        ]

    ignoreCase
      = O.flag Grep.Sensitive Grep.Insensitive
      $ mconcat
        [ O.help "Ignore case"
        , O.short 'i'
        , O.long "ignore-case"
        ]

    presentationMode = listPresentation <|> treePresentation


    includeFunctionContext = O.switch $ mconcat
      [ O.short 'W'
      , O.long "function-context"
      , O.help $  "include the context of the surrounding "
               <> "function for each search result"
      ]

    treePresentation = O.flag TreeMode TreeMode $ mconcat
      [ O.long "tree"
      , O.help "Show the results as a tree"
      ]

    listPresentation = O.flag' ListMode $ mconcat
      [ O.long "list"
      , O.help "Show the results as a list"
      ]


branch :: Command
branch = (runBranch <$> parser) `info` O.progDesc "git branch actions"
  where
    runBranch cmdOpts = runKitM (runLoggerT stdOutLog $ Branch.branch cmdOpts)
                                print

    parser :: O.Parser Branch.BranchOptions
    parser = O.helper <*> opts

    opts = Branch.BranchOptions <$> searchPattern
                                <*> includeRemotes

    searchPattern
      = O.optional
      $ fmap (makeRegex :: String -> Regex)
      $ O.strArgument
      $ mconcat [ O.metavar "REGEX"
                , O.help "Regex pattern to filter branches"
                ]

    includeRemotes = O.flag False True $ mconcat
      [ O.short 'a'
      , O.long "include-remotes"
      ]


shove :: Command
shove = (runShove <$> parser) `info` O.progDesc "git commit wrapper"
  where
    runShove cmdOpts = runKitM (Shove.shove cmdOpts) print

    parser :: O.Parser Shove.ShoveOptions
    parser = O.helper <*> opts

    opts = Shove.ShoveOptions <$> message
                              <*> autoPush
                              <*> specific

    message = O.optional $ O.strArgument $ mconcat
      [ O.metavar "message"
      , O.help "Commit message"
      ]

    autoPush = O.flag False True $ mconcat
      [ O.short 'p'
      , O.long "push"
      ]

    specific = O.flag False True $ mconcat
      [ O.short 's'
      , O.long "specific"
      ]


new :: Command
new = (runNew <$> parser) `info` O.progDesc "create git branch"
  where
    runNew cmdOpts = runKitM (NewBranch.newBranch cmdOpts) print

    parser :: O.Parser NewBranch.NewBranchOptions
    parser = O.helper <*> pure NewBranch.NewBranchOptions


stdOutLog :: MonadIO m => Text -> m ()
stdOutLog = T.putStrLn

