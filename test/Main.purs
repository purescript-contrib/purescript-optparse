module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Gen (class MonadGen, chooseFloat, chooseInt, suchThat)
import Control.Monad.Gen.Common (genMaybe)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array (fold, foldl, replicate)
import Data.Array as Array
import Data.Char.Gen (genUnicodeChar)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Int (even, odd)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Gen (genString)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (tuple3, (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Examples.Alternatives as Alternatives
import Examples.Cabal as Cabal
import Examples.Commands as Commands
import Examples.Formatting as Formatting
import Examples.Hello as Hello
import ExitCodes (ExitCode)
import ExitCodes as ExitCode
import Options.Applicative (argument, briefDesc, columns, command, completeWith, defaultPrefs, disambiguate, execParserPure, flag', forwardOptions, header, help, (<**>), helper, hidden, info, int, long, metavar, noBacktrack, noIntersperse, option, prefs, progDesc, renderFailure, short, showDefault, showHelpOnEmpty, showHelpOnError, str, strArgument, strOption, subparser, subparserInline, switch, value)
import Options.Applicative.Help (Chunk(..), editDistance, extractChunk, isEmpty, listToChunk, paragraph, stringChunk)
import Options.Applicative.Internal.Utils (lines, words)
import Options.Applicative.Types (CompletionResult(..), ParserFailure, ParserHelp, ParserInfo, ParserPrefs, ParserResult(..), ReadM, many, readerAsk, readerError, some, optional)
import Test.QuickCheck ((<?>), (===))
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner as Spec
import Text.PrettyPrint.Leijen (Doc)
import Text.PrettyPrint.Leijen as Doc

isOK :: QC.Result -> Aff Unit
isOK QC.Success = pure unit
isOK (QC.Failed err) = fail $ "notOK: " <>err

run :: forall a . ParserInfo a -> Array String -> ParserResult a
run = execParserPure defaultPrefs

counterExample :: String -> QC.Result
counterExample str = false <?> str

conjoin :: Array QC.Result -> QC.Result
conjoin = flip foldl QC.Success case _, _ of
  QC.Success, QC.Success -> QC.Success
  QC.Success, a -> a
  QC.Failed str, QC.Success -> QC.Failed str
  QC.Failed str, QC.Failed str' -> QC.Failed $ str <> ";\n" <> str'

assertError :: forall a. Show a => ParserResult a -> (ParserFailure ParserHelp -> QC.Result) -> QC.Result
assertError x f = case x of
  Success r -> counterExample ("expected failure, got success: " <> show r)
  Failure e -> f e
  CompletionInvoked _ -> counterExample "expected failure, got completion"

assertResult :: forall a. ParserResult a -> (a -> QC.Result) -> QC.Result
assertResult x f = case x of
  Success r -> f r
  Failure e -> do
    let (Tuple msg _) = renderFailure e "test"
    counterExample ("unexpected parse error\n" <> msg)
  CompletionInvoked _ -> counterExample "expected result, got completion"


assertCompletion :: forall m a. MonadEffect m => Show a => ParserResult a -> (Array String -> QC.Result) -> m QC.Result
assertCompletion x f = case x of
  CompletionInvoked (CompletionResult {execCompletion}) -> do
    completions <- liftEffect $ lines <$> execCompletion "test"
    pure $ f completions
  Failure _   -> pure $ counterExample "unexpected failure"
  Success val -> pure $ counterExample ("unexpected result " <> show val)

assertHasLine :: String -> String -> QC.Result
assertHasLine l s = l `elem` lines s <?> ("expected line:\n\t" <> l <> "\nnot found")

isInfixOf :: Pattern -> String -> Boolean
isInfixOf p = String.indexOf p >>> isJust

condr :: (Int -> Boolean) -> ReadM Int
condr f = do
  x <- int
  if (f x) then
    pure x
  else
    readerError "oh no"

type Asset = { name :: String, msg :: String }

mkAsset :: String -> String -> Asset
mkAsset name msg = {name, msg: stripNl String.stripPrefix $ stripNl String.stripSuffix $ msg }
  where
  stripNl f s = fromMaybe s $ f (String.Pattern "\n") s

asset_alt :: Asset
asset_alt = mkAsset "alt" """
Usage: alt (--virtual-machine VM | --cloud-service CS | --dry-run)

Available options:
  --virtual-machine VM     Virtual machine name
  --cloud-service CS       Cloud service name
  -h,--help                Show this help text
"""

asset_cabal :: Asset
asset_cabal = mkAsset "cabal" """
Usage: cabal configure [--enable-tests] [-f|--flags FLAGS]
  Prepare to build the package

Available options:
  --enable-tests           Enable compilation of test suites
  -f,--flags FLAGS         Enable the given flag
  -h,--help                Show this help text
"""


asset_carry :: Asset
asset_carry = mkAsset "carry" """
Missing: -a A

Usage: carry c -a A
"""

asset_commands_header_full :: Asset
asset_commands_header_full = mkAsset "commands_header_full" """
Invalid option `-zello'

Did you mean this?
    hello

foo

Usage: commands_header_full COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  hello                    Print greeting
  goodbye                  Say goodbye

French commands:
  bonjour                  Print greeting
  au-revoir                Say goodbye
"""

asset_commands_header :: Asset
asset_commands_header = mkAsset "commands_header" """
Invalid option `-zello'

Did you mean this?
    hello

Usage: commands_header COMMAND
"""


asset_commands :: Asset
asset_commands = mkAsset "commands" """
Usage: commands COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  hello                    Print greeting
  goodbye                  Say goodbye

French commands:
  bonjour                  Print greeting
  au-revoir                Say goodbye
"""

asset_dropback :: Asset
asset_dropback = mkAsset "dropback" """
Missing: C

Usage: dropback B C
"""

asset_formatting :: Asset
asset_formatting = mkAsset "formatting" """
Usage: formatting [-t|--test FOO_BAR_BAZ_LONG_METAVARIABLE]
  This is a very long program description. This
  text should be automatically wrapped to fit the
  size of the terminal

Available options:
  -t,--test FOO_BAR_BAZ_LONG_METAVARIABLE
                           This is an options with
                           a very very long
                           description. Hopefully,
                           this will be nicely
                           formatted by the help
                           text generator.
  -h,--help                Show this help text
"""

asset_hello :: Asset
asset_hello = mkAsset "hello" """
hello - a test for optparse-applicative

Usage: hello --hello TARGET [-q|--quiet] [--repeat INT]
  Print a greeting for TARGET

Available options:
  --hello TARGET           Target for the greeting
  -q,--quiet               Whether to be quiet
  --repeat INT             Repeats for greeting (default: 1)
  -h,--help                Show this help text
"""

asset_helponempty :: Asset
asset_helponempty = mkAsset "helponempty" """
Usage: helponempty B C

Available options:
  -h,--help                Show this help text

Available commands:
  b                        
  c                        
"""

asset_helponemptysub :: Asset
asset_helponemptysub = mkAsset "helponemptysub" """
Usage: helponemptysub c -a A

Available options:
  -a A                     both commands require this
"""

asset_nested :: Asset
asset_nested = mkAsset "nested" """
Missing: -a A

Usage: nested c b -a A
"""


asset_subparsers :: Asset
asset_subparsers = mkAsset "subparsers" """
Usage: subparsers COMMAND COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  add                      Add a file to the repository
  commit                   Record changes to the repository
"""


checkHelpTextWith
  :: forall a
   . Show a
  => ExitCode
  -> ParserPrefs
  -> Asset
  -> ParserInfo a
  -> Array String
  -> QC.Result
checkHelpTextWith ecode pprefs asset p args =
  let result = execParserPure pprefs p args
  in assertError result $ \failure -> Tuple asset.msg ecode === renderFailure failure asset.name

checkHelpText :: forall a. Show a => Asset -> ParserInfo a -> Array String -> QC.Result
checkHelpText = checkHelpTextWith ExitCode.Success defaultPrefs

spec :: Spec Unit
spec = describe "optparse" $ do

  it "prop_hello" $ defer \_ -> do
    isOK $ checkHelpText asset_hello Hello.opts ["--help"]

  it "prop_modes" $ defer \_ -> do
    isOK $ checkHelpText asset_commands Commands.opts ["--help"]

  it "prop_cmd_header" $ defer \_ -> do
    let i = info (helper <*> Commands.sample) (header "foo")
    isOK $ conjoin
      [ checkHelpTextWith ExitCode.Error defaultPrefs asset_commands_header i ["-zello"]
      , checkHelpTextWith ExitCode.Error (prefs showHelpOnError) asset_commands_header_full i ["-zello"]
      ]


  it "prop_cabal_conf" $ defer \_ -> do
    isOK $ checkHelpText asset_cabal Cabal.pinfo ["configure", "--help"]


  it "prop_args" $ defer \_ -> do
    let result = run Commands.opts ["hello", "foo", "bar"]
    isOK $ assertResult result (_ === (Commands.Hello ["foo", "bar"]))


  it "prop_args_opts" $ defer \_ -> do
    let result = run Commands.opts ["hello", "foo", "--bar"]
    isOK $ assertError result $ const QC.Success


  it "prop_args_ddash" $ defer \_ -> do
    let result = run Commands.opts ["hello", "foo", "--", "--bar", "--", "baz"]
    isOK $ assertResult result (_ === (Commands.Hello ["foo", "--bar", "--", "baz"]))


  it "prop_alts" $ defer \_ -> do
    let result = run Alternatives.opts ["-b", "-a", "-b", "-a", "-a", "-b"]
    isOK $ assertResult result $ \xs ->
      let a = Alternatives.A
          b = Alternatives.B
      in  [b, a, b, a, a, b] === xs


  it "prop_show_default" $ defer \_ -> do
    let p = option int
            ( short 'n'
            <> help "set count"
            <> value 0
            <> showDefault )
        i = info (p <**> helper) mempty
        result = run i ["--help"]
    isOK $ assertError result $ \failure ->
      let (Tuple msg _) = renderFailure failure "test"
      in  assertHasLine
          "  -n ARG                   set count (default: 0)"
          msg


  it "prop_alt_cont" $ defer \_ -> do
    let p = Alternatives.a <|> Alternatives.b
        i = info p mempty
        result = run i ["-a", "-b"]
    isOK $ assertError result $ const QC.Success


  it "prop_alt_help" $ defer \_ -> do
    let p = p1 <|> p2 <|> p3
        p1 = (Just <<< Left)
          <$> strOption ( long "virtual-machine"
                      <> metavar "VM"
                      <> help "Virtual machine name" )
        p2 = (Just <<< Right)
          <$> strOption ( long "cloud-service"
                      <> metavar "CS"
                      <> help "Cloud service name" )
        p3 = flag' Nothing ( long "dry-run" )
        i = info (p <**> helper) mempty
    isOK $ checkHelpText asset_alt i ["--help"]


  it "prop_nested_commands" $ defer \_ -> do
    let p3 = strOption (short 'a' <> metavar "A")
        p2 = subparser (command "b" (info p3 mempty))
        p1 = subparser (command "c" (info p2 mempty))
        i = info (p1 <**> helper) mempty
    isOK $ checkHelpTextWith ExitCode.Error defaultPrefs asset_nested i ["c", "b"]


  it "prop_drops_back_contexts" $ defer \_ -> do
    let p3 = strOption (short 'a' <> metavar "A")
        p2 = subparser (command "b" (info p3 mempty)  <> metavar "B")
        p1 = subparser (command "c" (info p3 mempty)  <> metavar "C")
        p0 = Tuple <$> p2 <*> p1
        i = info (p0 <**> helper) mempty
    isOK $ checkHelpTextWith ExitCode.Error defaultPrefs asset_dropback i ["b", "-aA"]


  it "prop_context_carry" $ defer \_ -> do
    let p3 = strOption (short 'a' <> metavar "A")
        p2 = subparser (command "b" (info p3 mempty)  <> metavar "B")
        p1 = subparser (command "c" (info p3 mempty)  <> metavar "C")
        p0 = Tuple <$> p2 <*> p1
        i = info (p0 <**> helper) mempty
    isOK $ checkHelpTextWith ExitCode.Error defaultPrefs asset_carry i ["b", "-aA", "c"]


  it "prop_help_on_empty" $ defer \_ -> do
    let p3 = strOption (short 'a' <> metavar "A")
        p2 = subparser (command "b" (info p3 mempty)  <> metavar "B")
        p1 = subparser (command "c" (info p3 mempty)  <> metavar "C")
        p0 = Tuple <$> p2 <*> p1
        i = info (p0 <**> helper) mempty
    isOK $ checkHelpTextWith ExitCode.Error (prefs showHelpOnEmpty) asset_helponempty i []


  it "prop_help_on_empty_sub" $ defer \_ -> do
    let p3 = strOption (short 'a' <> metavar "A" <> help "both commands require this")
        p2 = subparser (command "b" (info p3 mempty)  <> metavar "B")
        p1 = subparser (command "c" (info p3 mempty)  <> metavar "C")
        p0 = Tuple <$> p2 <*> p1
        i = info (p0 <**> helper) mempty
    isOK $ checkHelpTextWith ExitCode.Error (prefs showHelpOnEmpty) asset_helponemptysub i ["b", "-aA", "c"]

  it "prop_many_args" $ defer \_ -> do
    quickCheckGen do
      nargs <- chooseInt 0 20 -- TODO if we use bigger upper range stack will explode
      let p = many (argument str mempty)
          i = info p mempty
          result = run i (replicate nargs "foo")
      pure $ assertResult result (\xs -> nargs === List.length xs)


  it "prop_disambiguate" $ defer \_ -> do
    let p =   flag' 1 (long "foo")
          <|> flag' 2 (long "bar")
          <|> flag' 3 (long "baz")
        i = info p mempty
        result = execParserPure (prefs disambiguate) i ["--f"]
    isOK $ assertResult result (_ === 1)


  it "prop_ambiguous" $ defer \_ -> do
    let p =   flag' 1 (long "foo")
          <|> flag' 2 (long "bar")
          <|> flag' 3 (long "baz")
        i = info p mempty
        result = execParserPure (prefs disambiguate) i ["--ba"]
    isOK $ assertError result $ const QC.Success

  it "prop_completion" $ defer \_ -> do
    let p = Tuple
          <$> strOption (long "foo" <> value "")
          <*> strOption (long "bar" <> value "")
        i = info p mempty
        result = run i ["--bash-completion-index", "0"]
    isOK =<< assertCompletion result (["--foo", "--bar"] === _)

  it "prop_completion_opt_after_double_dash" $ defer \_ -> do
    let p = Tuple
          <$> strOption (long "foo" <> value "")
          <*> argument readerAsk (completeWith ["bar"])
        i = info p mempty
        result = run i ["--bash-completion-index", "2"
                      , "--bash-completion-word", "test"
                      , "--bash-completion-word", "--"]
    isOK =<< assertCompletion result (["bar"] === _)

  it "prop_completion_only_reachable" $ defer \_ -> do
    let p = Tuple
          <$> strArgument (completeWith ["reachable"])
          <*> strArgument (completeWith ["unreachable"])
        i = info p mempty
        result = run i ["--bash-completion-index", "0"]
    isOK =<< assertCompletion result (["reachable"] === _)

  it "prop_completion_only_reachable_deep" $ defer \_ -> do
    let p = Tuple
          <$> strArgument (completeWith ["seen"])
          <*> strArgument (completeWith ["now-reachable"])
        i = info p mempty
        result = run i [ "--bash-completion-index", "2"
                      , "--bash-completion-word", "test-prog"
                      , "--bash-completion-word", "seen" ]
    isOK =<< assertCompletion result (["now-reachable"] === _)

  it "prop_completion_multi" $ defer \_ -> do
    let p = many (strArgument (completeWith ["reachable"]))
        i = info p mempty
        result = run i [ "--bash-completion-index", "3"
                      , "--bash-completion-word", "test-prog"
                      , "--bash-completion-word", "nope" ]
    isOK =<< assertCompletion result (["reachable"] === _)

  it "prop_completion_rich" $ defer \_ -> do
    let p = Tuple
          <$> option readerAsk (long "foo" <> help "Fo?")
          <*> option readerAsk (long "bar" <> help "Ba?")
        i = info p mempty
        result = run i ["--bash-completion-enriched", "--bash-completion-index", "0"]
    isOK =<< assertCompletion result (["--foo\tFo?", "--bar\tBa?"] === _)

  it "prop_completion_rich_lengths" $ defer \_ -> do
    let p = Tuple
          <$> option readerAsk (long "foo" <> help "Foo hide this")
          <*> option readerAsk (long "bar" <> help "Bar hide this")
        i = info p mempty
        result = run i [ "--bash-completion-enriched"
                      , "--bash-completion-index=0"
                      , "--bash-completion-option-desc-length=3"
                      , "--bash-completion-command-desc-length=30"]
    isOK =<< assertCompletion result (["--foo\tFoo...", "--bar\tBar..."] === _)



  it "prop_bind_usage" $ defer \_ -> do
    let p = Array.fromFoldable <$> many (argument str (metavar "ARGS..."))
        i = info (p <**> helper) briefDesc
        result = run i ["--help"]
    isOK $ assertError result $ \failure ->
      let text = Array.head $ lines $ fst $ renderFailure failure "test"
      in  Just "Usage: test [ARGS...]" === text


  it "prop_issue_19" $ defer \_ -> do
    let p = option (map Just str) ( short 'x' <> value Nothing )
        i = info (p <**> helper) mempty
        result = run i ["-x", "foo"]
    isOK $ assertResult result (Just "foo" === _)


  it "prop_arguments1_none" $ defer \_ -> do
    let p = Array.fromFoldable <$> some (argument str mempty)
        i = info (p <**> helper) mempty
        result = run i []
    isOK $ assertError result $ const QC.Success


  it "prop_arguments1_some" $ defer \_ -> do
    let p = Array.fromFoldable <$> some (argument str mempty)
        i = info (p <**> helper) mempty
        result = run i ["foo", "--", "bar", "baz"]
    isOK $ assertResult result (["foo", "bar", "baz"] === _)


  it "prop_arguments_switch" $ defer \_ -> do
    let p = map Array.fromFoldable
          $ switch (short 'x')
          *> many (argument str mempty)
        i = info p mempty
        result = run i ["--", "-x"]
    isOK $ assertResult result $ \args -> ["-x"] === args


  it "prop_issue_35" $ defer \_ -> do
    let p =  flag' true (short 't' <> hidden) <|> flag' false (short 'f')
        i = info p mempty
        result = run i []
    isOK $ assertError result $ \failure ->
      let text = lines <<< fst $ renderFailure failure "test"
      in  ["Missing: -f", "", "Usage: test -f"] === text


  it "prop_backtracking" $ defer \_ -> do
    let p2 = switch (short 'a')
        p1 = Tuple
          <$> subparser (command "c" (info p2 mempty))
          <*> switch (short 'b')
        i = info (p1 <**> helper) mempty
        result = execParserPure (prefs noBacktrack) i ["c", "-b"]
    isOK $ assertError result $ const QC.Success


  it "prop_subparser_inline" $ defer \_ -> do
    let p2 = switch (short 'a')
        p1 = Tuple
          <$> subparser (command "c" (info p2 mempty))
          <*> switch (short 'b')
        i = info (p1 <**> helper) mempty
        result = execParserPure (prefs subparserInline) i ["c", "-b", "-a" ]
    isOK $ assertResult result ((Tuple true true) === _)


  it "prop_error_context" $ defer \_ -> do
    let p = Tuple <$> option int (long "port")
              <*> option int (long "key")
        i = info p mempty
        result = run i ["--port", "foo", "--key", "291"]
    isOK $ assertError result $ \failure ->
        let (Tuple msg _) = renderFailure failure "test"
            errMsg = fromMaybe "" $ Array.head $ lines msg
        in  conjoin [ String.Pattern "port" `isInfixOf` errMsg <?> "no context in error message (option)"
                    , String.Pattern "foo" `isInfixOf` errMsg <?> "no context in error message (value)"]


  it "prop_arg_order_1" $ defer \_ -> do
    let p = Tuple
            <$> argument (condr even) mempty
            <*> argument (condr odd) mempty
        i = info p mempty
        result = run i ["3", "6"]
    isOK $ assertError result $ const QC.Success


  it "prop_arg_order_2" $ defer \_ -> do
    let p = tuple3
          <$> argument (condr even) mempty
          <*> option (condr even) (short 'a')
          <*> option (condr odd) (short 'b')
        i = info p mempty
        result = run i ["2", "-b", "3", "-a", "6"]
    isOK $ assertResult result (_ === tuple3 2 6 3)


  it "prop_arg_order_3" $ defer \_ -> do
    let p = Tuple
            <$> (  argument (condr even) mempty
              <|> option int (short 'n') )
            <*> argument (condr odd) mempty
        i = info p mempty
        result = run i ["-n", "3", "5"]
    isOK $ assertResult result (_ === Tuple 3 5)

  it "prop_unix_style" $ quickCheck \j k ->
    let p = Tuple
            <$> flag' (j :: Int) (short 'x')
            <*> flag' (k :: Int) (short 'c')
        i = info p mempty
        result = run i ["-xc"]
    in assertResult result (_ === (Tuple j k))


  it "prop_unix_with_options" $ defer \_ -> do
    let p = Tuple
            <$> flag' 1 (short 'x')
            <*> strOption (short 'a')
        i = info p mempty
        result = run i ["-xac"]
    isOK $ assertResult result (_ === (Tuple 1 "c"))


  it "prop_count_flags" $ defer \_ -> do
    let p = List.length <$> many (flag' unit (short 't'))
        i = info p mempty
        result = run i ["-ttt"]
    isOK $ assertResult result (_ === 3)


  it "prop_issue_47" $ defer \_ -> do
    let p = option r (long "test" <> value 9)
        r = readerError "error message"
        result = run (info p mempty) ["--test", "x"]
    isOK $ assertError result $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  String.Pattern "error message" `isInfixOf` text <?> "no error message"


  it "prop_long_help" $ defer \_ -> do
    let p = Formatting.opts <**> helper
        i = info p
          ( progDesc (fold
              [ "This is a very long program description. "
              , "This text should be automatically wrapped "
              , "to fit the size of the terminal" ]) )
    isOK $ checkHelpTextWith ExitCode.Success (prefs (columns 50)) asset_formatting i ["--help"]


  it "prop_issue_50" $ defer \_ -> do
    let p = argument str (metavar "INPUT")
            <* switch (long "version")
        result = run (info p mempty) ["--version", "test"]
    isOK $ assertResult result $ \r -> "test" === r


  it "prop_intersperse_1" $ defer \_ -> do
    let p = many (argument str (metavar "ARGS"))
            <* switch (short 'x')
        result = run (info p noIntersperse)
                  ["a", "-x", "b"]
    isOK $ assertResult result $ \args -> ["a", "-x", "b"] === Array.fromFoldable args


  it "prop_intersperse_2" $ defer \_ -> do
    let p = subparser
            (  command "run"
              ( info (many (argument str (metavar "OPTIONS")))
                      noIntersperse )
            <> command "test"
              ( info (many (argument str (metavar "ARGS")))
                      mempty ) )
        i = info p mempty
        result1 = run i ["run", "foo", "-x"]
        result2 = run i ["test", "bar", "-x"]
    isOK $ conjoin [ assertResult result1 $ \args -> ["foo", "-x"] === Array.fromFoldable args
              , assertError result2 $ const QC.Success ]


  it "prop_intersperse_3" $ defer \_ -> do
    let p = tuple3 <$> switch ( long "foo" )
                <*> strArgument ( metavar "FILE" )
                <*> many ( strArgument ( metavar "ARGS..." ) )
        i = info p noIntersperse
        result = run i ["--foo", "myfile", "-a", "-b", "-c"]
    isOK $ assertResult result $ \(b /\ f /\ as /\ unit) ->
      conjoin [ ["-a", "-b", "-c"] === Array.fromFoldable as
              , true               === b
              , "myfile"           === f ]


  it "prop_forward_options" $ defer \_ -> do
    let p = Tuple <$> switch ( long "foo" )
                <*> many ( strArgument ( metavar "ARGS..." ) )
        i = info p forwardOptions
        result = run i ["--fo", "--foo", "myfile"]
    isOK $ assertResult result $ \(Tuple b a) ->
      conjoin [ true               === b
              , ["--fo", "myfile"] === Array.fromFoldable a ]


  it "prop_issue_52" $ defer \_ -> do
    let p = subparser
          ( metavar "FOO"
          <> command "run" (info (pure "foo") mempty) )
        i = info p mempty
    isOK $ assertError (run i []) $ \failure -> do
      let text = lines <<< fst $ renderFailure failure "test"
      ["Missing: FOO", "", "Usage: test FOO"] === text


  it "prop_multiple_subparsers" $ defer \_ -> do
    let p1 = subparser
          (command "add" (info (pure unit)
              ( progDesc "Add a file to the repository" )))
        p2 = subparser
          (command "commit" (info (pure unit)
              ( progDesc "Record changes to the repository" )))
        i = info (p1 *> p2 <**> helper) mempty
    isOK $ checkHelpText asset_subparsers i ["--help"]


  it "prop_argument_error" $ defer \_ -> do
    let r = (condr (_ == 42))
          <|> (str >>= \x -> readerError (x <> " /= 42"))
        p1 = argument r mempty
        i = info (p1 *> p1) mempty
    isOK $ assertError (run i ["3", "4"]) $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  "3 /= 42" === text


  it "prop_reader_error_mplus" $ defer \_ -> do
    let r = (condr (_ == 42))
          <|> (str >>= \x -> readerError (x <> " /= 42"))
        p1 = argument r mempty
        i = info p1 mempty
    isOK $ assertError (run i ["foo"]) $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  "foo /= 42" === text


  it "prop_missing_flags_described" $ defer \_ -> do
    let p = tuple3
          <$> option str (short 'a')
          <*> option str (short 'b')
          <*> optional (option str (short 'c'))
        i = info p mempty
    isOK $ assertError (run i ["-b", "3"]) $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  "Missing: -a ARG" === text


  it "prop_many_missing_flags_described" $ defer \_ -> do
    let p = Tuple
          <$> option str (short 'a')
          <*> option str (short 'b')
        i = info p mempty
    isOK $ assertError (run i []) $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  "Missing: -a ARG -b ARG" === text


  it "prop_alt_missing_flags_described" $ defer \_ -> do
    let p = option str (short 'a') <|> option str (short 'b')
        i = info p mempty
    isOK $ assertError (run i []) $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  "Missing: (-a ARG | -b ARG)" === text


  it "prop_missing_option_parameter_err" $ defer \_ -> do
    let p = option str (short 'a')
        i = info p mempty
    isOK $ assertError (run i ["-a"]) $ \failure ->
      let text = fromMaybe "" $ Array.head $ lines $ fst $ renderFailure failure "test"
      in  "The option `-a` expects an argument." === text


  it "prop_many_pairs_success" $ defer \_ -> do
    let p = many $ Tuple <$> argument str mempty <*> argument str mempty
        i = info p mempty
        nargs = 10000 - 9900 -- TODO remove `if we remove -9900` stack will explode
        result = run i (replicate nargs "foo")
    isOK $ assertResult result $ \xs -> nargs `div` 2 === List.length xs


  it "prop_many_pairs_failure" $ defer \_ -> do
    let p = many $ Tuple <$> argument str mempty <*> argument str mempty
        i = info p mempty
        nargs = 9999 - 9900 -- TODO remove `if we remove -9900` stack will explode
        result = run i (replicate nargs "foo")
    isOK $ assertError result $ const QC.Success


  it "prop_many_pairs_lazy_progress" $ defer \_ -> do
    let p = many $ Tuple <$> optional (option str (short 'a')) <*> argument str mempty
        i = info p mempty
        result = run i ["foo", "-abar", "baz"]
    isOK $ assertResult result $ \xs -> [(Tuple (Just "bar") "foo"), (Tuple Nothing "baz")] === Array.fromFoldable xs


  it "prop_suggest" $ defer \_ -> do
    let p2 = subparser (command "first"   (info (pure unit) mempty))
        p1 = subparser (command "fst"     (info (pure unit) mempty))
        p3 = subparser (command "far-off" (info (pure unit) mempty))
        p  = p2 *> p1 *> p3
        i  = info p mempty
        result = run i ["fist"]
    isOK $ assertError result $ \failure ->
      let (Tuple msg _) = renderFailure failure "prog"
      in  String.Pattern "Did you mean one of these?\n    first\n    fst" `isInfixOf` msg <?> msg
  it "prop_listToChunk_1" $ quickCheck prop_listToChunk_1
  it "prop_listToChunk_2" $ quickCheck prop_listToChunk_2
  it "prop_extractChunk_1" $ quickCheck prop_extractChunk_1
  it "prop_extractChunk_2" $ quickCheckGen prop_extractChunk_2
  it "prop_stringChunk_1" $ quickCheckGen prop_stringChunk_1
  it "prop_stringChunk_2" $ quickCheck prop_stringChunk_2
  it "prop_paragraph" $ quickCheck prop_paragraph
  it "prop_edit_distance_gezero" $ quickCheck prop_edit_distance_gezero
  it "prop_edit_insertion" $ quickCheck prop_edit_insertion
  it "prop_edit_symmetric" $ quickCheck prop_edit_symmetric
  it "prop_edit_substitution" $ quickCheckGen prop_edit_substitution
  it "prop_edit_transposition" $ quickCheckGen prop_edit_transposition



equalDocs :: Number -> Int -> Doc -> Doc -> QC.Result
equalDocs f w d1 d2 = Doc.renderPretty f w d1
                  === Doc.renderPretty f w d2

prop_listToChunk_1 :: Array String -> QC.Result
prop_listToChunk_1 xs = isEmpty (listToChunk xs) === Array.null xs

prop_listToChunk_2 :: Array String -> QC.Result
prop_listToChunk_2 xs = listToChunk xs === fold (map pure xs)

prop_extractChunk_1 :: String -> QC.Result
prop_extractChunk_1 x = extractChunk (pure x) === x

prop_extractChunk_2 :: forall m. MonadRec m => MonadGen m => m QC.Result
prop_extractChunk_2 = do
  x <- Chunk <$> genMaybe (genString genUnicodeChar)
  pure $ extractChunk (map pure x) === x

prop_stringChunk_1 :: forall m. MonadRec m => MonadGen m => m QC.Result
prop_stringChunk_1 = do
  f <- chooseFloat 0.0 1.0
  w <- chooseInt 0 200
  s <- String.replaceAll (String.Pattern "\n") (String.Replacement "") <$> genString genUnicodeChar
  pure $ equalDocs f w (extractChunk (stringChunk s))
                (Doc.string s)

prop_stringChunk_2 :: String -> QC.Result
prop_stringChunk_2 s = isEmpty (stringChunk s) === (s == "")

prop_paragraph :: String -> QC.Result
prop_paragraph s = isEmpty (paragraph s) == Array.null (words s) <?> show {s, p: paragraph s, w: words s}

---

--
-- From
-- https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance
--
-- In information theory and computer science, the Damerau–Levenshtein
-- distance is a distance (string metric) between two strings, i.e.,
-- finite sequence of symbols, given by counting the minimum number
-- of operations needed to transform one string into the other, where
-- an operation is defined as an insertion, deletion, or substitution
-- of a single character, or a transposition of two adjacent characters.
--
prop_edit_distance_gezero :: String -> String -> Boolean
prop_edit_distance_gezero a b = editDistance (toCharArray a) (toCharArray b) >= 0

prop_edit_insertion :: Array Char -> Char -> Array Char -> QC.Result
prop_edit_insertion as i bs =
  editDistance (as <> bs) (as <> [i] <> bs) === 1

prop_edit_symmetric :: Array Char -> Array Char -> QC.Result
prop_edit_symmetric as bs =
  editDistance as bs === editDistance bs as

prop_edit_substitution :: forall m. MonadGen m => MonadRec m => m QC.Result
prop_edit_substitution = do
  as <- toCharArray <$> genString genUnicodeChar
  bs <- toCharArray <$> genString genUnicodeChar
  a <- genUnicodeChar
  b <- suchThat genUnicodeChar \b -> a /= b
  pure $ editDistance (as <> [a] <> bs) (as <> [b] <> bs) === 1

prop_edit_transposition :: forall m. MonadGen m => MonadRec m => m QC.Result
prop_edit_transposition = do
  as <- toCharArray <$> genString genUnicodeChar
  bs <- toCharArray <$> genString genUnicodeChar
  a <- genUnicodeChar
  b <- suchThat genUnicodeChar \b -> a /= b
  -- TODO in haskell version transposition is 1 operation but that that algorithm
  -- was a bit harder and I've ported simpler one which counts transposition as 2 operations
  pure $ editDistance (as <> [a] <> [b] <> bs) (as <> [b] <> [a] <> bs) === 2

---

main :: Effect Unit
main = launchAff_ $ Spec.runSpec [consoleReporter] spec

quickCheckGen :: forall p. QC.Testable p => Gen p -> Aff Unit
quickCheckGen = quickCheck' 100

quickCheck :: forall p. QC.Testable p => p -> Aff Unit
quickCheck = quickCheck' 100

-- | Runs a Testable with a random seed and the given number of inputs.
quickCheck' :: forall p. QC.Testable p => Int -> p -> Aff Unit
quickCheck' n prop = do
  seed <- liftEffect QC.randomSeed
  quickCheckPure seed n prop

getErrorMessage :: QC.Result -> Maybe String
getErrorMessage (QC.Failed msg) = Just msg
getErrorMessage _ = Nothing

-- | Runs a Testable with a given seed and number of inputs.
quickCheckPure :: forall p.
                  (QC.Testable p) =>
                  QC.Seed ->
                  Int ->
                  p ->
                  Aff Unit
quickCheckPure seed n prop = do
  let results = QC.quickCheckPure seed n prop
  let msgs = List.mapMaybe getErrorMessage results

  if List.length msgs > 0
    then throwError $ error $ List.intercalate "\n  " msgs
    else pure unit
