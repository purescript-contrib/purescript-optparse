module Examples.Cabal where

import Prelude

import Control.Apply (lift2)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Options.Applicative (Parser, ParserInfo, command, execParser, help, helper, hidden, hsubparser, info, infoOption, int, long, many, metavar, option, progDesc, short, strOption, switch, value, (<**>))

data Args = Args CommonOpts Command
derive instance genericArgs :: Generic Args _
instance showArgs :: Show Args where show = genericShow


data CommonOpts = CommonOpts
  { optVerbosity :: Int }
derive instance genericCommonOpts :: Generic CommonOpts _
instance showCommonOpts :: Show CommonOpts where show = genericShow


data Command
  = Install ConfigureOpts InstallOpts
  | Update
  | Configure ConfigureOpts
  | Build BuildOpts
derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show = genericShow


data InstallOpts = InstallOpts
  { instReinstall :: Boolean
  , instForce :: Boolean }
derive instance genericInstallOpts :: Generic InstallOpts _
instance showInstallOpts :: Show InstallOpts where show = genericShow


data ConfigureOpts = ConfigureOpts
  { configTests :: Boolean
  , configFlags :: List String }
derive instance genericConfigureOpts :: Generic ConfigureOpts _
instance showConfigureOpts :: Show ConfigureOpts where show = genericShow


type FilePath = String
data BuildOpts = BuildOpts
  { buildDir :: FilePath }
derive instance genericBuildOpts :: Generic BuildOpts _
instance showBuildOpts :: Show BuildOpts where show = genericShow


version :: forall a. Parser (a -> a)
version = infoOption "0.0.0"
  (  long "version"
  <> help "Print version information"
  <> hidden)

parser :: Parser Args
parser = ado
  opts <- commonOpts
  cmds <- hsubparser
            ( command "install"
              (info installParser
                    (progDesc "Installs a list of packages"))
           <> command "update"
              (info updateParser
                    (progDesc "Updates list of known packages"))
           <> command "configure"
              (info configureParser
                    (progDesc "Prepare to build the package"))
           <> command "build"
              (info buildParser
                    (progDesc "Make this package ready for installation")) )
  in Args opts cmds

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts <<< {optVerbosity:_}
  <$> option int
      ( short 'v'
     <> long "verbose"
     <> metavar "LEVEL"
     <> help "Set verbosity to LEVEL"
     <> value 0 )

installParser :: Parser Command
installParser = ado
  config <- configureOpts
  inst <- installOpts
  in Install config inst

installOpts :: Parser InstallOpts
installOpts = ado
  reinst <- switch (long "reinstall")
  force <- switch (long "force-reinstall")
  in InstallOpts
             { instReinstall: reinst
             , instForce: force }

updateParser :: Parser Command
updateParser = pure Update

configureParser :: Parser Command
configureParser = ado
  config <- configureOpts
  in Configure config

configureOpts :: Parser ConfigureOpts
configureOpts = ado
  tests <- switch
             ( long "enable-tests"
            <> help "Enable compilation of test suites" )
  flags <- many $ strOption
             ( short 'f'
            <> long "flags"
            <> metavar "FLAGS"
            <> help "Enable the given flag" )
  in ConfigureOpts {configTests: tests, configFlags: flags}

buildParser :: Parser Command
buildParser = ado
  opts <- buildOpts
  in Build opts

buildOpts :: Parser BuildOpts
buildOpts = ado
  buildDir <- strOption
            ( long "builddir"
           <> metavar "DIR"
           <> value "dist" )
  in BuildOpts {buildDir}

pinfo :: ParserInfo Args
pinfo = info (parser <**> (lift2 (>>>) version helper))
  ( progDesc "An example modelled on cabal" )

main :: Effect Unit
main = do
  r <- execParser pinfo
  logShow r
