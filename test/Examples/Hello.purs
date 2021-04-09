module Examples.Hello where

import Prelude
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, int, long, metavar, option, progDesc, short, showDefault, strOption, switch, value, (<**>))
import Data.Array (replicate)
import Data.Foldable (sequence_)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Sample = Sample
  { hello  :: String
  , quiet  :: Boolean
  , repeat :: Int }

derive instance genericSample :: Generic Sample _
instance showSample :: Show Sample where show = genericShow

sample :: Parser Sample
sample = map Sample $ ({ hello:_, quiet:_, repeat:_})
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option int
          ( long "repeat"
         <> help "Repeats for greeting"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: Effect Unit
main = greet =<< execParser opts

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

greet :: Sample -> Effect Unit
greet (Sample {hello, quiet:false, repeat}) = sequence_ $ replicate repeat $ log $ "Hello, " <> hello
greet _ = pure unit

