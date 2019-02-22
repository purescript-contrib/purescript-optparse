module Examples.Formatting where

import Prelude
import Options.Applicative (Parser, help, int, long, metavar, option, short, value)
import Data.Foldable (fold)

opts :: Parser Int
opts = option int $ fold
  [ long "test"
  , short 't'
  , value 0
  , metavar "FOO_BAR_BAZ_LONG_METAVARIABLE"
  , help "This is an options with a very very long description.  Hopefully, this will be nicely formatted by the help text generator." ]
