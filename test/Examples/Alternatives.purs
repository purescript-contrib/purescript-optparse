module Examples.Alternatives where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Options.Applicative (Parser, ParserInfo, execParser, flag', helper, idm, info, short, (<**>))
import Options.Applicative.Types (many)

data Value = A | B

derive instance valueEq :: Eq Value
derive instance genericValue :: Generic Value _
instance showValue :: Show Value where show = genericShow

values :: Parser (Array Value)
values = map Array.fromFoldable $ many $ a <|> b

a :: Parser Value
a = flag' A (short 'a')

b :: Parser Value
b = flag' B (short 'b')

opts :: ParserInfo (Array Value)
opts = info (values <**> helper) idm

main :: Effect Unit
main = do
  r <- execParser (opts)
  logShow r
