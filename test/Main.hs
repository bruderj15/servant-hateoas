module Main (main) where

import qualified Servant.Hateoas.ExampleSpec as ExampleSpec
import           Test.Hspec                  (hspec)

main :: IO ()
main = hspec ExampleSpec.spec
