import           Disorder.Core.Main

import qualified Test.Box.Data
import qualified Test.Box.Query

main :: IO ()
main =
  disorderMain [
      Test.Box.Data.tests
    , Test.Box.Query.tests
    ]
