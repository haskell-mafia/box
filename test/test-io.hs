import           Disorder.Core.Main

import qualified Test.IO.Box.Cache
import qualified Test.IO.Box.Store

main :: IO ()
main =
  disorderMain [
      Test.IO.Box.Cache.tests
    , Test.IO.Box.Store.tests
    ]
