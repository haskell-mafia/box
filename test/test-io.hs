import           Disorder.Core.Main

import qualified Test.IO.Box.Store

main :: IO ()
main =
  disorderMain [
      Test.IO.Box.Store.tests
    ]
