import           Disorder.Core.Main

import qualified Test.Box.Parse
import qualified Test.Box.Query

main :: IO ()
main =
  disorderMain [
       Test.Box.Parse.tests
    ,  Test.Box.Query.tests
    ]
