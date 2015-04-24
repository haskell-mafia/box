import           Orphanarium.Core.Main

import qualified Test.Box.Parse
import qualified Test.Box.Query

main :: IO ()
main =
  orphanariumMain [
       Test.Box.Parse.tests
    ,  Test.Box.Query.tests
    ]
