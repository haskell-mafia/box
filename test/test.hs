import qualified Box.QueryTest
import qualified Box.ParseTest
import           Control.Monad

main :: IO ()
main = void $ do
  Box.QueryTest.run
  Box.ParseTest.run
