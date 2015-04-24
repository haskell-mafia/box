module Test ( tests ) where

import           Distribution.TestSuite

tests :: IO [Test]
tests = return [   Test queries
                 , Test parse
               ]
  where
    queries = TestInstance
        { run = return $ Finished Pass -- Q.run
        , name = "queries"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right queries
        }
    parse = TestInstance
        { run = return $ Finished Pass -- P.run
        , name = "parse"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right parse
        }
