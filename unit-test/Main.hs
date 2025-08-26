import Test.Tasty

import qualified KTutImpTest
import qualified NonDetTest

main :: IO ()
main = defaultMain $ testGroup "Tests" [KTutImpTest.tests, NonDetTest.tests]
