import Test.Tasty

import qualified KTutImpTest
import qualified CleanImpTest
import qualified NonDetTest

main :: IO ()
main = defaultMain $ testGroup "Tests" [KTutImpTest.tests, CleanImpTest.tests, NonDetTest.tests]
