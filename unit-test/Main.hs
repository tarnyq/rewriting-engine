import Test.Tasty

import qualified KTutImpTest
import qualified CleanImpTest
import qualified NonDetTest
import qualified SBVTest (tests)

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ KTutImpTest.tests
    , CleanImpTest.tests
    , NonDetTest.tests
    , SBVTest.tests
    ]
