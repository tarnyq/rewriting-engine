import Test.Tasty

import qualified CleanImpTest
import qualified KTutImpTest
import qualified NonDetTest

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ CleanImpTest.tests
    , KTutImpTest.tests
    , NonDetTest.tests
    ]
