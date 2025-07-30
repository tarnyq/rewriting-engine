import Test.Tasty

import qualified KTutImpTest
import qualified CleanImpTest

main :: IO ()
main = defaultMain $ testGroup "Tests" [KTutImpTest.tests, CleanImpTest.tests]
