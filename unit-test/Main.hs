import Test.Tasty

import qualified KTutImpTest
import qualified NonDetTest
import qualified MiniCTest

main :: IO ()
main = defaultMain $ testGroup "Tests" [KTutImpTest.tests, NonDetTest.tests, MiniCTest.tests]
