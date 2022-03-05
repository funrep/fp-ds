import Test.Tasty
import qualified TreeSpec
import qualified TreeHeapSpec
import qualified BinHeapSpec

main = defaultMain $ testGroup "tests" 
  [ TreeSpec.tests
  , TreeHeapSpec.tests
  , BinHeapSpec.tests
  ]
