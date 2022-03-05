import Test.Tasty
import qualified TreeSpec as TreeSpec
import qualified HeapSpec as HeapSpec

main = defaultMain $ testGroup "tests" 
  [ TreeSpec.tests
  , HeapSpec.tests
  ]
