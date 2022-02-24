import Test.Tasty
import qualified TreeSpec as TreeSpec

main = defaultMain $ testGroup "tests" 
  [ TreeSpec.tests
  ]
