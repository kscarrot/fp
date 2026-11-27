import Test.Hspec
import LinearSpec
import QuickSortSpec

main :: IO ()
main = hspec $ do
    LinearSpec.spec
    QuickSortSpec.spec 