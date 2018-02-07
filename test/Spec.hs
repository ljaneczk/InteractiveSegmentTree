import SumSpec
import MinSpec
import MaxSpec
import GetSpec
import SetSpec
import AddSpec
import Test.HUnit
import Test.QuickCheck


main :: IO Counts
main = do
    let allTests = testsSumSpec ++ testsMinSpec ++ testsMaxSpec
    quickCheck (withMaxSuccess 1000 isGetFunctionCorrect)
    quickCheck (withMaxSuccess 1000 isSetFunctionCorrect)
    quickCheck (withMaxSuccess 1000 isAddFunctionCorrect)
    runTestTT $ TestList allTests
