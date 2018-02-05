import SumSpec
import MinSpec
import MaxSpec
import Test.HUnit

allTests = testsSumSpec ++ testsMinSpec ++ testsMaxSpec
main :: IO Counts
main = runTestTT $ TestList allTests
