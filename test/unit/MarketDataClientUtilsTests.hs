module MarketDataClientUtilsTests where

import MarketDataClientUtils
import MarketDataClientTypes

import Test.Tasty
import Test.Tasty.HUnit ( testCase, (@?=) )
import Data.String (IsString (fromString))

rqQueryStrTests :: TestTree
rqQueryStrTests = testGroup "Simple rqQueryStr builder Tests"
  [ 
    testCase "Returns default limit" $
      rqQueryStr ReqParams { host="", endpoint="", apiKey="", query = QueryParams 0 0} @?= [(fromString "limit", Just (fromString "100"))]
    
    , testCase "Returns limit from QueryParams" $
      rqQueryStr ReqParams { host="", endpoint="", apiKey="", query = QueryParams {start=0, limit=10}} @?= [(fromString "limit", Just (fromString "10"))]
  ]
