module CoinDataUtils where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


toBStr :: String -> BL.ByteString
toBStr = TLE.encodeUtf8.TL.pack

toKey :: String -> Key
toKey = K.fromString

toParser :: FromJSON a => Object -> String -> Parser a
toParser o s = o .: toKey s 

-- | Convenience method for annotating the error message output by a parser
--  with the field name that will be looked up in the supplied object
-- Example:
--
-- > parseJSON (Object o) = annotatedParser o "fieldName"
annotatedParser :: FromJSON a => Object -> String -> Parser a
annotatedParser obj fieldName = 
  modifyFailure ("Error parsing field: \"" <> fieldName <> "\" " ++)
  (obj .: toKey fieldName)

qq = QuasiQuoter { 
    quoteExp = stringE -- only interested in expressions currently
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }