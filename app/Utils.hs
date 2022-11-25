-- |

module Utils where
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.ByteString (ByteString)

bsToText :: ByteString -> Text
bsToText = T.decodeUtf8
