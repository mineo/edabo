import Data.Aeson.Encode.Pretty (encodePretty)
import Edabo.MPD (getTracksFromPlaylist)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
     tl <- getTracksFromPlaylist
     case tl of
       Left e -> putStrLn e
       Right tracks -> B.putStrLn $  encodePretty tracks