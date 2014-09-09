import           Edabo.CmdLine (handleArgs)
import           Edabo.Utils   (ensureUserDir)

main :: IO ()
main = do
  ensureUserDir
  handleArgs
