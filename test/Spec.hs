import qualified Effects.Auth
import qualified Effects.FileStore
import qualified Effects.PostDatabase

main :: IO ()
main = do
  Effects.Auth.runTests
  Effects.FileStore.runTests
  Effects.PostDatabase.runTests
