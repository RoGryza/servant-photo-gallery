import qualified Effects.Auth
import qualified Effects.FileStore
import qualified Effects.PostDatabase
import qualified Auth
import qualified Types

main :: IO ()
main = do
  Effects.Auth.runTests
  Effects.FileStore.runTests
  Effects.PostDatabase.runTests
  Auth.runTests
  Types.runTests
