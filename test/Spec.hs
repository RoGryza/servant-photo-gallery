import qualified Effects.Auth
import qualified Effects.FileStore
import Hedgehog
import Control.Monad (void)

main :: IO ()
main = do
  void $ checkParallel $ Effects.Auth.tests
  void $ checkParallel $ Effects.FileStore.tests
