-- import qualified Effects.Auth
import qualified Effects.FileStore
import Hedgehog
import Control.Monad (void)

main :: IO ()
main = void $ checkParallel Effects.FileStore.tests
