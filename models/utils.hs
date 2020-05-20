import qualified System.Random as R
import Data.Random.Normal
import Chromar
import SeedsModel.Env
import SeedsModel.Utils

main :: IO ()
main = do
  gen <- R.getStdGen
  let psis = normals' (0.0, 6.0) gen
  go psis 1
