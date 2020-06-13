import qualified System.Random as R
import Data.Random.Normal
import SeedsModel.Germ

main :: IO ()
main = do
  gen' <- R.getStdGen
  let psis = normals' (0.0, 1.0) gen'
  go psis 50
