import Test.Hspec
import CarteSpec as CS
import EnvironementSpec as ES

main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
    -- carte
    CS.carteAllCoordsInBoundsSpec
    CS.carteCoordSpec
    ES.engineSpec
