import Test.Hspec
import CarteQuickcheck as CS
import EnvironementSpec as ES
import EnvironnementQuickcheck as EQ
main :: IO ()
--main = putStrLn "Test suite not yet implemented"
main = hspec $ do
    ES.engineSpec    
    CS.carteAllCoordsInBoundsSpec
    CS.carteCoordSpec
    EQ.entiteSpec

