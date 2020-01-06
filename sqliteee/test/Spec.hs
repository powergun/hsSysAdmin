import qualified SqliteSimple.Demo
import qualified SqliteSimple.OpenMemory
import qualified SqliteSimple.Parameters
import qualified SqliteSimple.SelectMemory

main :: IO ()
main = do
  -- SqliteSimple.Demo.demo
  SqliteSimple.OpenMemory.demo
  SqliteSimple.SelectMemory.demo
  SqliteSimple.Parameters.demo
