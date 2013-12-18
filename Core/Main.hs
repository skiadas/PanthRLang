module Main where
    
import Interp

main :: IO ()
main = interactive

interactive :: IO ()
interactive = do {
    putStr "\n> ";
    l <- getLine;
    putStr $ show $ eval l;
    main;
}
