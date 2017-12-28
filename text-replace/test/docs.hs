{-# LANGUAGE CPP #-}

#ifdef DOCTEST
import Test.DocTest

main :: IO ()
main = doctest ["src"]
#else

main :: IO ()
main = putStrLn "Tests using doctest are disabled."

#endif
