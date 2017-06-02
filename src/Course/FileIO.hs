{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

{-
Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

-}


-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= \arg ->
         case arg of
          Nil -> putStrLn "give me an argument"
          h :. _ -> run h

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run name = 
  readFile name >>= \c ->
  getFiles (lines c) >>= \z ->
  printFiles z
  -- read the initial file
  --- split it into lines
  --- for each line : getThatFile and then print that file


getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles paths = sequence ( getFile <$> paths)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile name = (\c -> (name, c)) <$> readFile name
--readFile path >>= \contents -> pure (path, contents)
-- List Char

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles x = void (sequence (uncurry printFile <$> x))
-- void . sequence . (<$>) (uncurry printFile)

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name contents =
  putStrLn ("============ " ++ name) *> putStrLn contents