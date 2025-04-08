Assignment 1 stub using gloss
=============================

MacOS
-----

Install XQuartz:

    brew install --cask xquartz

Then, you should be able to build and run the stub:

    stack run

Windows
-------

On Windows, Haskell works best using the Stack build tool.

First, install the GLUT library:

    stack exec -- pacman -S mingw-w64-freeglut

Then, you should be able to build and run the stub:

    stack run
