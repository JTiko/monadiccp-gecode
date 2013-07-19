monadiccp-gecode
================

Gecode extension for the Monadic Constraint Programming Framework
containing a code generator and a runtime solver backend for the C++ based constraint library Gecode

This package requires a working Gecode 3.1.0 installation.
A Gecode 3.1.0 source package can be downloaded from http://www.gecode.org/download/gecode-3.1.0.tar.gz.

After installing Gecode 3.1.0 this package can be installed by running
cabal install --extra-include-dirs=$GECODE/include --extra-lib-dirs=$GECODE/lib

$GECODE refers to the installation directory of Gecode 3.1.0
