monadiccp-gecode
================

Gecode extension for the Monadic Constraint Programming Framework
containing a code generator and a runtime solver backend for the
C++ based constraint library Gecode

This package requires a working Gecode installation.
A Gecode source package can be downloaded from http://www.gecode.org/download/.

After installing Gecode this package can be installed by running
`cabal install --extra-include-dirs=$GECODE/include --extra-lib-dirs=$GECODE/lib`

where `$GECODE` refers to the installation directory of Gecode

Acknowledgments
---------------

 * Thanks to Johannes Waldmann for adding support for Gecode 6.0.0 and getting
   the package to compile against GHC 8.4.3
