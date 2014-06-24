
Creates a directory for your dependencies to live in:

    $ cabal sandbox init

This builds the project and downloads its dependencies. Do this after adding any dependencies:

    $ cabal install

To run the program:

	$ .cabal-sandbox/bin/HaskellPlayground

The name of the binary is given by the Cabal file "executable" pragma

To get a REPL:

	$ cabal repl

This starts GHCI using the libraries installed in the sandbox.



