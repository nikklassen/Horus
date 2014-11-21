# TextToMath
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/nikklassen/TextToMath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# Installation
  ## Ubuntu
  1. Use the steps [here](https://gist.github.com/yantonov/10083524) to install GHC 7.8.2 (They do 7.8.3 but the steps should be the same) and Cabal 1.20.0.0.
  2. `clone` and `cd` TextToMath and in the root directory run `cabal sandbox init`
  3. Install dependencies with `cabal install --only-dependencies`
  4. Grab `npm` from the package manager and run `npm install` in the root directory
  5. run `make`
  6. `cd /bin` and run the server executable. The default port is 3000.
  
Make beautiful math.
