# TextToMath

[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/nikklassen/TextToMath?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

TextToMath is a simple mathematical programming language/scientific calculator.  The main advantage over a traditional scientific calculator is that functions and variables are supported, and tied to the user's session.  A working instance can be found [here](http://nikklassen.ca/TextToMath).

## Variables

There are two different types of variables, although they can be generalized as a variable is a value that is always equal to the right hand side of its definition.  A traditional variable is defined as follows

```c
a = 2 + 3
```

A _bound_ variable is a variable whose value depends on the current value of another variable.  It is defined as such

```c
b := a * 4
```

Currently `b` would be 20, but if `a` changed, so would `b`.  Recursive definitions are not allowed.

## Functions

Functions work as usual, with local parameters shadowing global variables.  For example, in

```c
a = 4
f(x) = x + a
```

the function `f` will add the value of its parameter `x` to the current value of the global variable `a`.  Functions can call other functions, but scopes do not propogate in any way.  Many standard scientific functions are built in.

# Installation

TextToMath requires `GHC >= 7.8.*`, and `node.js` for tests.  [Here](https://gist.github.com/yantonov/10083524) are Ubuntu instructions for installing `GHC` if you need a place to start.  Package installation for TextToMath can be done using cabal and npm.  For example

```bash
# Working directory is the clone of TextToMath
cabal sandbox init
cabal install --only-dependencies

make
```

If you don't want to use a cabal sandbox you will have to remove the applicable GHC args in the Makefile.

To run the tests do

```bash
npm install
make tests
```

Ensure that the local `node_modules` is in your path, or install the `protractor` and `mocha` modules globally.

## Hosting

Static files are all in `resources`, so these can be served with your method of choice.  The default port for the server executable is 3000, and needs to be served on the same port as the static files, under the path `/api`.  A sample `nginx` config is

```nginx
location ^~ /TextToMath {
	alias $CHECKOUT_DIR/TextToMath/resources/html;
	
	location ~* \.(css|js)$ {
		root $CHECKOUT_DIR/TextToMath/resources/;
	    rewrite ^/TextToMath/(.*)$ /$1 break;
	}
	
	location ~ api/ {
	    rewrite ^/TextToMath/api/(.*) /$1 break;
	    proxy_pass http://localhost:3000;
	}
}
```

where `$CHECKOUT_DIR` is specific to you.
