# LambdaCalculus

A library for lambda calculus with browser client

Try it: [instance](http://lambdasistemi.net/public/ghcjs/Lambda/)

# Compilation to Javascript

To compile to javascript you need to install [reflex-platform](https://github.com/reflex-frp/reflex-platform)

After that, from the reflex shell run 

`cabal configure --ghcjs`

`cabal build`

The page assets will be found in `dist/build/LambdaCalculus/LambdaCalculus.jsexe/`

After first build html assets can be copied there (needed only first time)

`cp index.html client.css dist/build/LambdaCalculus/LambdaCalculus.jsexe/`

Now you can use it with the browser

    firefox `pwd`/dist/build/LambdaCalculus/LambdaCalculus.jsexe/index.html


