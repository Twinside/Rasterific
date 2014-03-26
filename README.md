Rasterific
==========

![Rasterific logo](https://raw.github.com/Twinside/Rasterific/master/img/logo.png)

[![Build Status](https://travis-ci.org/Twinside/Rasterific.png?branch=master)](https://travis-ci.org/Twinside/Rasterific)

Rasterific is a Haskell rasterization engine (a vectorial renderer)
implemented on top of [JuicyPixels](https://github.com/Twinside/Juicy.Pixels).
Rasterific bases its text rendering on [FontyFruity](https://github.com/Twinside/FontyFruity).

Design
------
The renderer design is based on the
[Nile](https://github.com/damelang/nile) /
[Gezira](https://github.com/damelang/gezira) renderer from the STEP
project from the [VPRI](http://www.vpri.org/index.html) institute. The
interesting thing about this renderer is the conciseness of it's
implementation, providing antialiased rendering in the way.

