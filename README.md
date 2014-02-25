Rasterific
==========

![Rasterific logo](https://raw.github.com/Twinside/Rasterific/master/img/logo.png)

Rasterific is an Haskell rasterization engine (a vectorial renderer)
implemented on top of [JuicyPixels](https://github.com/Twinside/Juicy.Pixels).
Rasterific bases its text rendering on [FontyFruity](https://github.com/Twinside/FontyFruity).

Design
------
The renderer design is based on the
[Nile](https://github.com/damelang/nile) /
[Gezira](https://github.com/damelang/gezira) renderer from the STEP
project from the [VPRI](http://www.vpri.org/index.html) institute. The
interesting thing about this renderer is the tinyness of the
implementation, providing antialiased rendering in the way.

