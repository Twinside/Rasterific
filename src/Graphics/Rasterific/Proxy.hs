module Graphics.Rasterific.Proxy ( Proxy( .. )
                                 , asProxyTypeOf
                                 ) where

data Proxy p = Proxy

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf a Proxy = a

