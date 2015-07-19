{-# LANGUAGE CPP #-}
module Graphics.UI.Gtk.OSX.Window (
    allowFullscreen
) where

import System.Glib.FFI
{#import Graphics.UI.Gtk.OSX.Types#}

allowFullscreen :: DrawWindowClass window => window -> IO ()
allowFullscreen window =
    {# call unsafe gtk2hs_osx_allow_fullscreen #}
        (toDrawWindow window)

