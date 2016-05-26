{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) SourceBuffer
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2003-2008 Peter Gavin, Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
module Graphics.UI.Gtk.OSX.Application (
  Application,
  ApplicationClass,
  castToApplication,
  gTypeApplication,
  toApplication,

  applicationNew,
  applicationReady,
  applicationSetUseQuartsAccelerators,
  applicationSetUseQuartzAccelerators,
  applicationGetUseQuartsAccelerators,
  applicationGetUseQuartzAccelerators,
  applicationSetMenuBar,
  applicationSyncMenuBar,
  applicationInsertAppMenuItem,
  applicationSetWindowMenu,
  applicationSetHelpMenu,
  GtkosxApplicationAttentionType(..),
  applicationSetDockMenu,
  applicationSetDockIconPixbuf,
  applicationSetDockIconResource,
  AttentionRequestID(..),
  applicationAttentionRequest,
  applicationCancelAttentionRequest,
  applicationGetBundlePath,
  applicationGetResourcePath,
  applicationGetExecutablePath,
  applicationGetBundleId,
  applicationGetBundleInfo,
  didBecomeActive,
  willResignActive,
  blockTermination,
  willTerminate,
  openFile
 ) where

import Control.Monad	(liftM)
import Data.Maybe    (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GObject              (objectNew, makeNewGObject)
{#import System.Glib.Properties#}
import System.Glib.Attributes
{#import Graphics.UI.Gtk.OSX.Types#}
{#import Graphics.UI.Gtk.OSX.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- methods

applicationNew :: IO Application
applicationNew = makeNewGObject mkApplication $ liftM castPtr $
    objectNew gTypeApplication []

-- |
--
applicationReady :: ApplicationClass self => self -> IO ()
applicationReady self =
    {#call unsafe gtkosx_application_ready#} (toApplication self)

-- |
--
applicationSetUseQuartzAccelerators :: ApplicationClass self => self -> Bool -> IO ()
applicationSetUseQuartzAccelerators self b =
    {#call unsafe gtkosx_application_set_use_quartz_accelerators#} (toApplication self) (fromBool b)
applicationSetUseQuartsAccelerators :: ApplicationClass self => self -> Bool -> IO ()
applicationSetUseQuartsAccelerators = applicationSetUseQuartzAccelerators
{-# DEPRECATED applicationSetUseQuartsAccelerators "instead of 'applicationSetUseQuartsAccelerators' use 'applicationSetUseQuartzAccelerators'" #-}

-- |
--
applicationGetUseQuartzAccelerators :: ApplicationClass self => self -> IO Bool
applicationGetUseQuartzAccelerators self = liftM toBool $
    {#call unsafe gtkosx_application_use_quartz_accelerators#} (toApplication self)
applicationGetUseQuartsAccelerators :: ApplicationClass self => self -> IO Bool
applicationGetUseQuartsAccelerators = applicationGetUseQuartzAccelerators
{-# DEPRECATED applicationGetUseQuartsAccelerators "instead of 'applicationGetUseQuartsAccelerators' use 'applicationGetUseQuartzAccelerators'" #-}

-- |
--
applicationSetMenuBar :: (ApplicationClass self, MenuShellClass menu) => self -> menu -> IO ()
applicationSetMenuBar self menu =
    {#call unsafe gtkosx_application_set_menu_bar#} (toApplication self) (toMenuShell menu)

-- |
--
applicationSyncMenuBar :: ApplicationClass self => self -> IO ()
applicationSyncMenuBar self =
    {#call unsafe gtkosx_application_sync_menubar#} (toApplication self)

-- |
--
applicationInsertAppMenuItem :: (ApplicationClass self, WidgetClass menu_item) => self -> menu_item -> Int -> IO ()
applicationInsertAppMenuItem self menu_item index =
    {#call unsafe gtkosx_application_insert_app_menu_item#} (toApplication self) (toWidget menu_item) (fromIntegral index)

-- |
--
applicationSetWindowMenu :: (ApplicationClass self, MenuItemClass menuItem)
    => self -> menuItem -> IO ()
applicationSetWindowMenu self menuItem =
    {#call unsafe gtkosx_application_set_window_menu#} (toApplication self) (toMenuItem menuItem)

-- |
--
applicationSetHelpMenu :: (ApplicationClass self, MenuItemClass menuItem)
    => self -> menuItem -> IO ()
applicationSetHelpMenu self menuItem =
    {#call unsafe gtkosx_application_set_help_menu#} (toApplication self) (toMenuItem menuItem)

{#enum GtkosxApplicationAttentionType {underscoreToCase} deriving (Eq,Show)#}

-- |
--
applicationSetDockMenu :: (ApplicationClass self, MenuShellClass menu) => self -> menu -> IO ()
applicationSetDockMenu self menu =
    {#call unsafe gtkosx_application_set_dock_menu#} (toApplication self) (toMenuShell menu)

-- |
--
applicationSetDockIconPixbuf :: (ApplicationClass self, PixbufClass pixbuf) => self -> Maybe pixbuf -> IO ()
applicationSetDockIconPixbuf self mbPixbuf =
    {#call unsafe gtkosx_application_set_dock_icon_pixbuf#} (toApplication self)
        (maybe (Pixbuf nullForeignPtr) toPixbuf mbPixbuf)

-- |
--
applicationSetDockIconResource :: (ApplicationClass self, GlibString string)
    => self -> string -> string -> string -> IO ()
applicationSetDockIconResource self name rType subdir =
    withUTFString name $ \namePtr ->
    withUTFString rType $ \typePtr ->
    withUTFString subdir $ \subdirPtr ->
    {#call unsafe gtkosx_application_set_dock_icon_resource#} (toApplication self) namePtr typePtr subdirPtr

newtype AttentionRequestID = AttentionRequestID CInt

-- |
--
applicationAttentionRequest :: ApplicationClass self => self -> GtkosxApplicationAttentionType -> IO AttentionRequestID
applicationAttentionRequest self rType = liftM AttentionRequestID $
    {#call unsafe gtkosx_application_attention_request#} (toApplication self) (fromIntegral $ fromEnum rType)

-- |
--
applicationCancelAttentionRequest :: ApplicationClass self => self -> AttentionRequestID -> IO ()
applicationCancelAttentionRequest self (AttentionRequestID id) =
    {#call unsafe gtkosx_application_cancel_attention_request#} (toApplication self) id

-- |
--
applicationGetBundlePath :: GlibString string => IO string
applicationGetBundlePath =
    {#call unsafe gtkosx_application_get_bundle_path#} >>= peekUTFString

-- |
--
applicationGetResourcePath :: GlibString string => IO string
applicationGetResourcePath =
    {#call unsafe gtkosx_application_get_resource_path#} >>= peekUTFString

-- |
--
applicationGetExecutablePath :: GlibString string => IO string
applicationGetExecutablePath =
    {#call unsafe gtkosx_application_get_executable_path#} >>= peekUTFString

-- |
--
applicationGetBundleId :: GlibString string => IO string
applicationGetBundleId =
    {#call unsafe gtkosx_application_get_bundle_id#} >>= peekUTFString

-- |
--
applicationGetBundleInfo :: GlibString string => string -> IO string
applicationGetBundleInfo key =
    withUTFString key $ \keyPtr ->
    {#call unsafe gtkosx_application_get_bundle_info#} keyPtr >>= peekUTFString

-- |
--
didBecomeActive :: ApplicationClass self => Signal self (IO ())
didBecomeActive = Signal (connect_NONE__NONE "NSApplicationDidBecomeActive")

-- |
--
willResignActive :: ApplicationClass self => Signal self (IO ())
willResignActive = Signal (connect_NONE__NONE "NSApplicationWillResignActive")

-- |
--
blockTermination :: ApplicationClass self => Signal self (IO Bool)
blockTermination = Signal (connect_NONE__BOOL "NSApplicationBlockTermination")

-- |
--
willTerminate :: ApplicationClass self => Signal self (IO ())
willTerminate = Signal (connect_NONE__NONE "NSApplicationWillTerminate")

-- |
--
openFile :: (ApplicationClass self, GlibString string) => Signal self (string -> IO ())
openFile = Signal (connect_GLIBSTRING__NONE "NSApplicationOpenFile")

