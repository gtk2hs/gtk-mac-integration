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
  applicationCleanup,
  applicationSetUseQuartsAccelerators,
  applicationSetMenuBar,
  applicationSyncMenuBar,
  ApplicationMenuGroup(..),
  applicationAddAppMenuGroup,
  applicationAddAppMenuItem,
  applicationSetWindowMenu,
  applicationSetHelpMenu,
  GtkOSXApplicationAttentionType(..),
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
    {#call unsafe gtk_osxapplication_ready#} (toApplication self)

-- |
--
applicationCleanup :: ApplicationClass self => self -> IO ()
applicationCleanup self =
    {#call unsafe gtk_osxapplication_cleanup#} (toApplication self)

-- |
--
applicationSetUseQuartsAccelerators :: ApplicationClass self => self -> Bool -> IO ()
applicationSetUseQuartsAccelerators self b =
    {#call unsafe gtk_osxapplication_set_use_quartz_accelerators#} (toApplication self) (fromBool b)

-- |
--
applicationGetUseQuartsAccelerators :: ApplicationClass self => self -> IO Bool
applicationGetUseQuartsAccelerators self = liftM toBool $
    {#call unsafe gtk_osxapplication_use_quartz_accelerators#} (toApplication self)

-- |
--
applicationSetMenuBar :: (ApplicationClass self, MenuShellClass menu) => self -> menu -> IO ()
applicationSetMenuBar self menu =
    {#call unsafe gtk_osxapplication_set_menu_bar#} (toApplication self) (toMenuShell menu)

-- |
--
applicationSyncMenuBar :: ApplicationClass self => self -> IO ()
applicationSyncMenuBar self =
    {#call unsafe gtk_osxapplication_sync_menubar#} (toApplication self)

newtype ApplicationMenuGroup = GtkOSXApplicationMenuGroup (Ptr ())

-- |
--
applicationAddAppMenuGroup :: ApplicationClass self => self -> IO ApplicationMenuGroup
applicationAddAppMenuGroup self = liftM GtkOSXApplicationMenuGroup $
    {#call unsafe gtk_osxapplication_add_app_menu_group#} (toApplication self)

-- |
--
applicationAddAppMenuItem :: (ApplicationClass self, MenuItemClass menuItem)
    => self -> ApplicationMenuGroup -> menuItem -> IO ()
applicationAddAppMenuItem self (GtkOSXApplicationMenuGroup group) menuItem =
    {#call unsafe gtk_osxapplication_add_app_menu_item#} (toApplication self)
        group (toMenuItem menuItem)

-- |
--
applicationSetWindowMenu :: (ApplicationClass self, MenuItemClass menuItem)
    => self -> menuItem -> IO ()
applicationSetWindowMenu self menuItem =
    {#call unsafe gtk_osxapplication_set_window_menu#} (toApplication self) (toMenuItem menuItem)

-- |
--
applicationSetHelpMenu :: (ApplicationClass self, MenuItemClass menuItem)
    => self -> menuItem -> IO ()
applicationSetHelpMenu self menuItem =
    {#call unsafe gtk_osxapplication_set_help_menu#} (toApplication self) (toMenuItem menuItem)

{#enum GtkOSXApplicationAttentionType {underscoreToCase} deriving (Eq,Show)#}

-- |
--
applicationSetDockMenu :: (ApplicationClass self, MenuShellClass menu) => self -> menu -> IO ()
applicationSetDockMenu self menu =
    {#call unsafe gtk_osxapplication_set_dock_menu#} (toApplication self) (toMenuShell menu)

-- |
--
applicationSetDockIconPixbuf :: (ApplicationClass self, PixbufClass pixbuf) => self -> Maybe pixbuf -> IO ()
applicationSetDockIconPixbuf self mbPixbuf =
    {#call unsafe gtk_osxapplication_set_dock_icon_pixbuf#} (toApplication self)
        (maybe (Pixbuf nullForeignPtr) toPixbuf mbPixbuf)

-- |
--
applicationSetDockIconResource :: ApplicationClass self => self -> String -> String -> String -> IO ()
applicationSetDockIconResource self name rType subdir =
    withCString name $ \namePtr ->
    withCString rType $ \typePtr ->
    withCString subdir $ \subdirPtr ->
    {#call unsafe gtk_osxapplication_set_dock_icon_resource#} (toApplication self) namePtr typePtr subdirPtr

newtype AttentionRequestID = AttentionRequestID CInt

-- |
--
applicationAttentionRequest :: ApplicationClass self => self -> GtkOSXApplicationAttentionType -> IO AttentionRequestID
applicationAttentionRequest self rType = liftM AttentionRequestID $
    {#call unsafe gtk_osxapplication_attention_request#} (toApplication self) (fromIntegral $ fromEnum rType)

-- |
--
applicationCancelAttentionRequest :: ApplicationClass self => self -> AttentionRequestID -> IO ()
applicationCancelAttentionRequest self (AttentionRequestID id) =
    {#call unsafe gtk_osxapplication_cancel_attention_request#} (toApplication self) id

-- |
--
applicationGetBundlePath :: ApplicationClass self => self -> IO String
applicationGetBundlePath self =
    {#call unsafe gtk_osxapplication_get_bundle_path#} (toApplication self) >>= peekCString

-- |
--
applicationGetResourcePath :: ApplicationClass self => self -> IO String
applicationGetResourcePath self =
    {#call unsafe gtk_osxapplication_get_resource_path#} (toApplication self) >>= peekCString

-- |
--
applicationGetExecutablePath :: ApplicationClass self => self -> IO String
applicationGetExecutablePath self =
    {#call unsafe gtk_osxapplication_get_executable_path#} (toApplication self) >>= peekCString

-- |
--
applicationGetBundleId :: ApplicationClass self => self -> IO String
applicationGetBundleId self =
    {#call unsafe gtk_osxapplication_get_bundle_id#} (toApplication self) >>= peekCString

-- |
--
applicationGetBundleInfo :: ApplicationClass self => self -> String -> IO String
applicationGetBundleInfo self key =
    withCString key $ \keyPtr ->
    {#call unsafe gtk_osxapplication_get_bundle_info#} (toApplication self) keyPtr >>= peekCString

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
openFile :: ApplicationClass self => Signal self (String -> IO ())
openFile = Signal (connect_STRING__NONE "NSApplicationOpenFile")

