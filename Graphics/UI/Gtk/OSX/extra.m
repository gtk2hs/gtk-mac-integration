#include <gtk/gtk.h>
#include <gdk/gdkquartz.h>

void gtk2hs_osx_allow_fullscreen(GdkWindow *wnd)
{
    if (wnd != NULL)
    {
        NSWindow *native = gdk_quartz_window_get_nswindow (wnd);
        [native setCollectionBehavior: [native collectionBehavior] | NSWindowCollectionBehaviorFullScreenPrimary];
    }
}
