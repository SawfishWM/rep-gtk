/*
 * Copyright (C) 1997, 1998, 1999 Marius Vollmer
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "rep-gtk.h"
#include <string.h>
#include <gdk/gdkprivate.h>

/* It is not strictly correct to have Gdk support functions here.  But
   as long as we do not want to have some SCM_PROCs for the (gdk gdk)
   module, we are safe. */



GdkColor *
gdk_color_intern (GdkColor *color)
{
  return color;
}

GdkFont *
gdk_font_intern (GdkFont *font)
{
  return font;
}

GdkGC *
gtk_style_fg_gc (GtkStyle *style, GtkStateType state)
{
  if (style == NULL || state < 0 || state >= 5)
    return NULL;

  return style->fg_gc[state];
}

GdkGC *
gtk_style_bg_gc (GtkStyle *style, GtkStateType state)
{
  if (style == NULL || state < 0 || state >= 5)
    return NULL;

  return style->bg_gc[state];
}

/* Event destructuring */

GdkEventType
gdk_event_type (GdkEvent *event)
{
  return event->any.type;
}

GdkWindow *
gdk_event_window (GdkEvent *event)
{
  return event->any.window;
}

gboolean
gdk_event_send_event (GdkEvent *event)
{
  return event->any.send_event;
}

GdkRectangle *
gdk_event_area (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_EXPOSE:
      return &event->expose.area;
    default:
      return NULL;
    }
}

GdkVisibilityState
gdk_event_visibility_state (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_VISIBILITY_NOTIFY:
      return event->visibility.state;
    default:
      return GDK_VISIBILITY_UNOBSCURED; /* XXX */
    }
}

guint32
gdk_event_time (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.time;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.time;
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      return event->key.time;
      return event->key.time;
    case GDK_PROPERTY_NOTIFY:
      return event->property.time;
    case GDK_SELECTION_CLEAR:
    case GDK_SELECTION_REQUEST:
    case GDK_SELECTION_NOTIFY:
      return event->selection.time;
    case GDK_PROXIMITY_IN:
    case GDK_PROXIMITY_OUT:
      return event->proximity.time;
    default:
      return 0;
    }
}

gdouble
gdk_event_x (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.x;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.x;
    default:
      return 0;
    }
}

gdouble
gdk_event_y (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.y;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.y;
    default:
      return 0;
    }
}

gdouble
gdk_event_pressure (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.pressure;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.pressure;
    default:
      return 0;
    }
}

gdouble
gdk_event_xtilt (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.xtilt;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.xtilt;
    default:
      return 0;
    }
}

gdouble
gdk_event_ytilt (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.ytilt;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.ytilt;
    default:
      return 0;
    }
}

gint
gdk_event_button (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.button;
    default:
      return 0;
    }
}

gboolean
gdk_event_button_state (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.state;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.state;
    default:
      return 0;
    }
}

gboolean
gdk_event_is_hint (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.is_hint;
    default:
      return 0;
    }
}

GdkInputSource
gdk_event_source (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.source;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.source;
    default:
      return GDK_SOURCE_MOUSE; /* XXX */
    }
}

guint32
gdk_event_deviceid (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.deviceid;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.deviceid;
    default:
      return 0;
    }
}

gdouble
gdk_event_x_root (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.x_root;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.x_root;
    default:
      return 0;
    }
}

gdouble
gdk_event_y_root (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_MOTION_NOTIFY:
      return event->motion.y_root;
    case GDK_BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
      return event->button.y_root;
    default:
      return 0;
    }
}

gboolean
gdk_event_key_state (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      return event->key.state;
    default:
      return 0;
    }
}

guint
gdk_event_keyval (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      return event->key.keyval;
    default:
      return 0;
    }
}

gchar *
gdk_event_string (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_KEY_PRESS:
    case GDK_KEY_RELEASE:
      {
	gchar *str = g_malloc (event->key.length+1);
	strncpy (str, event->key.string, event->key.length);
	str[event->key.length] = '\0';
	return str;
      }
    default:
      return NULL;
    }
}

GdkWindow *
gdk_event_subwindow (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.subwindow;
    default:
      return 0;
    }
}

GdkNotifyType
gdk_event_notify_detail (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return event->crossing.detail;
    default:
      return 0;
    }
}

gboolean
gdk_event_in (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_FOCUS_CHANGE:
      return event->focus_change.in;
    default:
      return 0;
    }
}

gint16
gdk_event_configure_x (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_CONFIGURE:
      return event->configure.x;
    default:
      return 0;
    }
}

gint16
gdk_event_configure_y (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_CONFIGURE:
      return event->configure.y;
    default:
      return 0;
    }
}

gint16
gdk_event_configure_width (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_CONFIGURE:
      return event->configure.width;
    default:
      return 0;
    }
}

guint32
gdk_get_leader_window_id ()
{
  return (guint32) gdk_leader_window;
}

gint16
gdk_event_configure_height (GdkEvent *event)
{
  switch (event->any.type)
    {
    case GDK_CONFIGURE:
      return event->configure.height;
    default:
      return 0;
    }
}

guint32
gdk_window_xid (GdkWindow *win)
{
    GdkWindowPrivate *pri = (GdkWindowPrivate *)win;
    return pri->xwindow;
}



repv
sgtk_gtk_widget_get_allocation (GtkWidget *w)
{
    return Fcons (Fcons (rep_MAKE_INT (w->allocation.x),
			 rep_MAKE_INT (w->allocation.y)),
		  Fcons (rep_MAKE_INT (w->allocation.width),
			 rep_MAKE_INT (w->allocation.height)));
}
