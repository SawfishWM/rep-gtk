#include <config.h>
#include <gtk/gtk.h>
#include <gdk/gdkx.h>
#include "rep-gtk.h"

/* This whole file is rated XXX. */

gchar*
gtk_label_get_interp (GtkLabel *label)
{
  gchar *str;
  gtk_label_get (label, &str);
  return str;
}

/* cheap cop-out. */

static void
menu_popup_position (GtkMenu *menu, gint *xp, gint *yp, gpointer data)
{
    gulong coded = (gulong) data;
    gint x = coded & 0xffff;
    gint y = coded >> 16;

    /* copied from gtkmenu.c:gtk_menu_position () */

    GtkRequisition requisition;
    gint screen_width = gdk_screen_width ();
    gint screen_height = gdk_screen_height ();

    gtk_widget_size_request (GTK_WIDGET (menu), &requisition);

    x = CLAMP (x - 2, 0, MAX (0, screen_width - requisition.width));
    y = CLAMP (y - 2, 0, MAX (0, screen_height - requisition.height));

    *xp = x;
    *yp = y;
}

void
gtk_menu_popup_interp (GtkMenu *menu,
		       GtkWidget *parent_menu_shell,
		       GtkWidget *parent_menu_item,
		       gint button,
		       guint32 activate_time,
		       repv position)
{
  GtkMenuPositionFunc func = 0;
  gpointer func_data = 0;

  if (rep_CONSP (position)
      && rep_INTP(rep_CAR(position)) && rep_INTP(rep_CDR(position)))
  {
      gulong coded = (rep_INT(rep_CAR(position))
		      | (rep_INT(rep_CDR(position)) << 16));

      func = menu_popup_position;
      func_data = (void *) coded;
  }

  gtk_menu_popup (menu, parent_menu_shell, parent_menu_item,
		  func, func_data, button, activate_time);
}

GtkWidget*
gtk_radio_menu_item_new_with_label_from_widget (GtkRadioMenuItem *group,
						gchar            *label)
{
  GSList *g = group? gtk_radio_menu_item_group (group) : NULL;
  return gtk_radio_menu_item_new_with_label (g, label);
}

GtkWidget*
gtk_radio_menu_item_new_from_widget (GtkRadioMenuItem *group)
{
  GSList *g = group? gtk_radio_menu_item_group (group) : NULL;
  return gtk_radio_menu_item_new (g);
}

GtkWidget*
gtk_pixmap_new_interp (gchar *file,
		       GtkWidget *intended_parent)
{
  GtkStyle *style;
  GdkPixmap *pixmap;
  GdkBitmap *mask;

  style = gtk_widget_get_style (intended_parent);
  pixmap = gdk_pixmap_create_from_xpm (GDK_ROOT_PARENT(), &mask,
				       &style->bg[GTK_STATE_NORMAL],
				       file);
  return gtk_pixmap_new (pixmap, mask);
}

#ifndef HAVE_GDK_COLOR_COPY
/*
 *--------------------------------------------------------------
 * gdk_color_copy
 *
 *   Copy a color structure into new storage.
 *
 * Arguments:
 *   "color" is the color struct to copy.
 *
 * Results:
 *   A new color structure.  Free it with gdk_color_free.
 *
 *--------------------------------------------------------------
 */

static GMemChunk *color_chunk;

GdkColor*
gdk_color_copy (GdkColor *color)
{
  GdkColor *new_color;
  
  g_return_val_if_fail (color != NULL, NULL);

  if (color_chunk == NULL)
    color_chunk = g_mem_chunk_new ("colors",
				   sizeof (GdkColor),
				   4096,
				   G_ALLOC_AND_FREE);

  new_color = g_chunk_new (GdkColor, color_chunk);
  *new_color = *color;
  return new_color;
}

/*
 *--------------------------------------------------------------
 * gdk_color_free
 *
 *   Free a color structure obtained from gdk_color_copy.  Do not use
 *   with other color structures.
 *
 * Arguments:
 *   "color" is the color struct to free.
 *
 *-------------------------------------------------------------- */

void
gdk_color_free (GdkColor *color)
{
  g_assert (color_chunk != NULL);
  g_return_if_fail (color != NULL);

  g_mem_chunk_free (color_chunk, color);
}
#endif

GdkColor*
gdk_color_parse_interp (char *spec)
{
  /* not reentrant */
  static GdkColor color;
  if (!gdk_color_parse (spec, &color))
    return NULL;
  return &color;
}

GdkColor*
gtk_style_get_white_interp (GtkStyle *style)
{
  return &style->white;
}

#ifndef HAVE_GTK_WIDGET_PEEK_COLORMAP
GdkColormap *
gtk_widget_peek_colormap ()
{
  return gtk_widget_get_default_colormap ();
}
#endif

void
gtk_list_append_item (GtkList *list, GtkListItem *item)
{
  GList *items = g_list_alloc ();
  items->data = item;
  gtk_list_append_items (list, items);
}

void
gtk_list_prepend_item (GtkList *list, GtkListItem *item)
{
  GList *items = g_list_alloc ();
  items->data = item;
  gtk_list_prepend_items (list, items);
}

#ifndef HAVE_GTK_TYPE_GET_INFO
gboolean
gtk_type_get_info (GtkType type, GtkTypeInfo *info)
{
  g_warning("Your version of Gtk+ does not support gtk_type_get_info");
  return FALSE;
}
#endif

#ifndef HAVE_GTK_SIGNAL_SET_CLASS_FUNCTION_FULL
void
gtk_signal_set_class_function_full (GtkType            type,
				    const gchar       *signal,
				    GtkSignalFunc      func,
				    GtkCallbackMarshal marshal,
				    gpointer           data,
				    GtkDestroyNotify   destroy_func)
{
  g_warning("Your version of Gtk+ does not support"
	    " gtk_signal_set_class_function_full");
}
#endif

void 
gtk_color_selection_set_color_interp (GtkColorSelection *selection, GdkColor *color)
{
  gdouble vals[3];
  
  vals[0] = color->red / 65535.0; 
  vals[1] = color->green / 65535.0; 
  vals[2] = color->blue / 65535.0; 

  gtk_color_selection_set_color (selection, vals);
}


GdkColor *
gtk_color_selection_get_color_interp (GtkColorSelection *selection)
{
  gdouble vals[3];
  GdkColor dummy, *color;

  gtk_color_selection_get_color (selection, vals);

  /* XXX I don't know if this is a sensible way to obtain a new
     GdkColor */
  color = gdk_color_copy (&dummy);

  /* Since this color is not part of a colormap, the pixel value is
     pointless */
  color->pixel = 0; 
  color->red = (gushort) (65535.0 * vals[0]); 
  color->green = (gushort) (65535.0 * vals[1]); 
  color->blue = (gushort) (65535.0 * vals[2]); 

  return color;
}

void
gtk_widget_draw_interp (GtkWidget *widget)
{
    gtk_widget_draw (widget, NULL);
}
