#ifndef REP_GTK_REP_GNOME_H
#define REP_GTK_REP_GNOME_H

#include <rep.h>
#include <gnome.h>

extern int sgtk_gnome_init (const char *app_id, const char *app_version);

extern void sgtk_gnome_init_gnome_glue (void);
extern void sgtk_gnome_init_gnomeui_glue (void);
extern void sgtk_gnome_init_gnome_canvas_glue (void);

extern GnomeMetadataError_t
sgtk_gnome_metadata_set (const char *file, const char *name, repv data);
extern repv sgtk_gnome_metadata_get (const char *file, const char *name);
extern repv sgtk_gnome_metadata_get_fast (const char *file, const char *name);
extern void sgtk_gnome_metadata_regex_add (const char *regex, const char *key, repv data);
extern void sgtk_gnome_metadata_type_add (const char *regex, const char *key, repv data);

extern guint gnome_app_bar_get_type (void);
extern void sgtk_gnome_string_callback (gchar *string, gpointer data);
extern void sgtk_gnome_reply_callback (gint reply, gpointer data);

GnomeCanvasPoints *sgtk_gnome_canvas_points_new (repv data);
repv sgtk_gnome_canvas_points_conversion (repv arg);

#endif
