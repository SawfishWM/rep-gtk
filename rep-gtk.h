/* Copyright (C) 1997, 1998, 1999 Marius Vollmer
 * Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
 *
 * $Id$
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
 * the Free Software Foundation,51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 */

#ifndef REP_GTK_H
#define REP_GTK_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <rep/rep.h>
#include <gtk/gtk.h>

typedef struct _sgtk_type_info {
  char *name;
  GType type;
  repv (*conversion) (repv);
} sgtk_type_info;

typedef struct _sgtk_enum_literal {
  char *name;
  int value;
} sgtk_enum_literal;

typedef struct _sgtk_enum_info {
  sgtk_type_info header;
  int n_literals;
  sgtk_enum_literal *literals;
} sgtk_enum_info;

/* This is like an _sgtk_enum_literal, but the values are strings.
   This is used in Gnome.  */
typedef struct _sgtk_senum_literal {
  char *name;
  char *value;
} sgtk_senum_literal;

typedef struct _sgtk_senum_info {
  sgtk_type_info header;
  int n_literals;
  sgtk_senum_literal *literals;
} sgtk_senum_info;

typedef struct _sgtk_boxed_info {
  sgtk_type_info header;
  void *(*copy) (void *);
  void (*destroy) (void *);
  size_t size;
} sgtk_boxed_info;

typedef struct _sgtk_object_info {
  sgtk_type_info header;
  GType (*init_func) ();

  struct _sgtk_object_info *parent;
} sgtk_object_info;

void sgtk_register_type_infos (sgtk_type_info **infos);
sgtk_type_info *sgtk_get_type_info (GType type_seqno);
void sgtk_register_type_infos_gtk (GTypeInfo **infos);
sgtk_type_info* sgtk_maybe_find_type_info (GType type);
sgtk_type_info *sgtk_find_type_info (GType type);

int sgtk_valid_int (repv obj);
int sgtk_valid_uint (repv obj);
int sgtk_valid_long (repv obj);
int sgtk_valid_ulong (repv obj);
int sgtk_valid_char (repv obj);
repv sgtk_uint_to_rep (unsigned long x);
repv sgtk_int_to_rep (long x);
repv sgtk_long_to_rep (long x);
repv sgtk_ulong_to_rep (unsigned long x);
guint sgtk_rep_to_uint (repv obj);
gint sgtk_rep_to_int (repv obj);
gulong sgtk_rep_to_ulong (repv obj);
glong sgtk_rep_to_long (repv obj);
gchar sgtk_rep_to_char (repv obj);
repv sgtk_char_to_rep (gchar c);
char *sgtk_rep_to_string (repv obj);
repv sgtk_string_to_rep (char *x);
repv sgtk_static_string_to_rep (const char *x);
int sgtk_valid_string (repv obj);
repv sgtk_bool_to_rep (int x);
int sgtk_rep_to_bool (repv obj);
int sgtk_valid_function (repv obj);
int sgtk_valid_fd (repv obj);
int sgtk_rep_to_fd (repv obj);
repv sgtk_fd_to_rep (int fd);

repv sgtk_wrap_gobj (GObject *obj);
int sgtk_is_a_gobj (GType type, repv obj);
GObject *sgtk_get_gobj (repv obj);

/* compatibility */
repv sgtk_wrap_gtkobj (GtkObject *obj);
int sgtk_is_a_gtkobj (GType type, repv obj);
GtkObject *sgtk_get_gtkobj (repv obj);

int sgtk_valid_enum (repv obj, sgtk_enum_info*);
repv sgtk_enum_to_rep (gint val, sgtk_enum_info*);
gint sgtk_rep_to_enum (repv obj, sgtk_enum_info*);

int sgtk_valid_flags (repv obj, sgtk_enum_info*);
repv sgtk_flags_to_rep (gint val, sgtk_enum_info*);
gint sgtk_rep_to_flags (repv obj, sgtk_enum_info*);

int sgtk_valid_senum (repv obj, sgtk_senum_info*);
repv sgtk_senum_to_rep (char *val, sgtk_senum_info*);
char *sgtk_rep_to_senum (repv obj, sgtk_senum_info*);

repv sgtk_boxed_to_rep (gpointer ptr, sgtk_boxed_info*, int copyp);
void *sgtk_rep_to_boxed (repv obj);
int sgtk_valid_boxed (repv obj, sgtk_boxed_info*);

int sgtk_valid_float (repv obj);
gfloat sgtk_rep_to_float (repv obj);
repv sgtk_float_to_rep (gfloat f);

int sgtk_valid_double (repv obj);
double sgtk_rep_to_double (repv obj);
repv sgtk_double_to_rep (double f);

int sgtk_valid_pointer (repv obj);
void *sgtk_rep_to_pointer (repv obj);
repv sgtk_pointer_to_rep (void *ptr);

GType gobject_get_type (void);

int sgtk_valid_point (repv obj);
GdkPoint sgtk_rep_to_point (repv obj);
repv sgtk_point_to_rep (GdkPoint p);

int sgtk_valid_rect (repv obj);
GdkRectangle sgtk_rep_to_rect (repv obj);
repv sgtk_rect_to_rep (GdkRectangle p);

int sgtk_valid_segment (repv obj);
GdkSegment sgtk_scm2segment (repv obj);
repv sgtk_segment2scm (GdkSegment seg);

GType sgtk_type_from_name (char *name);
int sgtk_valid_type (repv obj);
GType sgtk_rep_to_type (repv obj);
repv sgtk_type_to_rep (GType t);

int sgtk_valid_composite (repv obj, int (*predicate)(repv));
int sgtk_valid_complen (repv obj, int (*predicate)(repv), int len);

repv sgtk_slist_to_rep (GSList *list, repv (*torep)(void*));
GSList *sgtk_rep_to_slist (repv obj, void (*fromrep)(repv, void*));
void sgtk_slist_finish (GSList *list, repv obj, repv (*torep)(void*));

repv sgtk_list_to_rep (GList *list, repv (*torep)(void*));
GList *sgtk_rep_to_list (repv obj, void (*fromrep)(repv, void*));
void sgtk_list_finish (GList *list, repv obj, repv (*torep)(void*));

typedef struct {
  int count;
  void *vec;
} sgtk_cvec;

sgtk_cvec sgtk_rep_to_cvec (repv obj, void (*fromrep)(repv, void*), size_t sz);
void sgtk_cvec_finish (sgtk_cvec *, repv obj, repv (*torep)(void*), size_t sz);
repv sgtk_cvec_to_rep (sgtk_cvec *cvec, repv (*toscm)(void *), size_t sz);

typedef struct sgtk_protshell sgtk_protshell;

void sgtk_set_protect (repv protector, sgtk_protshell *prot);
sgtk_protshell *sgtk_new_protect (repv obj);
sgtk_protshell *sgtk_protect (repv protector, repv obj);
void sgtk_unprotect (sgtk_protshell *);
repv sgtk_get_protect (sgtk_protshell *prot);

void sgtk_set_gclosure (repv protector, GClosure *closure);
repv sgtk_get_gclosure (GClosure *closure);
GClosure *sgtk_new_gclosure (repv obj);
GClosure *sgtk_gclosure (repv protector, repv obj);

void sgtk_gclosure_callback_marshal (GClosure *closure,
				     GValue *return_value,
				     guint n_param_values,
				     const GValue *param_values,
				     gpointer invocation_hint,
				     gpointer marshal_data);
void sgtk_gclosure_callback_destroy (gpointer data, GClosure *closure);

void sgtk_callback_marshal (GtkObject *obj,
			    gpointer data,
			    guint n_args,
			    GtkArg *args);
void sgtk_callback_destroy (gpointer data);

repv sgtk_callback_trampoline (repv new_trampoline);
void sgtk_callback_postfix (void);

int sgtk_valid_arg_type (GType, repv val);
repv sgtk_arg_to_rep (GtkArg *a, int free_mem);
void sgtk_rep_to_arg (GtkArg *a, repv obj, repv protector);
void sgtk_rep_to_ret (GtkArg *a, repv obj);

repv sgtk_gvalue_to_rep (const GValue *a);
int sgtk_valid_gvalue (const GValue *a, repv obj);
void sgtk_rep_to_gvalue (GValue *a, repv obj);

sgtk_object_info *sgtk_find_object_info_from_type (GType type);
sgtk_object_info *sgtk_find_object_info (const char *name);
void sgtk_free_args (GParameter *args, int n_args);
GParameter *sgtk_build_args (GObjectClass *objclass, int *n_argsp,
			     repv rep_args, char *subr);

repv sgtk_color_conversion (repv color);
repv sgtk_font_conversion (repv color);

void sgtk_throw_gerror (const char *func_name, GError *gerr);

void sgtk_set_standalone (int flag);
int sgtk_is_standalone (void);
repv sgtk_standalone_p (void);

void sgtk_init (void);
void sgtk_init_with_args (int *argcp, char ***argvp);

/* Additional useful Gdk routines. */

/* The following two do their magic with type conversions that are
   automatically generated by build-guile-gtk. */

GdkColor *gdk_color_intern (GdkColor *);
GdkFont *gdk_font_intern (GdkFont *);

GdkEventType gdk_event_type (GdkEvent *event);
GdkWindow *gdk_event_window (GdkEvent *event);
gboolean gdk_event_send_event (GdkEvent *event);
GdkRectangle *gdk_event_area (GdkEvent *event);
GdkVisibilityState gdk_event_visibility_state (GdkEvent *event);
guint32 gdk_event_time (GdkEvent *event);
gdouble gdk_event_x (GdkEvent *event);
gdouble gdk_event_y (GdkEvent *event);
gdouble gdk_event_pressure (GdkEvent *event);
gdouble gdk_event_xtilt (GdkEvent *event);
gdouble gdk_event_ytilt (GdkEvent *event);
gint gdk_event_button (GdkEvent *event);
gboolean gdk_event_button_state (GdkEvent *event);
gboolean gdk_event_is_hint (GdkEvent *event);
GdkInputSource gdk_event_source (GdkEvent *event);
guint32 gdk_event_deviceid (GdkEvent *event);
gdouble gdk_event_x_root (GdkEvent *event);
gdouble gdk_event_y_root (GdkEvent *event);
gboolean gdk_event_key_state (GdkEvent *event);
guint gdk_event_keyval (GdkEvent *event);
gchar *gdk_event_string (GdkEvent *event);
GdkWindow *gdk_event_subwindow (GdkEvent *event);
GdkNotifyType gdk_event_notify_detail (GdkEvent *event);
gboolean gdk_event_in (GdkEvent *event);
gint16 gdk_event_configure_x (GdkEvent *event);
gint16 gdk_event_configure_y (GdkEvent *event);
gint16 gdk_event_configure_width (GdkEvent *event);
gint16 gdk_event_configure_height (GdkEvent *event);

guint32 gdk_get_leader_window_id (void);

guint32 gdk_window_xid (GdkWindow *win);

GdkGC *gtk_style_fg_gc (GtkStyle *style, GtkStateType state);
GdkGC *gtk_style_bg_gc (GtkStyle *style, GtkStateType state);

/* Gtk stuff that wouldn't be here in an ideal world. */

gchar *gtk_label_get_interp (GtkLabel *label);
void gtk_menu_popup_interp (GtkMenu *menu,
			    GtkWidget *parent_menu_shell,
			    GtkWidget *parent_menu_item,
			    gint button,
			    guint32 activate_time,
			    repv position);

#if GTK_MAJOR_VERSION < 2 || (GTK_MAJOR_VERSION == 2 && GTK_MINOR_VERSION < 4)
GtkWidget*
gtk_radio_menu_item_new_with_label_from_widget (GtkRadioMenuItem *group,
		                                                const gchar      *label);
GtkWidget*
gtk_radio_menu_item_new_with_mnemonic_from_widget (GtkRadioMenuItem *group,
		                                                   const gchar      *label);
GtkWidget* gtk_radio_menu_item_new_from_widget (GtkRadioMenuItem *group);
#endif

GtkWidget* gtk_pixmap_new_interp (char *file, GtkWidget *intended_parent);

GdkColor *gdk_color_parse_interp (char *spec);
GdkColor *gtk_style_get_white_interp (GtkStyle *style);

#ifndef HAVE_GTK_WIDGET_PEEK_COLORMAP
GdkColormap *gtk_widget_peek_colormap (void);
#endif

void gtk_list_append_item (GtkList *list, GtkListItem *item);
void gtk_list_prepend_item (GtkList *list, GtkListItem *item);

#ifndef HAVE_GTK_TYPE_GET_INFO
gboolean gtk_type_get_info (GtkType type, GtkTypeInfo *info);
#endif

GtkType gtk_class_new (GtkType parent_type, gchar *name);
guint
gtk_signal_new_generic (const gchar     *name,
			GtkSignalRunType signal_flags,
			GtkType          type,
			GtkType          return_type,
			guint            nparams,
			GtkType         *params);
void sgtk_signal_emit (GtkObject *obj, char *name, repv rep_args);

#ifndef HAVE_GTK_SIGNAL_SET_CLASS_FUNCTION_FULL
void gtk_signal_set_class_function_full (GtkType            type,
					 const gchar       *signal,
					 GtkSignalFunc      func,
					 GtkCallbackMarshal marshal,
					 gpointer           data,
					 GtkDestroyNotify   destroy_func);
#endif

void gtk_color_selection_set_color_interp (GtkColorSelection *sel, GdkColor *color);
GdkColor *gtk_color_selection_get_color_interp (GtkColorSelection *sel);
char *gtk_color_button_get_color_interp (GtkColorButton *button);
extern void gtk_widget_draw_interp (GtkWidget *widget);

GtkTextIter *gtk_text_iter_new (void);
repv sgtk_gtk_widget_get_allocation (GtkWidget *w);

extern rep_xsubr *sgtk_subrs[];
extern sgtk_type_info *sgtk_type_infos[];

#endif /* !REP_GTK_H */
