/* gnome-canvas-support.c -- helper functions for GNOME Canvas binding
   $Id$ */

#include <config.h>
#include <assert.h>
#include <gnome.h>
#include "rep-gtk.h"
#include "rep-gnome.h"
#include <string.h>

DEFSYM(gnome_canvas, "gnome-canvas");

static int
list_length (repv list)
{
    repv len = Flength (list);
    return (len && rep_INTP (len)) ? rep_INT (len) : 0;
}

DEFUN ("gnome-canvas-item-new", Fgnome_canvas_item_new,
       Sgnome_canvas_item_new, (repv p_group, repv type_sym, repv scm_args),
       rep_Subr3)
{
    /* from guile-gnome/gnomeg.c */

  GnomeCanvasItem* cr_ret;
  GnomeCanvasGroup* c_group;

  int n_args;
  sgtk_object_info *info;
  GtkArg *args;

  rep_DECLARE (1, p_group, sgtk_is_a_gtkobj (gnome_canvas_group_get_type (), p_group));
  rep_DECLARE (2, type_sym, rep_SYMBOLP(type_sym));
  n_args = list_length (scm_args);
  rep_DECLARE (3, scm_args, n_args >= 0 && (n_args%2) == 0);
  n_args = n_args/2;

  info = sgtk_find_object_info (rep_STR(rep_SYM(type_sym)->name));
  rep_DECLARE (2, type_sym, info != NULL);

  c_group = (GnomeCanvasGroup*)sgtk_get_gtkobj (p_group);
  args = sgtk_build_args (info, &n_args, scm_args, Qnil,
			  (char *) &Sgnome_canvas_item_new);
  cr_ret = gnome_canvas_item_newv (c_group, info->header.type, n_args, args);
  g_free (args);

  return sgtk_wrap_gtkobj ((GtkObject*)cr_ret);
}

DEFUN ("gnome-canvas-item-set", Fgnome_canvas_item_set,
       Sgnome_canvas_item_set, (repv p_item, repv scm_args), rep_Subr2)
{
    /* from guile-gnome/gnomeg.c */

  GnomeCanvasItem* c_item;
  int n_args;
  sgtk_object_info *info;
  GtkArg *args;

  rep_DECLARE (1, p_item, sgtk_is_a_gtkobj (gnome_canvas_item_get_type (), p_item));
  n_args = list_length (scm_args);
  rep_DECLARE (2, scm_args, n_args >= 0 && (n_args%2) == 0);
  n_args = n_args/2;

  c_item = (GnomeCanvasItem*)sgtk_get_gtkobj (p_item);
  info = sgtk_find_object_info_from_type (GTK_OBJECT_TYPE(c_item));
  rep_DECLARE (1, p_item, info != NULL);
  
  args = sgtk_build_args (info, &n_args, scm_args, p_item,
			  (char *) &Sgnome_canvas_item_set);
  gnome_canvas_item_setv (c_item, n_args, args);
  g_free (args);

  return Qnil;
}

GnomeCanvasPoints *
sgtk_gnome_canvas_points_new (repv data)
{
    repv len = Flength (data);
    if (len && rep_INT (len) % 2 == 0)
    {
	int i, count = rep_INT (len);
	GnomeCanvasPoints *p = gnome_canvas_points_new (count / 2);
	if (rep_CONSP (data))
	{
	    for (i = 0; i < count; i++)
	    {
		p->coords[i] = sgtk_rep_to_double (rep_CAR (data));
		data = rep_CDR (data);
	    }
	}
	else if (rep_VECTORP (data))
	{
	    for (i = 0; i < count; i++)
		p->coords[i] = sgtk_rep_to_double (rep_VECTI (data, i));
	}
	return p;
    }
    else
	return 0;
}

repv
sgtk_gnome_canvas_points_conversion (repv arg)
{
    extern repv Fgnome_canvas_points_new (repv);

    if (rep_LISTP (arg) || rep_VECTORP (arg))
	return Fgnome_canvas_points_new (arg);
    else
	return arg;
}


/* dl hooks / init */

repv
rep_dl_init (void)
{
#if rep_INTERFACE >= 9
    repv s = rep_push_structure ("gnome-canvas");
#endif

    sgtk_gnome_init_gnome_canvas_glue ();
    rep_ADD_SUBR (Sgnome_canvas_item_new);
    rep_ADD_SUBR (Sgnome_canvas_item_set);

#if rep_INTERFACE >= 9
    return rep_pop_structure (s);
#else
    rep_INTERN(gnome_canvas);
    return Qgnome_canvas;
#endif
}
