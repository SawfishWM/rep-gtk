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
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <gdk/gdkprivate.h>
#include "rep-gtk.h"
#include <string.h>

/* Define this to enable some output during GC and other interesting
   actions. */
#undef DEBUG_PRINT



/* Associating SCM values with Gtk pointers.

   We keep a hash table that can store a SCM value for an arbitray
   gpointer.  This is used for the proxies of GtkObjects and the boxed
   types.  */

static GHashTable *proxy_tab;

static guint
gpointer_hash (gpointer a)
{
  return (guint)a;
}

static gint
gpointer_compare (gpointer a, gpointer b)
{
  return a == b;
}

static void
enter_proxy (gpointer obj, repv proxy)
{
  if (proxy_tab == NULL)
    proxy_tab = g_hash_table_new ((GHashFunc)gpointer_hash,
				  (GCompareFunc)gpointer_compare);
  g_hash_table_insert (proxy_tab, obj, (gpointer)proxy);
}

static repv
get_proxy (gpointer obj)
{
  if (proxy_tab)
    {
      gpointer val = g_hash_table_lookup (proxy_tab, obj);
      return val? (repv) val : Qnil;
    }
  return Qnil;
}

static void
forget_proxy (gpointer obj)
{
  g_hash_table_remove (proxy_tab, obj);
}



/* Hacking the basic types --jsh */

int
sgtk_valid_int (repv obj)
{
    return rep_INTP (obj);
}

int
sgtk_valid_uint (repv obj)
{
    return rep_INTP (obj);
}

int
sgtk_valid_long (repv obj)
{
    return rep_INTP (obj);
}

int
sgtk_valid_ulong (repv obj)
{
    return rep_INTP (obj);
}

int
sgtk_valid_char (repv obj)
{
    return rep_INTP (obj);
}

repv
sgtk_uint_to_rep (u_long x)
{
    assert (x <= rep_LISP_MAX_INT);
    return rep_MAKE_INT (x);
}

repv
sgtk_int_to_rep (long x)
{
    assert (x <= rep_LISP_MAX_INT && x >= rep_LISP_MIN_INT);
    return rep_MAKE_INT (x);
}

guint
sgtk_rep_to_uint (repv obj)
{
    return rep_INT (obj);
}

gint
sgtk_rep_to_int (repv obj)
{
    return rep_INT (obj);
}

gulong
sgtk_rep_to_ulong (repv obj)
{
    return rep_INT (obj);
}

glong
sgtk_rep_to_long (repv obj)
{
    return rep_INT (obj);
}

gchar
sgtk_rep_to_char (repv obj)
{
    return rep_INT (obj);
}

char *
sgtk_rep_to_string (repv obj)
{
    return rep_STRINGP (obj) ? rep_STR (obj) : (u_char *)"";
}

repv
sgtk_string_to_rep (char *x)
{
    repv obj;
    if (x == 0)
	return Qnil;
    obj = rep_string_dup (x);
    g_free (x);
    return obj;
}

repv
sgtk_static_string_to_rep (char *x)
{
    repv obj;
    if (x == 0)
	return Qnil;
    obj = rep_string_dup (x);
    return obj;
}

int
sgtk_valid_string (repv obj)
{
    return rep_STRINGP (obj);
}

repv
sgtk_bool_to_rep (int x)
{
    return x ? Qt : Qnil;
}

int
sgtk_rep_to_bool (repv obj)
{
    return obj != Qnil;
}

int
sgtk_valid_function (repv obj)
{
    return Ffunctionp (obj) != Qnil;
}

int
sgtk_valid_fd (repv obj)
{
    return rep_FILEP (obj) && rep_LOCAL_FILE_P (obj);
}

int
sgtk_rep_to_fd (repv obj)
{
    return fileno (rep_FILE(obj)->file.fh);
}

static int
list_length (repv list)
{
    repv len = Flength (list);
    return (len && rep_INTP (len)) ? rep_INT (len) : 0;
}



/* Storing additional info about a GtkType.

   Each GtkType has a unique sequence number.  We use that to simply
   index an array of sgtk_type_info pointers.  The array is grown
   dynamically when necessary. */

#define TYPE_INFO_INCR_MASK 0xFF

static sgtk_type_info **type_info_tab;
static guint n_type_info_tab = 0;

static void
enter_type_info (sgtk_type_info *info)
{
  guint seqno = GTK_TYPE_SEQNO (info->type);

  if (seqno >= n_type_info_tab)
    {
      guint i, new_size = (seqno+TYPE_INFO_INCR_MASK)&(~TYPE_INFO_INCR_MASK);
      type_info_tab = (sgtk_type_info **)
			rep_realloc ((char *)type_info_tab,
				     sizeof(sgtk_type_info*) * new_size);
      for (i = n_type_info_tab; i < new_size; i++)
	type_info_tab[i] = NULL;
      n_type_info_tab = new_size;
    }

  type_info_tab[seqno] = info;
}

sgtk_type_info*
sgtk_get_type_info (guint seqno)
{
  if (seqno >= n_type_info_tab)
    return NULL;
  return type_info_tab[seqno];
}

static sgtk_type_info*
must_get_type_info (guint seqno)
{
  sgtk_type_info *info = sgtk_get_type_info (seqno);
  if (info == NULL)
    abort ();
  return info;
}

typedef struct _type_infos {
  struct _type_infos *next;
  sgtk_type_info **infos;
} type_infos;

static type_infos *all_type_infos;

/* Finds types that are mentioned in our *.defs files but are not
   provided by the Gtk run-time system.  This is only used
   occasionally to update the table in sgtk_try_missing_type.  */
#ifdef NEED_UNUSED_CODE
static void
sgtk_find_missing_types (type_infos *infos)
{
  sgtk_type_info **ip;
  for (ip = infos->infos; *ip; ip++)
    {
      if (gtk_type_from_name ((*ip)->name) == GTK_TYPE_INVALID
	  && (*ip)->type != GTK_TYPE_OBJECT)
	printf ("missing: %s, %s\n",
		(*ip)->name, gtk_type_name ((*ip)->type));
    }
}
#endif

void
sgtk_register_type_infos (sgtk_type_info **infos)
{
  type_infos *t;

  sgtk_init ();

  t = (type_infos *) rep_alloc (sizeof(type_infos));
  t->infos = infos;
  t->next = all_type_infos;
  all_type_infos = t;

#if 0
  sgtk_find_missing_types (t);
#endif
}

/* When INFO refers to one of the known `missing' types, we initialize
   that type ourselves.  This is used to fix certain discrepancies
   between old Gtk versions and our *.defs files.  It is not OK to do
   this in general because we should not assume that we can safely
   initialize types from other modules.  */

static GtkType
sgtk_try_missing_type (char *name)
{
  static sgtk_type_info missing[] = {
    { "GdkGC", GTK_TYPE_BOXED },
    { "GtkToolbarStyle", GTK_TYPE_ENUM },
    { "GtkToolbarChildType", GTK_TYPE_ENUM },
    { "GtkTreeViewMode", GTK_TYPE_ENUM },
    { "GtkSpinButtonUpdatePolicy", GTK_TYPE_ENUM },
    { "GtkCellType", GTK_TYPE_ENUM },
    { "GdkOverlapType", GTK_TYPE_ENUM },
    { "GdkWMDecoration", GTK_TYPE_FLAGS },
    { "GdkWMFunction", GTK_TYPE_FLAGS },
    { "GdkVisibilityState", GTK_TYPE_ENUM },
    { "GdkInputSource", GTK_TYPE_ENUM },
    {NULL, GTK_TYPE_NONE}
  };

  sgtk_type_info *m;
  for (m = missing; m->name; m++)
    if (!strcmp (m->name, name))
      {
	GtkTypeInfo info = { NULL };
	info.type_name = name;
	return gtk_type_unique (m->type, &info);
      }

  return GTK_TYPE_INVALID;
}

static int
sgtk_fillin_type_info (sgtk_type_info *info)
{
  if (info->type != GTK_TYPE_OBJECT
      && info->type == GTK_FUNDAMENTAL_TYPE (info->type)
      && info->type != GTK_TYPE_INVALID)
    {
      GtkType parent_type = info->type;
      GtkType this_type = gtk_type_from_name (info->name);
      if (this_type == GTK_TYPE_INVALID)
	this_type = sgtk_try_missing_type (info->name);
      if (this_type == GTK_TYPE_INVALID)
	{
	  fprintf (stderr, "unknown type `%s'.\n", info->name);
	  return 0;
	}
      info->type = this_type;
      if (GTK_FUNDAMENTAL_TYPE (info->type) != parent_type)
	{
	  fprintf (stderr, "mismatch for type `%s'.\n", info->name);
	  info->type = GTK_TYPE_INVALID;
	  return 0;
	}
      enter_type_info (info);
    }

  return 1;
}      
     
sgtk_type_info*
sgtk_maybe_find_type_info (GtkType type)
{
  sgtk_type_info *info;
  type_infos *infos;
  char *name;

  info = sgtk_get_type_info (GTK_TYPE_SEQNO(type));
  if (info)
    return info;

  /* XXX - merge this with the GtkObject code.  I don't have the brain
     right now to do it. */

  name = gtk_type_name (type);
  for (infos = all_type_infos; infos; infos = infos->next)
    {
      sgtk_type_info **ip;
      for (ip = infos->infos; *ip; ip++)
	if (!strcmp ((*ip)->name, name))
	  {
	    if (GTK_FUNDAMENTAL_TYPE (type) != (*ip)->type)
	      {
		fprintf (stderr, "mismatch for type `%s'.\n", name);
		info->type = GTK_TYPE_INVALID;
		abort ();
	      }
	    (*ip)->type = type;
	    enter_type_info (*ip);
	    return *ip;
	  }
    }

  /* XXX - should use the Gtk+ type introspection here instead of
     giving up. */

  return NULL;
}

sgtk_type_info *
sgtk_find_type_info (GtkType type)
{
  sgtk_type_info *info = sgtk_maybe_find_type_info (type);

  if (info)
    return info;

  fprintf (stderr, "unknown type `%s'.\n", gtk_type_name (type));
  abort ();
}



/* GtkObjects.

   GtkObjects are wrapped with a smob.  The smob of a GtkObject is
   called its proxy.  The proxy and its GtkObject are strongly
   connected; that is, the GtkObject will stay around as long as the
   proxy is referenced from Scheme, and the proxy will not be
   collected as long as the GtkObject is used from outside of Scheme.

   The lifetime of GtkObjects is controlled by a reference count,
   while Scheme objects are managed by a tracing garbage collector
   (mark/sweep).  These two techniques are made to cooperate like
   this: the pointer from the proxy to the GtkObject is reflected in
   the reference count of the GtkObject.  All proxies are kept in a
   list and those that point to GtkObjects with a reference count
   greater than the number of `internal' references are marked during
   the marking phase of the tracing collector.  An internal reference
   is one that goes from a GtkObject with a proxy to another GtkObject
   with a proxy.  We can only find a subset of the true internal
   references (because Gtk does not yet cooperate), but this should be
   good enough.

   By using this combination of tracing and reference counting it is
   possible to break the cycle that is formed by the proxy pointing to
   the GtkObject and the GtkObject pointing back.  It is
   straightforward to extend this to other kind of cycles that might
   occur.  For example, when connecting a Scheme procedure as a signal
   handler, the procedure is very likely to have the GtkObject that it
   is connected to in its environment.  This cycle can be broken by
   including the procedure in the set of Scheme objects that get
   marked when we are tracing GtkObjects with a reference count
   greater than 1.

   Therefore, each proxy contains a list of `protects' that are marked
   when the proxy itself is marked.  In addition to this, there is
   also a global list of `protects' that is used for Scheme objects
   that are somewhere in Gtk land but not clearly associated with a
   particular GtkObject (like timeout callbacks).

  */

/* The CDR of a GtkObject smob points to one of these.  PROTECTS is a
   Scheme list of all SCM values that need to be protected from the GC
   because they are in use by OBJ.  PROTECTS includes the smob cell
   itself.  NEXT and PREVP are used to chain all proxies together for
   the marking mentioned above.  NEXT simply points to the next proxy
   struct and PREVP points to the pointer that points to us.  */

typedef struct _sgtk_object_proxy {
  repv car;
  GtkObject *obj;
  repv protects;
  int traced_refs;
  struct _sgtk_object_proxy *next;
} sgtk_object_proxy;

/* The list of all existing proxies. */

static sgtk_object_proxy *all_proxies = NULL;

/* Analogous to the PROTECTS list of a proxy but for SCM values that
   are not associated with a particular GtkObject. */

static repv global_protects;

/* The smob for GtkObjects.  */

static long tc16_gtkobj;

#define GTKOBJP(x)       (rep_CELL16_TYPEP(x, tc16_gtkobj))
#define GTKOBJ_PROXY(x)  ((sgtk_object_proxy *)rep_PTR(x))

static void
mark_traced_ref (GtkWidget *obj, void *data)
{
  repv p = (repv)get_proxy (obj);
  if (p != Qnil)
    {
      sgtk_object_proxy *proxy = GTKOBJ_PROXY (p);
#ifdef DEBUG_PRINT
      fprintf (stderr, "marking trace %p %s\n",
	       proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif
      rep_MARKVAL (proxy->protects);
    }
}

static void
gtkobj_mark (repv obj)
{
  sgtk_object_proxy *proxy = GTKOBJ_PROXY(obj);

#ifdef DEBUG_PRINT
  fprintf (stderr, "marking %p %s\n",
	   proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif

  if (GTK_IS_CONTAINER (proxy->obj))
    gtk_container_foreach (GTK_CONTAINER(proxy->obj), mark_traced_ref, NULL);
  rep_MARKVAL(proxy->protects);
}

static void
gtkobj_print (repv stream, repv obj)
{
    char buf[32];
  sgtk_object_proxy *proxy = GTKOBJ_PROXY (obj);
  GtkType tid = GTK_OBJECT_TYPE (proxy->obj);

  rep_stream_puts (stream, "#<", -1, rep_FALSE);
  rep_stream_puts (stream, gtk_type_name (tid), -1, rep_FALSE);
  rep_stream_puts (stream, " ", -1, rep_FALSE);
  sprintf (buf, "%lx", (long)proxy->obj);
  rep_stream_puts (stream, buf, -1, rep_FALSE);
  rep_stream_putc (stream, '>');
}

static void
gtkobj_free (repv obj)
{
  sgtk_object_proxy *proxy = GTKOBJ_PROXY (obj);

#ifdef DEBUG_PRINT
  fprintf (stderr, "freeing %p %s\n",
	   proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif

  forget_proxy (proxy->obj);
  gtk_object_unref (proxy->obj);
  rep_FREE_CELL ((char *)proxy);
}

static void
gtkobj_sweep (void)
{
  sgtk_object_proxy *proxy = all_proxies;
  all_proxies = 0;
  while (proxy != 0)
  {
      sgtk_object_proxy *next = proxy->next;
      if (! rep_GC_CELL_MARKEDP(rep_VAL(proxy)))
	  gtkobj_free (rep_VAL(proxy));
      else
      {
	  rep_GC_CLR_CELL (rep_VAL(proxy));
	  proxy->next = all_proxies;
	  all_proxies = proxy;
      }
      proxy = next;
  }
}

/* Protect OBJ from being collected.  When PROTECTOR is a GtkObject
   proxy, OBJ is only protected as long as GtkObject is live. */

repv
sgtk_protect (repv protector, repv obj)
{
  if (GTKOBJP (protector))
    {
      sgtk_object_proxy *proxy = GTKOBJ_PROXY(protector);
      proxy->protects = Fcons (obj, proxy->protects);
    }
  else
    global_protects = Fcons (obj, global_protects);
  return obj;
}

static void
sgtk_unprotect_1 (repv *prev, repv obj)
{
  repv walk;

  for (walk = *prev; rep_CONSP (walk);
       walk = rep_CDR (walk))
    {
      if (rep_CAR (walk) == obj)
	{
	  *prev = rep_CDR (walk);
	  break;
	}
      else
	prev = rep_CDRLOC (walk);
    }
}

/* XXX - performance improvement by searching only a single
   proxy->protects.  */

void
sgtk_unprotect (repv obj)
{
  sgtk_object_proxy *proxy;

  for (proxy = all_proxies; proxy; proxy = proxy->next)
    sgtk_unprotect_1 (&proxy->protects, obj);
  sgtk_unprotect_1 (&global_protects, obj);
}

/* Treating GtkObject proxies right during GC.  We need to run custom
   code during the mark phase of the Scheme GC.  We do this by
   creating a new smob type and allocating one actual smob of it.
   This smob is made permanent and thus its marking function is
   invoked for every GC.  We hijack this function to do the tracing of
   all existing proxies as well. */

static void
count_traced_ref (GtkWidget *obj, void *data)
{
  repv p = (repv)get_proxy (obj);
  if (p != Qnil)
    {
      sgtk_object_proxy *proxy = GTKOBJ_PROXY (p);
#ifdef DEBUG_PRINT
      fprintf (stderr, "counting %p %s\n",
	       proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif
      proxy->traced_refs++;
    }
}

static void
gtkobj_marker_hook (void)
{
  sgtk_object_proxy *proxy;

  /* We do two passes here.  The first pass counts how many references
     an object has from other objects that have a proxy.  The second
     pass marks all objects that have more than this number of
     references.  For the first pass to work, we need to enumerate all
     references that an object has to other objects.  We can't do that
     precisely without help from Gtk+ itself.  But luckily, *not*
     knowing about an `internal' reference is the conservative thing.
     Missing a reference will make it appear to us that an object has
     more `external' references to it than it really has, thus making
     us keep the proxy alive.  Only when these `external' references
     form a cycle over some Scheme values, we loose.  As a first
     approximation to the true set of references of a GtkObject, we
     just traverse its children with gtk_container_foreach.  */

  /* First pass. */
  for (proxy = all_proxies; proxy; proxy = proxy->next)
    {
      GtkObject *obj = proxy->obj;
#ifdef DEBUG_PRINT
      fprintf (stderr, "on %p %p\n", proxy, obj);
#endif
      if (GTK_IS_CONTAINER (obj))
	gtk_container_foreach (GTK_CONTAINER(obj), count_traced_ref, NULL);
    }
#ifdef DEBUG_PRINT
  fprintf (stderr, "done with pass 1.\n");
#endif

  /* Second pass. */
  for (proxy = all_proxies; proxy; proxy = proxy->next)
    {
      if (proxy->obj->ref_count > proxy->traced_refs + 1)
	{
#ifdef DEBUG_PRINT
	  fprintf (stderr, "hooking %p %s\n",
		   proxy->obj, gtk_type_name (GTK_OBJECT_TYPE (proxy->obj)));
#endif
	  rep_MARKVAL (proxy->protects);
	}
      proxy->traced_refs = 0;
    }
  rep_MARKVAL (global_protects);
}

/* Create a proxy for OBJ. */

static repv
make_gtkobj (GtkObject *obj)
{
  sgtk_object_proxy *proxy;

  proxy = (sgtk_object_proxy *)rep_ALLOC_CELL (sizeof(sgtk_object_proxy));
  gtk_object_ref (obj);
  gtk_object_sink (obj);
#ifdef DEBUG_PRINT
  fprintf (stderr, "New proxy %p for %p %s\n", proxy, obj,
	   gtk_type_name (GTK_OBJECT_TYPE (obj)));
#endif
  proxy->obj = obj;
  proxy->protects = Qnil;
  proxy->traced_refs = 0;
  proxy->next = all_proxies;
  all_proxies = proxy;

  proxy->car = tc16_gtkobj;
  enter_proxy (obj, rep_VAL(proxy));

  sgtk_protect (rep_VAL(proxy), rep_VAL(proxy));
  return rep_VAL(proxy);
}

/* Return the proxy for OBJ if it already has one, else create a new
   one.  When OBJ is NULL, return `#f'. */

repv
sgtk_wrap_gtkobj (GtkObject *obj)
{
  repv handle;

  if (obj == NULL)
    return Qnil;

  handle = get_proxy (obj);
  if (handle == Qnil)
    handle = make_gtkobj (obj);
  return handle;
}

int
sgtk_is_a_gtkobj (guint type, repv obj)
{
  if (!(GTKOBJP (obj)))
    return 0;
  return gtk_type_is_a (GTK_OBJECT_TYPE(GTKOBJ_PROXY(obj)->obj), type);
}

GtkObject*
sgtk_get_gtkobj (repv obj)
{
  if (obj == Qnil)
    return NULL;
  else
    return GTKOBJ_PROXY(obj)->obj;
}


/* Enums.

   Enumerations are described by a `sgtk_enum_info' structure.  That
   structure contains a list of all literals and their respective
   values.  In Scheme, an enum element is represented by a symbol
   whose name is the literal. */

int
sgtk_valid_enum (repv obj, sgtk_enum_info *info)
{
  int i;
  char *obj_name;

  if (!rep_SYMBOLP (obj))
    return 0;

  obj_name = rep_STR(rep_SYM(obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (!strcmp (info->literals[i].name, obj_name))
      return 1;
  return 0;
}

repv
sgtk_enum_to_rep (gint val, sgtk_enum_info *info)
{
  int i;
  for (i = 0; i < info->n_literals; i++)
    if (info->literals[i].value == val)
      return Fintern (rep_string_dup(info->literals[i].name), Qnil);
#if 0
  /* XXX */
  SCM_ASSERT (0, SCM_MAKINUM (val), SCM_ARG1, "enum->symbol");
#endif
  return Qnil;
}

gint
sgtk_rep_to_enum (repv obj, sgtk_enum_info *info)
{
  int i;
  char *obj_name = rep_STR(rep_SYM(obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (!strcmp (info->literals[i].name, obj_name))
      return info->literals[i].value;
  return -1;
}


/* Flags.

   Like enums, flags are described by a `sgtk_enum_info' structure.
   In Scheme, flags are represented by a list of symbols, one for each
   bit that is set in the flags value. */

int
sgtk_valid_flags (repv obj, sgtk_enum_info *info)
{
  while (obj != Qnil)
    {
      int i, valid;
      repv sym;
      char *sym_name;
      
      if (!rep_CONSP (obj))
	return 0;
      sym = rep_CAR (obj);
      if (!rep_SYMBOLP (sym))
	return 0;
      
      sym_name = rep_STR(rep_SYM(sym)->name);
      for (i = 0, valid = 0; i < info->n_literals; i++)
	if (!strcmp (info->literals[i].name, sym_name))
	  {
	    valid = 1;
	    break;
	  }
      if (!valid)
	return 0;

      obj = rep_CDR (obj);
    }
  
  return 1;
}

repv
sgtk_flags_to_rep (gint val, sgtk_enum_info *info)
{
  repv ans = Qnil;
  int i;
  for (i = 0; i < info->n_literals; i++)
    if (val & info->literals[i].value)
      {
	ans = Fcons (Fintern (rep_string_dup(info->literals[i].name), Qnil),
		     ans);
	val &= ~info->literals[i].value;
      }
  return ans;
}

gint
sgtk_rep_to_flags (repv obj, sgtk_enum_info *info)
{
  int ans = 0;

  while (obj != Qnil)
    {
      int i;
      repv sym = rep_CAR (obj);
      char *sym_name = rep_STR(rep_SYM(sym)->name);

      for (i = 0; i < info->n_literals; i++)
	if (!strcmp (info->literals[i].name, sym_name))
	  {
	    ans |= info->literals[i].value;
	    break;
	  }
      obj = rep_CDR (obj);
    }
  
  return ans;
}


/* String enums.

   A string enum is like an enum, but the values are strings.  The
   range of values can be extended, so anywhere a "string enum" value
   is accepted, we also accept a string (but not a symbol).  */

int
sgtk_valid_senum (repv obj, sgtk_senum_info *info)
{
  int i;
  char *obj_name;

  if (rep_STRINGP (obj))
    return 1;
  if (! rep_SYMBOLP (obj))
    return 0;

  obj_name = rep_STR(rep_SYM(obj)->name);
  for (i = 0; i < info->n_literals; i++)
    if (! strcmp (info->literals[i].name, obj_name))
      return 1;
  return 0;
}

repv
sgtk_senum_to_rep (char *val, sgtk_senum_info *info)
{
  int i;
  for (i = 0; i < info->n_literals; i++)
    if (! strcmp (info->literals[i].value, val))
      return Fintern (rep_string_dup(info->literals[i].name), Qnil);
  return rep_string_dup (val);
}

char *
sgtk_rep_to_senum (repv obj, sgtk_senum_info *info)
{
  int i;

  if (rep_STRINGP (obj))
    {
      return rep_STR (obj);
    }

  for (i = 0; i < info->n_literals; i++)
    if (! strcmp (info->literals[i].name, rep_STR (obj)))
      return info->literals[i].value;
  return NULL;
}



/* Boxed Values.

   I'm trying to use the same hash table approach as with the gtkobj's,
   but without such complex gc tracing. I'm hoping that the `opaqueness'
   of the boxed types preclude any internal pointers..  --jsh

 */

typedef struct _sgtk_boxed_proxy {
  repv car;
  struct _sgtk_boxed_proxy *next;
  guint seqno;
  gpointer ptr;
} sgtk_boxed_proxy;

static sgtk_boxed_proxy *all_boxed;

static long tc16_boxed;

#define BOXED_P(x)     (rep_CELL16_TYPEP(x, tc16_boxed))
#define BOXED_PROXY(x) ((sgtk_boxed_proxy *)rep_PTR(x))
#define BOXED_SEQNO(x) (BOXED_PROXY(x)->seqno)
#define BOXED_PTR(x)   (BOXED_PROXY(x)->ptr)
#define BOXED_INFO(x)  ((sgtk_boxed_info*)must_get_type_info(BOXED_SEQNO(x)))

static void
boxed_free (repv obj)
{
  sgtk_boxed_info *info = BOXED_INFO (obj);
  info->destroy (BOXED_PTR (obj));
  forget_proxy (BOXED_PTR (obj));
  rep_FREE_CELL (rep_PTR(obj));
}

static void
boxed_print (repv stream, repv exp)
{
  char buf[32];
  sgtk_boxed_info *info = BOXED_INFO (exp);
  rep_stream_puts (stream, "#<", -1, rep_FALSE);
  rep_stream_puts (stream, info->header.name, -1, rep_FALSE);
  rep_stream_putc (stream, ' ');
  sprintf (buf, "%lx", (long)BOXED_PTR (exp));
  rep_stream_puts (stream, buf, -1, rep_FALSE);
  rep_stream_putc (stream, '>');
}

static void
boxed_sweep (void)
{
  sgtk_boxed_proxy *proxy = all_boxed;
  all_boxed = 0;
  while (proxy != 0)
  {
      sgtk_boxed_proxy *next = proxy->next;
      if (! rep_GC_CELL_MARKEDP(rep_VAL(proxy)))
	  boxed_free (rep_VAL(proxy));
      else
      {
	  rep_GC_CLR_CELL (rep_VAL(proxy));
	  proxy->next = all_boxed;
	  all_boxed = proxy;
      }
      proxy = next;
  }
}

repv
sgtk_boxed_to_rep (gpointer ptr, sgtk_boxed_info *info, int copyp)
{
  repv handle;

  if (ptr == NULL)
    return Qnil;

  if (!sgtk_fillin_type_info (&info->header))
    return Qnil;

  handle = get_proxy (ptr);
  if (handle == Qnil) {
      /* Allocate a new proxy */
      sgtk_boxed_proxy *p = rep_ALLOC_CELL (sizeof (sgtk_boxed_proxy));
      if (copyp)
	  ptr = info->copy (ptr);
      if (GTK_TYPE_SEQNO(info->header.type) > 0xFFFF)
	  abort ();
      p->car = tc16_boxed;
      p->next = all_boxed;
      all_boxed = p;
      p->seqno = GTK_TYPE_SEQNO(info->header.type);
      p->ptr = ptr;
      handle = rep_VAL(p);
  }
  return handle;
}

void *
sgtk_rep_to_boxed (repv obj)
{
  if (obj == Qnil)
    return NULL;
  return BOXED_PTR (obj);
}

int
sgtk_valid_boxed (repv obj, sgtk_boxed_info *info)
{
  return (BOXED_P (obj) && BOXED_INFO (obj) == info);
}

/* Floats.

   Only here to set things straight.

   Of course, rep currently only has integer. Kludge this. --jsh */

int
sgtk_valid_float (repv obj)
{
  return rep_INTP (obj);
}

gfloat
sgtk_rep_to_float (repv obj)
{
  return rep_INTP (obj) ? rep_INT (obj) : 0.0;
}

repv
sgtk_float_to_rep (gfloat f)
{
  return rep_MAKE_INT ((long int)f);
}

int
sgtk_valid_double (repv obj)
{
  return rep_INTP (obj);
}

double
sgtk_rep_to_double (repv obj)
{
  return rep_INTP (obj) ? rep_INT (obj) : 0.0;
}

repv
sgtk_double_to_rep (double f)
{
  return rep_MAKE_INT ((long int)f);
}

int
sgtk_valid_point (repv obj)
{
  return (rep_CONSP (obj)
	  && rep_INTP (rep_CAR (obj))    /* too permissive */
	  && rep_INTP (rep_CDR (obj)));  /* too permissive */
}

GdkPoint
sgtk_rep_to_point (repv obj)
{
  GdkPoint res;
  res.x = rep_INT (rep_CAR (obj));
  res.y = rep_INT (rep_CDR (obj));
  return res;
}

repv
sgtk_point_to_rep (GdkPoint p)
{
  return Fcons (sgtk_int_to_rep (p.x),
		sgtk_int_to_rep (p.y));
}



/* GtkType objects

   I'm going to be lazy and try to store these in rep's 30-bit
   signed integers, let's see if it works...  --jsh */

#define GTKTYPEP(x)     (rep_INTP(x))
#define GTKTYPE(x)      ((GtkType)rep_INT(x))

GtkType
sgtk_type_from_name (char *name)
{
  GtkType type = gtk_type_from_name (name);
  if (type == GTK_TYPE_INVALID)
    {
      sgtk_object_info *info = sgtk_find_object_info (name);
      if (info)
	type = info->header.type;
    }
  return type;
}

int
sgtk_valid_type (repv obj)
{
  return (obj == Qnil || GTKTYPEP (obj)
	  || (rep_SYMBOLP (obj)
	      && sgtk_type_from_name (rep_STR(rep_SYM(obj)->name))));
}

GtkType
sgtk_rep_to_type (repv obj)
{
  if (obj == Qnil)
    return GTK_TYPE_INVALID;
  else if (GTKTYPEP (obj))
    return GTKTYPE (obj);
  else {
      if (rep_SYMBOLP(obj))
	  obj = rep_SYM(obj)->name;

      if (rep_STRINGP(obj))
	  return sgtk_type_from_name (rep_STR (obj));
      else
	  return GTK_TYPE_INVALID;
  }
}

repv
sgtk_type_to_rep (GtkType t)
{
  if (t == GTK_TYPE_INVALID)
    return Qnil;

  assert (t <= rep_LISP_MAX_INT);

  return sgtk_uint_to_rep (t);
}



/* Composites. */

int
sgtk_valid_composite (repv obj, int (*predicate)(repv))
{
  return sgtk_valid_complen (obj, predicate, -1);
}

int
sgtk_valid_complen (repv obj, int (*predicate)(repv), int len)
{
  int actual_len;

  if (rep_LISTP(obj))
    {
      actual_len = list_length (obj);

      if (len >= 0 && len != actual_len)
	return 0;

      if (predicate)
	{
	  while (rep_CONSP(obj))
	    {
	      if (!predicate (rep_CAR(obj)))
		return 0;
	      obj = rep_CDR(obj);
	    }
	}
      return 1;
    }
  else if (rep_VECTORP(obj))
    {
      int i;
      repv *elts;

      actual_len = rep_VECT_LEN (obj);
      if (len >= 0 && len != actual_len)
	return 0;

      if (predicate)
	{
	  elts = rep_VECT(obj)->array;
	  for (i = 0; i < len; i++)
	    if (!predicate(elts[i]))
	      return 0;
	}
      return 1;
    }
  else
    return 0;
}

repv
sgtk_slist_to_rep (GSList *list, repv (*toscm)(void*))
{
  repv res, *tail = &res;
  while (list)
    {
      *tail = Fcons (toscm (&list->data), *tail);
      tail = rep_CDRLOC (*tail);
      list = list->next;
    }
  *tail = Qnil;
  return res;
}

GSList*
sgtk_rep_to_slist (repv obj, void (*fromscm)(repv, void*))
{
  GSList *res, **tail = &res;

  if (obj == Qnil || (rep_CONSP(obj)))
    {
      while (rep_CONSP(obj))
	{
	  *tail = g_slist_alloc ();
	  if (fromscm)
	    fromscm (rep_CAR (obj), &(*tail)->data);
	  obj = rep_CDR(obj);
	  tail = &(*tail)->next;
	}
    }
  else if (rep_VECTORP(obj))
    {
      int len = rep_VECT_LEN (obj), i;
      repv *elts = rep_VECT(obj)->array;
      for (i = 0; i < len; i++)
	{
	  *tail = g_slist_alloc ();
	  if (fromscm)
	    fromscm (elts[i], &(*tail)->data);
	  tail = &(*tail)->next;
	}
    }
  *tail = NULL;
  return res;
}

void
sgtk_slist_finish (GSList *list, repv obj, repv (*toscm)(void*))
{
  if (toscm)
    {
      if (obj == Qnil || (rep_CONSP(obj)))
	{
	  while (rep_CONSP(obj) && list)
	    {
	      rep_CAR(obj) = toscm (list->data);
	      obj = rep_CDR(obj);
	      list = list->next;
	    }
	}
      else if (rep_VECTORP(obj))
	{
	  int len = rep_VECT_LEN (obj), i;
	  repv *elts = rep_VECT(obj)->array;
	  for (i = 0; i < len && list; i++)
	    {
	      elts[i] = toscm (list->data);
	      list = list->next;
	    }
	}
    }

  g_slist_free (list);
}

repv
sgtk_list_to_rep (GList *list, repv (*toscm)(void*))
{
  repv res, *tail = &res;
  while (list)
    {
      *tail = Fcons (toscm (&list->data), *tail);
      tail = rep_CDRLOC (*tail);
      list = list->next;
    }
  *tail = Qnil;
  return res;
}

GList*
sgtk_rep_to_list (repv obj, void (*fromscm)(repv, void*))
{
  GList *res = NULL, *tail;
  
  if (obj == Qnil || (rep_CONSP(obj)))
    {
      while (rep_CONSP(obj))
      {
        GList *n = g_list_alloc ();
	if (res == NULL)
	  res = tail = n;
	else 
	  {
	    g_list_concat (tail, n);
	    tail = n;
	  }
	if (fromscm)
	  fromscm (rep_CAR (obj), &(n->data));
	obj = rep_CDR(obj);
      }
    }
  else if (rep_VECTORP(obj))
    {
      int len = rep_VECT_LEN (obj), i;
      repv *elts = rep_VECT(obj)->array;
      for (i = 0; i < len; i++)
	{
	  GList *n = g_list_alloc ();
	  if (res == NULL)
	    res = tail = n;
	  else 
	    {
	      g_list_concat (tail, n);
	      tail = n;
	    }
	  if (fromscm)
	    fromscm (elts[i], &(n->data));
	}
    }
  return res;
}

void
sgtk_list_finish (GList *list, repv obj, repv (*toscm)(void*))
{
  if (toscm)
    {
      if (obj == Qnil || (rep_CONSP(obj)))
	{
	  while (rep_CONSP(obj) && list)
	    {
	      rep_CAR (obj) = toscm (list->data);
	      obj = rep_CDR(obj);
	      list = list->next;
	    }
	}
      else if (rep_VECTORP(obj))
	{
	  int len = rep_VECT_LEN (obj), i;
	  repv *elts = rep_VECT(obj)->array;
	  for (i = 0; i < len && list; i++)
	    {
	      elts[i] = toscm (list->data);
	      list = list->next;
	    }
	}
    }
  
  g_list_free (list);
}

sgtk_cvec
sgtk_rep_to_cvec (repv obj, void (*fromscm)(repv, void*), size_t sz)
{
  sgtk_cvec res;
  int i;
  char *ptr;

  if (rep_LISTP(obj))
    {
      res.count = list_length (obj);
      res.vec = rep_alloc (res.count * sz);
      if (fromscm)
	{
	  for (i = 0, ptr = res.vec; i < res.count; i++, ptr += sz)
	    {
	      fromscm (rep_CAR (obj), ptr);
	      obj = rep_CDR(obj);
	    }
	}
    }
  else if (rep_VECTORP(obj))
    {
      repv *elts = rep_VECT(obj)->array;
      res.count = rep_VECT_LEN (obj);
      res.vec = rep_alloc (res.count * sz);
      if (fromscm)
	{
	  for (i = 0, ptr = res.vec; i < res.count; i++, ptr += sz)
	    fromscm (elts[i], ptr);
	}
    }

  return res;
}

void
sgtk_cvec_finish (sgtk_cvec *cvec, repv obj, repv (*toscm)(void *), size_t sz)
{
  if (toscm)
    {
      if (obj == Qnil || (rep_CONSP(obj)))
	{
	  int i, len = cvec->count;
	  char *ptr;

	  for (i = 0, ptr = cvec->vec;
	       i < len && rep_CONSP(obj);
	       i++, ptr += sz, obj = rep_CDR (obj))
	    {
	      rep_CAR (obj) = toscm (ptr);
	    }
	}
      else if (rep_VECTORP(obj))
	{
	  repv *elts = rep_VECT(obj)->array;
	  int len1 = rep_VECT_LEN (obj), len2 = cvec->count, i;
	  char *ptr;

	  for (i = 0, ptr = cvec->vec; i < len1 && i < len2; i++, ptr += sz)
	    elts[i] = toscm (ptr);
	}
    }

  rep_free (cvec->vec);
}


/* converting between SCM and GtkArg */

repv
sgtk_arg_to_rep (GtkArg *a, int free_mem)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return Qnil;
    case GTK_TYPE_CHAR:
      return rep_MAKE_INT (GTK_VALUE_CHAR(*a));
    case GTK_TYPE_BOOL:
      return GTK_VALUE_BOOL(*a)? Qt : Qnil;
    case GTK_TYPE_INT:
      return sgtk_int_to_rep (GTK_VALUE_INT(*a));
    case GTK_TYPE_UINT:
      return sgtk_uint_to_rep (GTK_VALUE_UINT(*a));
    case GTK_TYPE_LONG:
      return sgtk_int_to_rep (GTK_VALUE_LONG(*a));
    case GTK_TYPE_ULONG:
      return sgtk_uint_to_rep (GTK_VALUE_ULONG(*a));
    case GTK_TYPE_FLOAT:
      return sgtk_float_to_rep (GTK_VALUE_FLOAT(*a));
    case GTK_TYPE_DOUBLE:
      return sgtk_double_to_rep (GTK_VALUE_DOUBLE(*a));
    case GTK_TYPE_STRING:
      {
	repv ret = rep_string_dup (GTK_VALUE_STRING(*a));
	if (free_mem)
	  g_free GTK_VALUE_STRING(*a);
	return ret;
      }
    case GTK_TYPE_ENUM:
      return sgtk_enum_to_rep (GTK_VALUE_FLAGS(*a),
			       (sgtk_enum_info *)sgtk_find_type_info (a->type));
    case GTK_TYPE_FLAGS:
      return sgtk_flags_to_rep (GTK_VALUE_FLAGS(*a),
				(sgtk_enum_info *)sgtk_find_type_info (a->type));
    case GTK_TYPE_BOXED:
      return sgtk_boxed_to_rep (GTK_VALUE_BOXED(*a),
				(sgtk_boxed_info *)sgtk_find_type_info (a->type),
				TRUE);
    case GTK_TYPE_OBJECT:
      return sgtk_wrap_gtkobj (GTK_VALUE_OBJECT(*a));
    default:
      fprintf (stderr, "illegal type %s in arg\n", 
	       gtk_type_name (a->type));
      return Qnil;
    }
}

int
sgtk_valid_arg (GtkArg *a, repv obj)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return TRUE;
    case GTK_TYPE_CHAR:
      return rep_INTP(obj);
    case GTK_TYPE_BOOL:
      return TRUE;
    case GTK_TYPE_INT:
    case GTK_TYPE_UINT:
    case GTK_TYPE_LONG:
    case GTK_TYPE_ULONG:
    case GTK_TYPE_FLOAT:
    case GTK_TYPE_DOUBLE:
      return rep_INTP (obj);		/* XXX to be fixed.. --jsh */
    case GTK_TYPE_STRING:
      return rep_STRINGP (obj);
    case GTK_TYPE_ENUM:
      return sgtk_valid_enum (obj, ((sgtk_enum_info *)
				    sgtk_find_type_info (a->type)));
    case GTK_TYPE_FLAGS:
      return sgtk_valid_flags (obj, ((sgtk_enum_info *)
				     sgtk_find_type_info (a->type)));
    case GTK_TYPE_BOXED:
      return sgtk_valid_boxed (obj, ((sgtk_boxed_info *)
				     sgtk_find_type_info (a->type)));
      break;
    case GTK_TYPE_CALLBACK:
      return Ffunctionp (obj);
    case GTK_TYPE_OBJECT:
      return sgtk_is_a_gtkobj (a->type, obj);
    default:
      fprintf (stderr, "unhandled arg type %s\n", gtk_type_name (a->type));
      return FALSE;
    }
}

void
sgtk_rep_to_arg (GtkArg *a, repv obj, repv protector)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return;
    case GTK_TYPE_CHAR:
      GTK_VALUE_CHAR(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_BOOL:
      GTK_VALUE_BOOL(*a) = obj != Qnil;
      break;
    case GTK_TYPE_INT:
      GTK_VALUE_INT(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_UINT:
      GTK_VALUE_UINT(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_LONG:
      GTK_VALUE_LONG(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_ULONG:
      GTK_VALUE_ULONG(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_FLOAT:
      GTK_VALUE_FLOAT(*a) = sgtk_rep_to_float (obj);
      break;
    case GTK_TYPE_DOUBLE:
      GTK_VALUE_DOUBLE(*a) = sgtk_rep_to_double (obj);
      break;
    case GTK_TYPE_STRING:
      GTK_VALUE_STRING(*a) = rep_STR (obj);
      break;
    case GTK_TYPE_ENUM:
      GTK_VALUE_ENUM(*a) =
	sgtk_rep_to_enum (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_FLAGS:
      GTK_VALUE_ENUM(*a) =
	sgtk_rep_to_flags (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_BOXED:
      GTK_VALUE_BOXED(*a) = sgtk_rep_to_boxed (obj);
      break;
    case GTK_TYPE_CALLBACK:
      sgtk_protect (protector, obj);
      GTK_VALUE_CALLBACK(*a).marshal = sgtk_callback_marshal;
      GTK_VALUE_CALLBACK(*a).data = (gpointer)obj;
      GTK_VALUE_CALLBACK(*a).notify = sgtk_callback_destroy;
      break;
    case GTK_TYPE_OBJECT:
      GTK_VALUE_OBJECT(*a) = sgtk_get_gtkobj (obj);
      break;
    default:
      fprintf (stderr, "unhandled arg type %s\n", gtk_type_name (a->type));
      break;
    }
}

void
sgtk_rep_to_ret (GtkArg *a, repv obj)
{
  switch (GTK_FUNDAMENTAL_TYPE (a->type))
    {
    case GTK_TYPE_NONE:
      return;
    case GTK_TYPE_CHAR:
      *GTK_RETLOC_CHAR(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_BOOL:
      *GTK_RETLOC_BOOL(*a) = (obj != Qnil);
      break;
    case GTK_TYPE_INT:
      *GTK_RETLOC_INT(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_UINT:
      *GTK_RETLOC_UINT(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_LONG:
      *GTK_RETLOC_LONG(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_ULONG:
      *GTK_RETLOC_ULONG(*a) = rep_INT (obj);
      break;
    case GTK_TYPE_FLOAT:
      *GTK_RETLOC_FLOAT(*a) = sgtk_rep_to_float (obj);
      break;
    case GTK_TYPE_DOUBLE:
      *GTK_RETLOC_DOUBLE(*a) = sgtk_rep_to_double (obj);
      break;
    case GTK_TYPE_STRING:
      GTK_VALUE_STRING(*a) = g_strdup (rep_STR(obj));
      break;
    case GTK_TYPE_ENUM:
      *GTK_RETLOC_ENUM(*a) =
	sgtk_rep_to_enum (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_FLAGS:
      *GTK_RETLOC_ENUM(*a) =
	sgtk_rep_to_flags (obj, (sgtk_enum_info *)sgtk_find_type_info (a->type));
      break;
    case GTK_TYPE_BOXED:
      *GTK_RETLOC_BOXED(*a) = sgtk_rep_to_boxed (obj);
      break;
    case GTK_TYPE_OBJECT:
      if (sgtk_is_a_gtkobj (a->type, obj))
	  *GTK_RETLOC_OBJECT(*a) = sgtk_get_gtkobj (obj);
      else
	  *GTK_RETLOC_OBJECT(*a) = NULL;
      break;
    default:
      fprintf (stderr, "unhandled return type %s\n", gtk_type_name (a->type));
      break;
    }
}



/* Callbacks.

   Callbacks are executed within a new dynamic root.  That means that
   the flow of control can't leave them without Gtk noticing.  Throws
   are catched and briefly reported.  Calls to continuations that have
   been made outside the dynamic root can not be activated.

   Callbacks are invoked with whatever arguments that are specified by
   the Gtk documentation.  They do not, however, receive the GtkObject
   that has initiated the callback.

   [ actually, they do receive the GtkObject. For rep, there are no
     closures, so without the invoking object it's usually necessary
     to build ad hoc closures through backquoting..  --jsh ]

   When callback_trampoline is non-#f, we treat it as a procedure and
   call it as

      (trampoline proc args)

   PROC is the real callback procedure and ARGS is the list of
   arguments that should be passed to it.  */

static repv callback_trampoline;

DEFUN ("gtk-callback-trampoline", Fgtk_callback_trampoline,
       Sgtk_callback_trampoline, (repv new), rep_Subr1)
{
  repv old = rep_CAR (callback_trampoline);
  if (new != Qnil)
    rep_CAR (callback_trampoline) = new;
  return old;
}

/* Be carefull when this macro is true.
   scm_gc_heap_lock is set during gc.  */
#define rep_GC_P (rep_in_gc)

void
sgtk_callback_marshal (GtkObject *obj,
		       gpointer data,
		       guint n_args,
		       GtkArg *args)
{
  repv real_args = Qnil, ans;
  int i;

  if (rep_GC_P)
    {
      /* This should only happen for the "destroy" signal and is then
         harmless. */
      fprintf (stderr, "callback ignored during GC!\n");
      return;
    }
  
  for (i = n_args - 1; i >= 0; i--)
    real_args = Fcons (sgtk_arg_to_rep (args + i, 0), real_args);

  real_args = Fcons (sgtk_wrap_gtkobj (obj), real_args);

  if (rep_CAR(callback_trampoline) == Qnil)
    ans = rep_funcall ((repv)data, real_args, rep_FALSE);
  else
    ans = rep_funcall (rep_CAR(callback_trampoline),
		       Fcons ((repv)data, Fcons (real_args, Qnil)), rep_FALSE);

  sgtk_callback_postfix ();

  if (ans && args[n_args].type != GTK_TYPE_NONE)
    sgtk_rep_to_ret (args + n_args, ans);
}

void
sgtk_callback_destroy (gpointer data)
{
  sgtk_unprotect ((repv)data);
}



/* Type conversions */

extern sgtk_boxed_info sgtk_gdk_color_info;

repv
sgtk_color_conversion (repv color)
{
  repv orig_color = color;

  if (rep_STRINGP (color))
    {
      GdkColor colstruct;
      GdkColormap *colmap;

      if (!gdk_color_parse (rep_STR (color), &colstruct))
	{
	  Fsignal (Qerror, rep_list_2 (rep_string_dup ("no such color"),
				       orig_color));
	  return Qnil;
	}
      colmap = gtk_widget_peek_colormap ();
      if (!gdk_color_alloc (colmap, &colstruct))
	{
	  Fsignal (Qerror, rep_list_2 (rep_string_dup ("can't allocate color"),
				       orig_color));
	  return Qnil;
	}
      return sgtk_boxed_to_rep (&colstruct, &sgtk_gdk_color_info, 1);
    }
  return color;
}

extern repv Fgdk_font_load (repv font);

repv
sgtk_font_conversion (repv font)
{
  repv orig_font = font;

  if (rep_STRINGP (font))
    {
      font = Fgdk_font_load (font);
      if (font == Qnil)
	Fsignal (Qerror, rep_list_2 (rep_string_dup ("no such font: "),
				     orig_font));
    }
  return font;
}



/* Support for gtk_object_new, gtk_object_set, ... */

/* The SCM_PROC for the exported functions is in gtk-support.c to have
   it be snarfed for sgtk_init_gtk_support. */

sgtk_object_info *sgtk_find_object_info (char *name);

sgtk_object_info *
sgtk_find_object_info_from_type (GtkType type)
{
  sgtk_object_info *info;
  if (type == GTK_TYPE_INVALID)
    return NULL;
  info = (sgtk_object_info *)sgtk_get_type_info (GTK_TYPE_SEQNO(type));
  if (info)
    return info;
  
  return sgtk_find_object_info (gtk_type_name (type));
}

sgtk_object_info *
sgtk_find_object_info (char *name)
{
  GtkType type, parent;
  sgtk_object_info *info;
  type_infos *infos;
  int i;

  type = gtk_type_from_name (name);
  if (type != GTK_TYPE_INVALID)
    {
      info = (sgtk_object_info *)sgtk_get_type_info (GTK_TYPE_SEQNO(type));
      if (info)
	return info;
    }

  for (infos = all_type_infos; infos; infos = infos->next)
    {
      sgtk_type_info **ip;
      for (ip = infos->infos; *ip; ip++)
	if (!strcmp ((*ip)->name, name))
	  {
	    if (GTK_FUNDAMENTAL_TYPE((*ip)->type) != GTK_TYPE_OBJECT)
	      return NULL;

	    info = (sgtk_object_info *)*ip;
	    info->header.type = info->init_func ();
	    enter_type_info ((sgtk_type_info*)info);
	    goto query_args;
	  }
    }

  /* Not found among our precompiled types.  Construct a fresh
     sgtk_object_info, if it's known to Gtk+. */

  if (type != GTK_TYPE_INVALID)
    {
      fprintf (stderr, "Fresh info for %s, %d\n", name, type);

      info = (sgtk_object_info *)rep_alloc (sizeof(sgtk_object_info));
      info->header.type = type;
      info->header.name = name;
      info->init_func = NULL;
      enter_type_info ((sgtk_type_info*)info);
    }
  else
    return NULL;

 query_args:
  gtk_type_class (info->header.type);
  info->args = gtk_object_query_args (info->header.type,
				      &info->args_flags,
				      &info->n_args);
  info->args_short_names = (char **)rep_alloc (info->n_args*(sizeof(char*)));
  for (i = 0; i < info->n_args; i++)
    {
      char *l = info->args[i].name;
      char *d = strchr (l, ':');
      if (d == NULL || d[1] != ':')
	{
	  fprintf (stderr, "`%s' has no class part.\n", l);
	  info->args_short_names[i] = l;
	}
      else
	info->args_short_names[i] = d+2;
    }
  
  parent = gtk_type_parent (info->header.type);
  if (parent != GTK_TYPE_INVALID)
    info->parent = sgtk_find_object_info_from_type (parent);
  else
    info->parent = NULL;
  
  return info;
}

#ifdef NEED_UNUSED_CODE
static char*
xstrndup (char *str, int n)
{
  char *dup;

  if (str == NULL)
    return NULL;
  dup = rep_alloc (n+1, "xstrndup");
  strncpy (dup, str, n);
  dup[n] = '\0';
  return dup;
}
#endif

static void
sgtk_find_arg_info (GtkArg *arg, sgtk_object_info *info, char *name)
{
  /* XXX - handle signal handlers.  Do not use '::', use '.' instead. */

  char *d = strchr (name, ':');
  if (d && d[1] == ':')
    {
      /* A long name.  Find the object_info for the class part. */
      int len = d-name;

      while (info)
	{
	  if (info->header.name[len] == '\0'
	      && !strncmp (info->header.name, name, len))
	    break;
	  info = info->parent;
	}
      name = d+2;
    }
  
#ifdef DEBUG_PRINT
  fprintf (stderr, "searching short `%s'\n", name);
#endif
  while (info)
    {
      int i;
      for (i = 0; i < info->n_args; i++)
	{
#ifdef DEBUG_PRINT
	  fprintf (stderr, " on %s\n", info->args[i].name);
#endif
	  if (!strcmp (info->args_short_names[i], name))
	    {
	      *arg = info->args[i];
	      return;
	    }
	}
      info = info->parent;
    }
  
  arg->type = GTK_TYPE_INVALID;
  return;
}
      
GtkArg*
sgtk_build_args (sgtk_object_info *info, int *n_argsp, repv scm_args,
		 repv protector, char *subr)
{
  int i, n_args = *n_argsp;
  GtkArg *args;
  char *name;
  repv kw, val;
  sgtk_type_info *type_info;

  args = g_new0 (GtkArg, n_args);

  for (i = 0; i < n_args; i++)
    {
      kw = rep_CAR (scm_args);
      val = rep_CADR (scm_args);
      scm_args = rep_CDDR (scm_args);

      if (rep_SYMBOLP (kw))
	name = rep_STR(rep_SYM(kw)->name);
      else
	{
	  fprintf (stderr, "bad keyword\n");
	  n_args -= 1;
	  i -= 1;
	  continue;
	}

      sgtk_find_arg_info (&args[i], info, name);
      if (args[i].type == GTK_TYPE_INVALID)
	{
	  fprintf (stderr, "no such arg for type `%s': %s\n",
		   info->header.name, name);
	  n_args -= 1;
	  i -= 1;
	  continue;
	}

      /* XXX - rethink type info business.  Avoid double lookups. */

      type_info = sgtk_maybe_find_type_info (args[i].type);
      if (type_info && type_info->conversion)
	val = type_info->conversion (val);

      if (!sgtk_valid_arg (&args[i], val))
	{
	  repv throw_args = 
	    rep_LIST_3 (rep_string_dup ("wrong type for"),
			rep_string_dup (gtk_type_name (args[i].type)), val);
	  g_free (args);
	  Fsignal (Qerror, throw_args);
	}
	  
      sgtk_rep_to_arg (&args[i], val, protector);
    }

  *n_argsp = n_args;
  return args;
}

repv
sgtk_gtk_object_new (repv type_obj, repv scm_args)
{
  int n_args;
  sgtk_object_info *info;
  GtkArg *args;
  GtkObject *obj;
  repv scm_obj;

  rep_DECLARE (1, type_obj, type_obj != Qnil && sgtk_valid_type (type_obj));
  n_args = list_length (scm_args);
  rep_DECLARE (2, scm_args, n_args >= 0 && (n_args%2) == 0);
  n_args = n_args/2;

  info = sgtk_find_object_info_from_type (sgtk_rep_to_type (type_obj));
  if (info == 0)
      return Qnil;

  obj = gtk_object_new (info->header.type, NULL);
  scm_obj = sgtk_wrap_gtkobj (obj);
  args = sgtk_build_args (info, &n_args, scm_args, scm_obj, "gtk-object-new");
  gtk_object_setv (obj, n_args, args);
  g_free (args);

  return scm_obj;
}

repv
sgtk_gtk_object_set (repv scm_obj, repv scm_args)
{
  int n_args;
  sgtk_object_info *info;
  GtkArg *args;
  GtkObject *obj;

  rep_DECLARE (1, scm_obj, GTKOBJP(scm_obj));
  n_args = list_length (scm_args);
  rep_DECLARE (2, scm_args, n_args >= 0 && (n_args%2) == 0);
  n_args = n_args/2;

  obj = GTKOBJ_PROXY(scm_obj)->obj;
  info = sgtk_find_object_info_from_type (GTK_OBJECT_TYPE(obj));
  if (info == 0)
      return Qnil;
  
  args = sgtk_build_args (info, &n_args, scm_args, scm_obj, "gtk-object-set");
  gtk_object_setv (obj, n_args, args);
  g_free (args);

  return Qnil;
}

repv
sgtk_gtk_object_get (repv scm_obj, repv argsym)
{
  GtkObject *obj;
  sgtk_object_info *info;
  char *name;
  GtkArg arg;

  rep_DECLARE (1, scm_obj, GTKOBJP(scm_obj));
  rep_DECLARE (2, argsym, rep_SYMBOLP(argsym));

  obj = GTKOBJ_PROXY(scm_obj)->obj;
  info = sgtk_find_object_info_from_type (GTK_OBJECT_TYPE(obj));
  if (info == 0)
      return Qnil;

  name = rep_STR(rep_SYM(argsym)->name);
  sgtk_find_arg_info (&arg, info, name);

  if (arg.type != GTK_TYPE_INVALID)
    gtk_object_getv (obj, 1, &arg);

  if (arg.type == GTK_TYPE_INVALID)
    return Qnil;
  else
    return sgtk_arg_to_rep (&arg, 1);
}



/* Creating new object classes */

GtkType
gtk_class_new (GtkType parent_type, gchar *name)
{
  GtkTypeInfo info = { 0 };
  GtkTypeInfo parent_info;

  if (!gtk_type_get_info (parent_type, &parent_info))
    return GTK_TYPE_INVALID;

  info.type_name = name;
  info.object_size = parent_info.object_size;
  info.class_size = parent_info.class_size;
  info.class_init_func = NULL;
  info.object_init_func = NULL;
#if GTK_MAJOR_VERSION > 1 || GTK_MINOR_VERSION > 0
  info.base_class_init_func = NULL;
#endif

  return gtk_type_unique (parent_type, &info);
}

guint
gtk_signal_new_generic (const gchar     *name,
			GtkSignalRunType signal_flags,
			GtkType          type,
			GtkType          return_type,
			guint            nparams,
			GtkType         *params)
{
  guint signal_id;

  if (GTK_FUNDAMENTAL_TYPE (type) != GTK_TYPE_OBJECT)
    return 0;

  signal_id = gtk_signal_newv (name, signal_flags, type,
			       0, NULL,
			       return_type, nparams, params);
  if (signal_id > 0)
    gtk_object_class_add_signals (gtk_type_class (type),
				  &signal_id, 1);

  return signal_id;
}

void
sgtk_signal_emit (GtkObject *obj, char *name, repv scm_args)
{
  GtkSignalQuery *info;
  guint signal_id, i;
  GtkArg *args;

  signal_id = gtk_signal_lookup (name, GTK_OBJECT_TYPE (obj));
  if (signal_id == 0)
    {
      Fsignal (Qerror, rep_list_2 (rep_string_dup ("no such signal"),
				   rep_string_dup (name)));
      return;
    }

  info = gtk_signal_query (signal_id);
  if (!rep_CONSP(scm_args) || list_length (scm_args) != info->nparams)
    {
      g_free (info);
      Fsignal (Qerror, Fcons (rep_string_dup ("wrong number of signal arguments"), Qnil));
      return;
    }

  args = g_new (GtkArg, info->nparams+1);
  i = 0;
  while (rep_CONSP (scm_args))
    {
      args[i].name = NULL;
      args[i].type = info->params[i];

      if (!sgtk_valid_arg (&args[i], rep_CAR (scm_args)))
	{
	  repv throw_args =
	    rep_LIST_3 (rep_string_dup ("wrong type for"),
			rep_string_dup (gtk_type_name (args[i].type)),
			rep_CAR (scm_args));
	  g_free (args);
	  Fsignal (Qerror, throw_args);
	  return;
	}

      sgtk_rep_to_arg (&args[i], rep_CAR(scm_args), Qt);
      i++;
      scm_args = rep_CDR (scm_args);
    }
  args[i].type = GTK_TYPE_NONE;

  gtk_signal_emitv (obj, signal_id, args);

  g_free (args);
  g_free (info);
}


/* Support rep input handling through gtk_main

   XXX really need to add all _previously_ registered rep inputs
   XXX on initialisation.. */

/* The input_tags table hashes fds to gdk tags; the input_callbacks
   table hashes fds to rep callback function. These should be a single
   table really.. */
static GHashTable *input_tags, *input_callbacks;

static void
sgtk_input_callback (gpointer data, gint fd, GdkInputCondition cond)
{
    void (*func)(int fd) = g_hash_table_lookup (input_callbacks,
						(gpointer) fd);
    if (func != 0)
	(*func) (fd);
    sgtk_callback_postfix ();
}

static void
sgtk_register_input_fd (int fd, void (*callback)(int fd))
{
    if (callback != 0)
    {
	int tag;
	if (input_tags == 0)
	{
	    input_tags = g_hash_table_new ((GHashFunc) gpointer_hash,
					   (GCompareFunc) gpointer_compare);
	    input_callbacks = g_hash_table_new ((GHashFunc) gpointer_hash,
						(GCompareFunc) gpointer_compare);
	}
	tag = gdk_input_add (fd, GDK_INPUT_READ,
			     (GdkInputFunction) sgtk_input_callback, 0);
	g_hash_table_insert (input_tags, (gpointer) fd, (gpointer) tag);
	g_hash_table_insert (input_callbacks,
			     (gpointer) fd, (gpointer) callback);
    }
}

static void
sgtk_deregister_input_fd (int fd)
{
    if (input_tags != 0)
    {
	int tag = (int) g_hash_table_lookup (input_tags, (gpointer) fd);
	gdk_input_remove (tag);
	g_hash_table_remove (input_tags, (gpointer) fd);
	g_hash_table_remove (input_callbacks, (gpointer) fd);
    }
}

/* Mimic the rep idle stuff (every second) using a timeout that is reset
   after each callback/event. */
static int idle_timeout_set = 0, idle_timeout_counter = 0, idle_timeout_tag;

static gint
idle_timeout_callback (gpointer data)
{
    rep_proc_periodically ();
    rep_on_idle (idle_timeout_counter++);
    if (rep_INTERRUPTP)
	gtk_main_quit ();
    else if (rep_redisplay_fun != 0)
	(*rep_redisplay_fun)();
    return 1;
}

static void
reset_idle_timeout (void)
{
    if (idle_timeout_set)
	gtk_timeout_remove (idle_timeout_tag);
    idle_timeout_counter = 0;
    idle_timeout_tag = gtk_timeout_add (1000, idle_timeout_callback, 0);
    idle_timeout_set = TRUE;
}

/* Call this after executing any callbacks that could invoke Lisp code */
void
sgtk_callback_postfix (void)
{
    if (rep_INTERRUPTP)
	gtk_main_quit ();
    else if (rep_redisplay_fun != 0)
	(*rep_redisplay_fun)();
    reset_idle_timeout ();
}

/* This function replaces the standard rep event loop. */
static repv
sgtk_event_loop (void)
{
    while (1)
    {
	if (rep_redisplay_fun != 0)
	    (*rep_redisplay_fun)();

	reset_idle_timeout ();
	gtk_main ();

	rep_proc_periodically ();

	/* Check for exceptional conditions. */
	if(rep_throw_value != rep_NULL)
	{
	    repv result;
	    if(rep_handle_input_exception (&result))
		return result;
	}

#ifdef C_ALLOCA
	/* Using the C implementation of alloca. So garbage collect
	   anything below the current stack depth. */
	alloca(0);
#endif
    }
}

/* Called by librep/src/unix_processes.c whenever SIGCHLD is received
   (from the signal handler) */
static void
sgtk_sigchld_callback (void)
{
    /* XXX I'm hoping that this is safe to call from a signal handler... */

    gtk_main_quit ();
}



/* Initialization */

static int standalone_p = 1;

void
sgtk_set_standalone (int flag)
{
  standalone_p = flag;
}

int
sgtk_is_standalone ()
{
  return standalone_p;
}

DEFUN ("gtk-standalone-p", Fgtk_standalone_p,
       Sgtk_standalone_p, (void), rep_Subr0)
{
  return standalone_p ? Qt : Qnil;
}

DEFSYM (gtk_major_version, "gtk-major-version");
DEFSYM (gtk_minor_version, "gtk-minor-version");
DEFSYM (gtk_micro_version, "gtk-micro-version");
DEFSYM (gtk, "gtk");

static void
sgtk_init_substrate (void)
{
  tc16_gtkobj = rep_register_new_type ("gtk-object", 0,
				       gtkobj_print, gtkobj_print,
				       gtkobj_sweep, gtkobj_mark,
				       gtkobj_marker_hook,
				       0, 0, 0, 0, 0, 0);

  tc16_boxed = rep_register_new_type ("gtk-boxed", 0,
				      boxed_print, boxed_print,
				      boxed_sweep, 0, 0,
				      0, 0, 0, 0, 0, 0);

  global_protects = Qnil;
  
  callback_trampoline = Fcons (Qnil, Qnil);
  rep_mark_static (&callback_trampoline);

  rep_register_input_fd_fun = sgtk_register_input_fd;
  rep_deregister_input_fd_fun = sgtk_deregister_input_fd;
  rep_event_loop_fun = sgtk_event_loop;
  rep_sigchld_fun = sgtk_sigchld_callback;

  /* Need this in case sit-for is called. */
  rep_register_input_fd (ConnectionNumber (gdk_display), 0);

  rep_ADD_SUBR (Sgtk_callback_trampoline);
  rep_ADD_SUBR (Sgtk_standalone_p);
  rep_INTERN (gtk_major_version);
  rep_INTERN (gtk_minor_version);
  rep_INTERN (gtk_micro_version);
  rep_INTERN (gtk);
  rep_SYM (Qgtk_major_version)->value = rep_MAKE_INT (GTK_MAJOR_VERSION);
  rep_SYM (Qgtk_minor_version)->value = rep_MAKE_INT (GTK_MINOR_VERSION);
  rep_SYM (Qgtk_micro_version)->value = rep_MAKE_INT (GTK_MICRO_VERSION);
}

static int sgtk_inited = 0;

void
sgtk_init_with_args (int *argcp, char ***argvp)
{
  if (sgtk_inited)
    return;

  /* XXX - Initialize Gtk only once.  We assume that Gtk has already
     been initialized when Gdk has.  That is not completely correct,
     but the best I can do.

     Actually it shouldn't matter, gtk_init () won't initialise more
     than once.. --jsh */

  if (gdk_display == NULL)
    gtk_init (argcp, argvp);
  else
    standalone_p = 0;			/* a reasonable assumption? --jsh */

  sgtk_init_substrate ();
  sgtk_inited = 1;
}

static char*
xstrdup (char *str)
{
  if (str)
    {
      char *newstr = rep_alloc (strlen(str)+1);
      strcpy (newstr, str);
      return newstr;
    }
  else
    return NULL;
}

static void
make_argv (repv list, int *argc, char ***argv)
{
  static char *argv_storage[1] = { "rep-gtk" };

  int c = list_length (list), i;
  char **v;

  *argv = argv_storage;
  *argc = 1;

  if (c < 0)
    return;

  v = (char **)rep_alloc ((c+1) * sizeof(char**));
  for (i = 0; i < c; i++, list = rep_CDR (list))
    {
      if (!rep_STRINGP (rep_CAR (list)))
	{
	  rep_free ((char *)v);
	  return;
	}
      v[i] = xstrdup (rep_STR (rep_CAR (list)));
    }
  v[c] = NULL;
  
  *argv = v;
  *argc = c;
}

void
sgtk_init ()
{
  int argc;
  char **argv;
  repv head, *last;

  make_argv (Fcons (rep_SYM(Qprogram_name)->value,
		    rep_SYM(Qcommand_line_args)->value), &argc, &argv);
  sgtk_init_with_args (&argc, &argv);

  argc--; argv++;
  head = Qnil;
  last = &head;
  while(argc > 0)
  {
      *last = Fcons(rep_string_dup(*argv), Qnil);
      last = &rep_CDR(*last);
      argc--;
      argv++;
  }
  rep_SYM(Qcommand_line_args)->value = head;
}



/* DL hooks */

repv rep_dl_feature;

repv
rep_dl_init (void)
{
  rep_xsubr **x;

  sgtk_register_type_infos (sgtk_type_infos);

  for (x = sgtk_subrs; *x != 0; x++)
      rep_add_subr (*x);

  rep_dl_feature = Qgtk;
  return Qt;
}
