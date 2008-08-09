/* Copyright (C) 1997, 1998, 1999 Marius Vollmer
 * Copyright (C) 1999-2000 John Harper <john@dcs.warwick.ac.uk>
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
#include <glib.h>
#include "rep-gtk.h"
#include <string.h>
#include <limits.h>



/* Hacking the basic types --jsh */

static inline int
valid_int_type (repv obj)
{
    return rep_INTEGERP (obj) || rep_LONG_INTP (obj);
}

int
sgtk_valid_int (repv obj)
{
    return valid_int_type (obj);
}

int
sgtk_valid_uint (repv obj)
{
    return valid_int_type (obj);
}

int
sgtk_valid_long (repv obj)
{
    return valid_int_type (obj);
}

int
sgtk_valid_ulong (repv obj)
{
    return valid_int_type (obj);
}

int
sgtk_valid_char (repv obj)
{
    return rep_INTP (obj);
}

repv
sgtk_uint_to_rep (unsigned long x)
{
    return rep_make_long_uint (x);
}

repv
sgtk_int_to_rep (long x)
{
    return rep_make_long_int (x);
}

repv
sgtk_long_to_rep (long x)
{
    return rep_make_long_int (x);
}

repv
sgtk_ulong_to_rep (unsigned long x)
{
    return rep_make_long_uint (x);
}

guint
sgtk_rep_to_uint (repv obj)
{
    return rep_get_long_uint (obj);
}

gint
sgtk_rep_to_int (repv obj)
{
    return rep_get_long_int (obj);
}

gulong
sgtk_rep_to_ulong (repv obj)
{
    return rep_get_long_uint (obj);
}

glong
sgtk_rep_to_long (repv obj)
{
    return rep_get_long_int (obj);
}

gchar
sgtk_rep_to_char (repv obj)
{
    return rep_INT (obj);
}

repv
sgtk_char_to_rep (gchar c)
{
    return rep_MAKE_INT (c);
}

char *
sgtk_rep_to_string (repv obj)
{
    return rep_STRINGP (obj) ? rep_STR (obj) : (char *)"";
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
sgtk_static_string_to_rep (const char *x)
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

repv
sgtk_fd_to_rep (int fd)
{
    if (fd < 0)
	return Qnil;
    else
	return rep_file_fdopen (fd, "w+");
}

int
sgtk_valid_pointer (repv obj)
{
    return obj == Qnil || rep_INTEGERP (obj) || rep_LONG_INTP (obj);
}

void *
sgtk_rep_to_pointer (repv obj)
{
    if (obj == Qnil)
	return NULL;
    else
	return (void *) rep_get_long_uint (obj);
}

repv
sgtk_pointer_to_rep (void *ptr)
{
    repv data = rep_VAL (ptr);
    if (data == 0)
	return Qnil;
    else if (data > rep_LISP_MAX_INT)
	/* could use a bignum, but cons is more efficient */
	return rep_MAKE_LONG_INT (data);
    else
	return rep_MAKE_INT (data);
}

static int
list_length (repv list)
{
    repv len = Flength (list);
    return (len && rep_INTP (len)) ? rep_INT (len) : 0;
}

/* namespace fuckage. needed so we can represent GObject base class */
GType
gobject_get_type (void)
{
  return G_TYPE_OBJECT;
}


/* Floats. */

int
sgtk_valid_float (repv obj)
{
  return rep_NUMERICP (obj);
}

gfloat
sgtk_rep_to_float (repv obj)
{
  return rep_get_float (obj);
}

repv
sgtk_float_to_rep (gfloat f)
{
  return rep_make_float (f, rep_FALSE);
}

int
sgtk_valid_double (repv obj)
{
  return rep_NUMERICP (obj);
}

double
sgtk_rep_to_double (repv obj)
{
  return rep_get_float (obj);
}

repv
sgtk_double_to_rep (double f)
{
  return rep_make_float (f, rep_FALSE);
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
	  else
	    (*tail)->data = NULL;
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
	  else
	    (*tail)->data = NULL;
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
	    tail = n;
	  }
	if (fromscm)
	  fromscm (rep_CAR (obj), &(n->data));
	else
	  n->data = NULL;
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
	      tail = n;
	    }
	  if (fromscm)
	    fromscm (elts[i], &(n->data));
	  else
	    n->data = NULL;
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
      res.vec = rep_alloc ((res.count + 1) * sz);
      if (fromscm)
	{
	  for (i = 0, ptr = res.vec; i < res.count; i++, ptr += sz)
	    {
	      fromscm (rep_CAR (obj), ptr);
	      obj = rep_CDR(obj);
	    }
	}
      else
	memset (res.vec, 0, res.count * sz);
    }
  else if (rep_VECTORP(obj))
    {
      repv *elts = rep_VECT(obj)->array;
      res.count = rep_VECT_LEN (obj);
      res.vec = rep_alloc ((res.count + 1) * sz);
      if (fromscm)
	{
	  for (i = 0, ptr = res.vec; i < res.count; i++, ptr += sz)
	    fromscm (elts[i], ptr);
	}
      else
	memset (res.vec, 0, res.count * sz);
    }
  /* make all vectors zero terminated, makes `tvec' easier to implement */
  memset (((char *)res.vec) + res.count * sz, 0, sz);
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

repv
sgtk_cvec_to_rep (sgtk_cvec *cvec, repv (*toscm)(void *), size_t sz)
{
    int len = cvec->count, i;
    repv obj = Fmake_vector (rep_MAKE_INT(len), Qnil);
    repv *elts = rep_VECT (obj)->array;
    char *ptr;

    for (i = 0, ptr = cvec->vec; i < len; i++, ptr += sz)
	elts[i] = toscm (ptr);

    g_free (cvec->vec);
    return obj;
}


/* dl hooks */

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("gui.gtk-2.types");
    return rep_pop_structure (tem);
}
