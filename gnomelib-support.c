/* gnomelib-support.c -- helper functions for GNOME binding
   $Id$ */

#include <config.h>
#include <assert.h>
#include <gnome.h>
#include "rep-gtk.h"
#include "rep-gnome.h"
#include <string.h>


/* metadata */

#ifdef XXX

GnomeMetadataError_t
sgtk_gnome_metadata_set (const char *file, const char *name, repv data)
{
    if (rep_STRINGP (data))
    {
	return gnome_metadata_set (file, name,
				   rep_STRING_LEN (data), rep_STR (data));
    }
    else
	return GNOME_METADATA_NOT_FOUND;
}

repv
sgtk_gnome_metadata_get (const char *file, const char *name)
{
    int size;
    char *buffer;
    if (gnome_metadata_get (file, name, &size, &buffer) == 0)
    {
	repv ret = rep_string_dupn (buffer, size);
	g_free (buffer);
	return ret;
    }
    else
	return Qnil;
}

repv
sgtk_gnome_metadata_get_fast (const char *file, const char *name)
{
    int size;
    char *buffer;
    if (gnome_metadata_get_fast (file, name, &size, &buffer) == 0)
    {
	repv ret = rep_string_dupn (buffer, size);
	g_free (buffer);
	return ret;
    }
    else
	return Qnil;
}

void
sgtk_gnome_metadata_regex_add (const char *regex, const char *key, repv data)
{
    if (rep_STRINGP (data))
    {
	gnome_metadata_regex_add (regex, key,
				  rep_STRING_LEN (data), rep_STR (data));
    }
}

void
sgtk_gnome_metadata_type_add (const char *regex, const char *key, repv data)
{
    if (rep_STRINGP (data))
    {
	gnome_metadata_type_add (regex, key,
				 rep_STRING_LEN (data), rep_STR (data));
    }
}

#endif /* XXX */


/* dl hooks / init */

repv
rep_dl_init (void)
{
    repv s = rep_push_structure ("gui.gnome.lib");
    /* ::alias:gnomelib gui.gnome.lib:: */
    rep_alias_structure ("gnomelib");
    sgtk_gnome_init_gnome_glue ();
    return rep_pop_structure (s);
}
