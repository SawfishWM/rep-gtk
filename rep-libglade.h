/* rep-libglade.h -- 
   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawmill.

   sawmill is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawmill is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawmill; see the file COPYING.   If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef REP_LIBGLADE_H
#define REP_LIBGLADE_H

#include <rep.h>
#include <glade/glade.h>

extern void sgtk_glade_xml_signal_connect (GladeXML *self,
					   char *handler_name, repv func);

extern void sgtk_glade_xml_signal_autoconnect (GladeXML *self);

extern GladeXML *sgtk_glade_xml_new_from_string (repv text, const char *root,
						 const char *domain);

extern char *sgtk_glade_xml_textdomain (GladeXML *xml);

extern void sgtk_init_gtk_libglade_glue (void);

#endif /* REP_LIBGLADE_H */
