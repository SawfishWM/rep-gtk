
%define ver	0.4
%define rel	1

Summary: GTK+ binding for librep Lisp environment
Name: rep-gtk
Version: %{ver}
Release: %{rel}
Requires: librep >= 0.4, gtk+ >= 1.2
Copyright: GPL
Group: Development/Languages
Source: ftp.dcs.warwick.ac.uk:/people/John.Harper/librep/rep-gtk-%{ver}.tar.gz
URL: http://www.dcs.warwick.ac.uk/~john/sw/rep-gtk.html
Packager: John Harper <john@dcs.warwick.ac.uk>

%description
This is a binding of GTK+ for the librep Lisp interpreter. It is based
on Marius Vollmer's guile-gtk package (initially version 0.15, updated
to 0.16), with a new glue-code generator.

%prep
%setup

%build
./configure --prefix %{_prefix} %{_host}
make CFLAGS="$RPM_OPT_FLAGS"

%install
make install

%files
%doc README README.guile-gtk ChangeLog gtk-1.2.defs gdk-1.2.defs
%{_prefix}/libexec/rep/%{_host}/libgtk.so*
%{_prefix}/libexec/rep/%{_host}/libgtk.la
