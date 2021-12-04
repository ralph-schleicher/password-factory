## Makefile.am --- make file for the password factory.

# Copyright (C) 2020 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * Neither the name of the copyright holder nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

package_service_bin_PROGRAMS = password-factory
package_service_templ_DATA = index.en.html.in index.de.html.in

password_factory_SOURCES = \
password-factory.asd \
package.lisp \
common.lisp \
password.lisp \
special.lisp \
service.lisp \
application.lisp \
$(nil)

EXTRA_DIST = asd-components.sh
BUILT_SOURCES = password-factory.asd password-factory.service
CLEANFILES = password-factory.service

password-factory$(EXEEXT): $(password_factory_SOURCES)
	$(BUILDAPP) \
	--asdf-tree $(srcdir) \
	--load-system password-factory \
	--entry password-factory:main \
	--compress-core \
	--output $@ \
	|| rm -f $@

password-factory.asd: $(srcdir)/password-factory.asd.in $(srcdir)/Makefile.in
	cd $(srcdir) && \
	( sed \
	  -e 's/\@VERSION\@/$(PACKAGE_VERSION)/g' \
	  password-factory.asd.in | \
	  sh ./asd-components.sh $(password_factory_SOURCES) ) > $@~ && \
	mv -f $@~ $@

password-factory.service: $(srcdir)/password-factory.service.in $(srcdir)/Makefile.in
	sed \
	-e 's|[@]USER[@]|$(USER)|g' \
	-e 's|[@]GROUP[@]|$(GROUP)|g' \
	-e 's|[@]bindir[@]|$(package_service_bindir)|g' \
	-e 's|[@]confdir[@]|$(package_service_confdir)|g' \
	-e 's|[@]templdir[@]|$(package_service_templdir)|g' \
	-e 's|[@]basedir[@]|$(package_service_basedir)|g' \
	-e 's|[@]ssl_cert_file[@]|$(ssl_cert_path)|g' \
	-e 's|[@]ssl_key_file[@]|$(ssl_key_path)|g' \
	$(srcdir)/password-factory.service.in > $@~ && \
	mv -f $@~ $@

install-data-local:
	$(mkinstalldirs) $(package_service_confdir)/ssl ; \
	if test x$(ssl_cert_file) = x ; then : ; else \
	    $(install_sh) -c -m 644 $(ssl_cert_file) $(package_service_confdir)/ssl ; \
	fi
	if test x$(ssl_key_file) = x ; then : ; else \
	    $(install_sh) -c -m 640 $(ssl_key_file) $(package_service_confdir)/ssl ; \
	fi
	f=password-factory.service ; \
	if test -f $$f && test `id -u` = 0 && test -d /usr/lib/systemd/system ; \
	then \
	  $(INSTALL_DATA) $$f /usr/lib/systemd/system ; \
	  $(SYSTEMCTL) enable $$f ; \
	fi

install-data-hook:
	chown -R $(USER):$(GROUP) $(package_servicedir)

if MAINTAINER_MODE

### HTML Template

.PHONY: html-local
html-local: index.en.html.in index.de.html.in
# There are two options.  Either
#
#      <link rel="stylesheet" type="text/css" href="style.css.min" />
# or
#      <style type="text/css">
#       ...
#      </style>
#
# When embedding the CSS style sheet, indent the code properly.
#
# Also remove all empty lines in the HTML template.
index%.html.in: index%.html.in.in style.css.min
	cd $(srcdir) ; \
	{ \
	  echo '<style type="text/css">' ; \
	  cat style.css.min | sed -e 's/^/ /' ; \
	  echo '</style>' ; \
	} | \
	sed -e 's/^/  /' > temp.css
	sed -e '/<!-- TMPL_VAR CSS -->/ {' \
	    -e 'r temp.css' \
	    -e 'd' \
	    -e '}' \
	    -e '/^$$/d' \
	$@.in > $@~ ; \
	rm -f temp.css ; \
	mv -f $@~ $@

### CSS Style Sheet

.PHONY: css-local
css-local: style.css.min style.css
# Add a final newline character to the compressed style sheet.
# The sed(1) script joins lines of the form
#
#      @media ...{...{...}
#      }
style.css.min: style.css.in
	cd $(srcdir) ; \
	lessc --clean-css='--s1 -b' $< $@~ ; \
	echo >> $@~ ; \
	sed -i -e ':x /}$$/ { N; s/}\n}/}}/; bx }' $@~ ; \
	mv -f $@~ $@

# Ditto without compression.
style.css: style.css.in
	cd $(srcdir) ; \
	lessc $< $@

### Maintenance

README.html: README.md
	cd $(srcdir) ; \
	markdown README.md > $@~ ; \
	mv -f $@~ $@

check-local:
	cd $(srcdir) ; \
	quicklisp-check-build -sbcl -ccl password-factory

sync: README.html
	~/src/github/github.sh password-factory
endif

## Makefile.am ends here
