MAKEINFOFLAGS = --enable-encoding

langsdir = /$(lang)

MAXIMA_SET_ENV_VARS = $(shell $(AWK) -v v=$(V) -v l=$(lang) 'BEGIN{if(length(l)==2){ l=l "_" toupper(l) }; print v "=" l ".ISO-8859-1"; }')
MAXIMA_ENV_VARS = $(foreach V,LC_ALL LANG LANGUAGE LC_MESSAGES,$(MAXIMA_SET_ENV_VARS))

info_TEXINFOS = maxima.texi

all-local: maxima-index.lisp maxima.html contents.hhc

maxima-index.lisp: maxima.info
	$(MAXIMA_ENV_VARS) $(top_srcdir)/maxima-local --very-quiet --init=/dev/null --batch-string='setup_help_database();print_help_database("$@");'

maxima.html: maxima.texi $(maxima_TEXINFOS)
	perl ../texi2html -split_chapter --lang=$(lang:pt_BR=pt) --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 

contents.hhc: maxima.html
	perl ../create_index

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/$(lang)
genericdirDATA = \
contents.hhc index.hhk header.hhp maxima.hhp

htmlname = maxima
htmlinstdir = $(dochtmldir)/$(lang)
include $(top_srcdir)/common-html.mk

clean-local: clean-info clean-html

clean-info:
	rm -f maxima.info*
	rm -f maxima-index.lisp

clean-html:
	rm -f maxima.html maxima_*.html
	rm -f contents.hhc
	rm -f index.hhk

EXTRA_DIST = maxima-index.lisp $(genericdirDATA)

include ../common-lang.mk

install-info-am: $(INFO_DEPS) maxima-index.lisp
	test -z "$(infodir)$(langsdir)" || mkdir -p -- "$(DESTDIR)$(infodir)$(langsdir)"
	@srcdirstrip=`echo "$(srcdir)" | sed 's|.|.|g'`; \
	list='$(INFO_DEPS)'; \
	for file in $$list; do \
	  case $$file in \
	    $(srcdir)/*) file=`echo "$$file" | sed "s|^$$srcdirstrip/||"`;; \
	  esac; \
	  if test -f $$file; then d=.; else d=$(srcdir); fi; \
	  file_i=`echo "$$file" | sed 's|\.info$$||;s|$$|.i|'`; \
	  for ifile in $$d/$$file $$d/$$file-[0-9] $$d/$$file-[0-9][0-9] \
                       $$d/$$file_i[0-9] $$d/$$file_i[0-9][0-9] ; do \
	    if test -f $$ifile; then \
	      relfile=`echo "$$ifile" | sed 's|^.*/||'`; \
	      echo " $(INSTALL_DATA) '$$ifile' '$(DESTDIR)$(infodir)$(langsdir)/$$relfile'"; \
	      $(INSTALL_DATA) "$$ifile" "$(DESTDIR)$(infodir)$(langsdir)/$$relfile"; \
	    else : ; fi; \
	  done; \
	done
	$(INSTALL_DATA) maxima-index.lisp "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"

uninstall-info-am:
	@list='$(INFO_DEPS)'; \
	for file in $$list; do \
	  relfile=`echo "$$file" | sed 's|^.*/||'`; \
	  relfile_i=`echo "$$relfile" | sed 's|\.info$$||;s|$$|.i|'`; \
	  (if cd "$(DESTDIR)$(infodir)$(langsdir)"; then \
	     echo " cd '$(DESTDIR)$(infodir)$(langsdir)' && rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]"; \
	     rm -f $$relfile $$relfile-[0-9] $$relfile-[0-9][0-9] $$relfile_i[0-9] $$relfile_i[0-9][0-9]; \
	   else :; fi); \
	done
	rm -f "$(DESTDIR)$(infodir)$(langsdir)/maxima-index.lisp"

