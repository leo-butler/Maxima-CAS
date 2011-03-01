maxima_flags =  --very-quiet --init=/dev/null

MAKEINFOFLAGS = --enable-encoding --document-language=$(lang).utf8
no_recode = true

TEXINFO_TEX=../$(lang)/texinfo.tex

langsdir = /$(lang).utf8
origlangsdir = ../$(lang)

urecode = true

fcharset = ISO-8859-1
tcharset = UTF-8

fhtmlcharset = iso-8859-1
thtmlcharset = utf-8

maxima.texi: $(origlangsdir)/maxima.texi
	rm -f *.texi
	cp -p $(origlangsdir)/*.texi .
	sed -i -e "s|^@documentencoding $(fcharset)|@documentencoding $(tcharset)|ig" *.texi 

maxima-index.lisp: maxima.info
	$(top_srcdir)/maxima-local $(maxima_flags) --batch-string='setup_help_database();print_help_database("$@");'

maxima.html: maxima.texi
	rm -f maxima*.html 2>/dev/null
	perl ../texi2html -split_chapter --lang=$(lang) --output=. --css-include=../manual.css --init-file ../texi2html.init maxima.texi 
	for f in maxima*.html; do \
	    if test x$(urecode) = xtrue ; then \
	        recode $(fcharset)..$(tcharset) $$f ; \
	    else \
	        rm -f foo.$$f 2>/dev/null ; \
	        iconv -f $(fcharset) -t $(tcharset) $$f > foo.$$f ; \
	        mv -f foo.$$f $$f ; \
	    fi; \
	done
	for f in maxima*.html; do \
	    rm -f foo.$$f 2>/dev/null ; \
	    sed -e "s|charset=$(fhtmlcharset)|charset=$(thtmlcharset)|" < $$f > foo.$$f ; \
	    mv -f foo.$$f $$f ; \
	done

include $(top_srcdir)/common.mk
genericdir = $(dochtmldir)/$(lang).utf8
genericdirDATA = \
contents.hhc index.hhk header.hhp

EXTRA_DIST = maxima-index.lisp $(genericdirDATA)

