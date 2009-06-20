MARKDOWN_SOURCES=$(wildcard doc/*.md)
MARKDOWN_TARGETS=$(patsubst doc/%.md,doc/html/%.html,$(MARKDOWN_SOURCES))

HTML_SOURCES=$(wildcard priv/www/*.html)
HTML_TARGETS=$(patsubst priv/www/%.html,priv/www/standalone/%.html,$(HTML_SOURCES))

all: ebin
	(cd src;$(MAKE))

java:
	(cd priv/java;$(MAKE))

standalone: priv/www/standalone $(HTML_TARGETS)

priv/www/standalone/%.html: priv/www/%.html
	priv/tools/onefile.pl $< > $@

priv/www/standalone:
	mkdir -p priv/www/standalone

docs: erlang-docs html-docs

erlang-docs: doc/edoc
	(cd src;$(MAKE) docs)

html-docs: doc/html $(MARKDOWN_TARGETS)

doc/edoc:
	mkdir -p doc/edoc

doc/html:
	mkdir -p doc/html

doc/html/%.html: doc/%.md
	(title=`grep '^# ' $< | head -1 | sed -e 's:^# ::'` ;\
	 t=/tmp/$*.md ;\
	 sed -e "s:@TITLE@:$$title:g" < doc/header.html > $@ ;\
	 python doc/buildtoc.py < $< > $$t ;\
	 markdown $$t >> $@ ;\
	 rm $$t ;\
	 cat doc/footer.html >> $@)

ebin:
	mkdir -p ebin

clean: clean-docs clean-standalone
	(cd src;$(MAKE) clean)
	(cd priv/java;$(MAKE) clean)

clean-docs: clean-html
	rm -rf doc/edoc

clean-html:
	rm -rf doc/html

clean-standalone:
	rm -rf priv/www/standalone
