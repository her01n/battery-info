GUILE ?= guile
SITE = $(shell $(GUILE) -c "(display (%site-dir))")
SITE_CCACHE = $(shell $(GUILE) -c "(display (%site-ccache-dir))")
PREFIX ?= /usr/local

default: mos compile

test:
	LANG=C hdt

build/%.go: %.scm
	mkdir -p $(shell dirname $@)
	$(GUILE) -c "(compile-file \"$<\" #:output-file \"$@\")"

compile: build/battery/info.go build/dbus.go build/gtk.go

LANGUAGES = $(patsubst ./po/%.po,%,$(wildcard ./po/*.po))

list-languages:
	echo $(LANGUAGES)

battery-info.pot: battery/info.scm
	xgettext battery/info.scm -o $@

po/%.po: battery-info.pot
	msgmerge --update $@ $<

update-translations: $(foreach language,$(LANGUAGES), po/$(language).po)

locale/%/LC_MESSAGES/battery-info.mo: po/%.po
	mkdir -p $(shell dirname $@)
	msgfmt $^ -o $@

mos: $(patsubst %,locale/%/LC_MESSAGES/battery-info.mo,$(LANGUAGES))

clean:
	rm -rf build locale

install-mo-%: locale/%/LC_MESSAGES/battery-info.mo
	install -D $< $(PREFIX)/$<

install-mos: $(foreach language, $(LANGUAGES), install-mo-$(language))

install: install-mos
	install --directory $(SITE)/battery
	sed s!"locale"!"$(PREFIX)/share/locale"! <battery/info.scm >$(SITE)/battery/info.scm
	install -D -t $(SITE) dbus.scm --mode=0644
	install -D -t $(SITE) gtk.scm --mode=0644
	install build/battery/info.go --target-directory=$(SITE_CCACHE)/info
	install build/dbus.go build/gtk.go --target-directory=$(SITE_CCACHE)
	install -D -t $(PREFIX)/bin battery-info
	install -D -t $(PREFIX)/share/icons/hicolor/scalable/apps/ com.her01n.BatteryInfo.svg --mode=0644
	install -D -t $(PREFIX)/share/applications com.her01n.BatteryInfo.desktop --mode=0644
	install -D -t $(PREFIX)/share/metainfo com.her01n.BatteryInfo.metainfo.xml --mode=0644

deploy:
	rsync --update --archive --delete \
		com.her01n.BatteryInfo.flatpakref index.html screenshots \
		sykorka.herko.it:/var/www/herko.it/battery-info/

.PHONY: mos test update-translations list-languages install-mos deploy

