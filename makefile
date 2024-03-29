GUILE ?= guile
SITE = $(shell $(GUILE) -c "(display (%site-dir))")
SITE_CCACHE = $(shell $(GUILE) -c "(display (%site-ccache-dir))")
PREFIX ?= /usr/local

default: mos compile

test:
	LANG=C hdt

build/%.go: %.scm
	mkdir -p $(shell dirname $@)
	$(GUILE) -L . -c "(compile-file \"$<\" #:output-file \"$@\")"

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

uninstall-mos:
	rm -rf $(foreach language, $(LANGUAGES), locale/$(language)/LC_MESSAGES/battery-info.mo)

install: install-mos
	install --directory $(SITE)/battery
	sed s!"locale"!"$(PREFIX)/share/locale"! <battery/info.scm >$(SITE)/battery/info.scm
	install -D -t $(SITE) dbus.scm gtk.scm --mode=0644
	install -D -t $(SITE_CCACHE)/battery build/battery/info.go
	install -D -t $(SITE_CCACHE) build/dbus.go build/gtk.go
	install -D -t $(PREFIX)/bin battery-info
	install -D -t $(PREFIX)/share/icons/hicolor/scalable/apps/ com.her01n.BatteryInfo.svg --mode=0644
	install -D -t $(PREFIX)/share/applications com.her01n.BatteryInfo.desktop --mode=0644
	install -D -t $(PREFIX)/share/metainfo com.her01n.BatteryInfo.metainfo.xml --mode=0644

uninstall: uninstall-mos
	rm -rf $(SITE)/battery
	rm -rf $(SITE)/dbus.scm $(SITE)/gtk.scm
	rm -rf $(SITE_CCACHE)/battery
	rm -rf $(SITE_CCACHE)/dbus.go $(SITE_CCACHE)/gtk.go
	rm -rf $(PREFIX)/bin/battery-info
	rm -rf $(PREFIX)/share/icons/hicolor/scalable/apps/com.her01n.BatteryInfo.svg
	rm -rf $(PREFIX)/share/applications/com.her01n.BatteryInfo.desktop
	rm -rf $(PREFIX)/share/metainfo/com.her01n.BatteryInfo.metainfo.xml

deploy:
	rsync --update --archive --delete \
		com.her01n.BatteryInfo.flatpakref index.html screenshots \
		sykorka.herko.it:/var/www/herko.it/battery-info/

.PHONY: mos test update-translations list-languages install-mos deploy

