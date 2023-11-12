GUILE ?= guile
SITE = $(shell $(GUILE) -c "(display (%site-dir))")
PREFIX ?= /usr/local

default: mos .tested

.tested:
	LANG=C hdt

battery-info.pot:
	xgettext battery/info.scm -o $@

LANGUAGES = $(patsubst ./po/%.po,%,$(wildcard ./po/*.po))

update-translations: battery-info.pot
	$(foreach language,$(LANGUAGES),msgmerge --update po/$(language).po battery-info.pot)

locale/%/LC_MESSAGES/battery-info.mo: po/%.po
	mkdir -p $(shell dirname $@)
	msgfmt $^ -o $@

mos: $(patsubst %,locale/%/LC_MESSAGES/battery-info.mo,$(LANGUAGES))

list-languages:
	echo $(LANGUAGES)

install-mos:
	$(foreach language,$(LANGUAGES),install -D locale/$(language)/LC_MESSAGES/battery-info.mo $(PREFIX)/share/locale/$(language)/LC_MESSAGES/battery-info.mo)

install: install-mos
	install --directory $(SITE)/battery
	sed s!"locale"!"$(PREFIX)/share/locale"! <battery/info.scm >$(SITE)/battery/info.scm
	install -D -t $(SITE) dbus.scm --mode=0644
	install -D -t $(SITE) gtk.scm --mode=0644
	install -D -t $(PREFIX)/bin battery-info
	install -D -t $(PREFIX)/share/icons/hicolor/scalable/apps/ com.her01n.BatteryInfo.svg --mode=0644
	install -D -t $(PREFIX)/share/applications com.her01n.BatteryInfo.desktop --mode=0644
	install -D -t $(PREFIX)/share/metainfo com.her01n.BatteryInfo.metainfo.xml --mode=0644

deploy:
	rsync --update --archive --delete \
		index.html screenshots \
		sykorka.herko.it:/var/www/herko.it/battery-info/

