GUILE ?= guile
SITE = $(shell $(GUILE) -c "(display (%site-dir))")
PREFIX ?= /usr/local

default: .tested

.tested:
	hdt

install:
	install -D -t $(SITE)/battery battery/info.scm
	install -D -t $(PREFIX)/bin battery-info
	install -D -t $(PREFIX)/share/icons/hicolor/scalable/apps/ com.her01n.BatteryInfo.svg
	install -D -t $(PREFIX)/share/applications battery-info.desktop

deploy:
	rsync --update --archive --delete \
		index.html screenshots \
		sykorka.herko.it:/var/www/herko.it/battery-info/

