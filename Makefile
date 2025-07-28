PREFIX         ?= /usr
GUILE_SITEDIR  ?= $(PREFIX)/share/guile/site/3.0
PROJECT        := cpnet
SRC_DIR        := $(PROJECT)
DESTDIR        ?=

.PHONY: all build install uninstall clean

all: build

build:
	@echo "Compiling all cpnet modules..."
	guile -L . -c '(use-modules (cpnet category) (cpnet core) (cpnet functor) (cpnet nt) (cpnet runtime) (cpnet architecture) (cpnet detail))'

install: build
	@echo "Installing only .scm files to $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)"
	install -d $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)
	find $(SRC_DIR) -maxdepth 1 -type f -name '*.scm' \
	   -exec install -m 644 {} $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)/ \;

uninstall:
	@echo "Uninstalling from $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)"
	rm -rf $(DESTDIR)$(GUILE_SITEDIR)/$(PROJECT)

clean:
	@echo "Cleaning compiled files and cache"
	rm -f $(SRC_DIR)/*.go
	-rm -rf $(HOME)/.cache/guile/ccache
