CC = gcc -std=gnu99

#X_LIB_DIR = -L/usr/X11R6/lib/
#X_HEADER_DIR = -I /usr/X11R6/include/

X_LIB_DIR = -L /opt/xorg/lib/
X_HEADER_DIR = -I /opt/xorg/include/


# These may be overridden by make invocators
DESTDIR  = 
OPTFLAGS = -fomit-frame-pointer 


# These are set by configure
CFLAGS = -Wall -g -O2 -I/usr/lib/gauche/0.8.13/include $(OPTFLAGS) $(X_HEADER_DIR)
LDFLAGS = -Wl,-Bsymbolic-functions   -shared -o
X_LIBS= -lX11 -lfork -lxkbfile -lXi
# -lXtst
# mmc: fixme!
LIBS = -L/usr/lib/gauche/0.8.13/i486-pc-linux-gnu  -lgauche -ldl -lcrypt -lutil -lm  -lpthread  $(X_LIBS)
GOSH = /usr/bin/gosh
GAUCHE_CONFIG = /usr/bin/gauche-config
GAUCHE_TOP = @GAUCHE_TOP@
INSTALL = /usr/bin/install -c

ARCHFILES = xlib.so

SCMFILES = xlib.scm  $(wildcard xlib/*scm xlib/xkb/*scm)
BINFILES = $(wildcard bin/*.scm)

HEADERS =

TARGET = xlib.so
OBJS = xlib_head.o xlib.o xlib_tail.o  xlib-lib.o
CONFIG_GENERATED = Makefile config.cache config.log config.status\
		   autom4te*.cache xlib_head.c xlib_tail.c

INSTALL_TYPE = sys
HEADER_INSTALL_DIR  = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)incdir`/
SCM_INSTALL_DIR     = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`/
ARCH_INSTALL_DIR    = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)archdir`/
BIN_INSTALL_DIR = /usr/bin

GENSTUB = mmc/mmgenstub

all : $(TARGET)

xlib.so : $(OBJS)
	$(CC) $(X_LIB_DIR) $(LDFLAGS) xlib.so $(OBJS) $(LIBS)

xlib.c : xlib.stub xlib-lib.h
	$(GOSH) $(GENSTUB) xlib.stub || (rm $@ ; false)

xlib-lib.o:  xlib-lib.c  xlib-lib.h
test : all
	@rm -f test.log
	$(GOSH) -I. test.scm > test.log

install : all
	$(INSTALL) -d $(HEADER_INSTALL_DIR)
	$(INSTALL) -d $(SCM_INSTALL_DIR)
	$(INSTALL) -d $(ARCH_INSTALL_DIR)
	@for f in $(HEADERS) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -m 444 $$f $(HEADER_INSTALL_DIR); \
	  fi; \
	done
	echo ahoj $(SCMFILES)
	@for f in $(SCMFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -v -D -m 444 $$f $(SCM_INSTALL_DIR)/$$f; \
	  fi; \
	done
	@for f in $(ARCHFILES) _end; do \
	  if test $$f != _end; then \
	    $(INSTALL) -vm 555 $$f $(ARCH_INSTALL_DIR); \
	  fi; \
	done
	install -v -D --directory $(DESTDIR)$(BIN_INSTALL_DIR)
	@for p in $(BINFILES); do \
	  $(INSTALL) -v -m 555 $$p $(DESTDIR)$(BIN_INSTALL_DIR) ; \
	done

clean :
	rm -rf core $(TARGET) $(OBJS) *~ test.log so_locations xlib.c

distclean : clean
	rm -rf $(CONFIG_GENERATED)

realclean : clean
	rm -rf $(CONFIG_GENERATED) configure

# 'link' creates symlinks from source tree to extension modules, so that
# it can be tested within the source tree.  'unlink' removes them.
# these are only for developer's.

link : $(TARGET) $(SCMFILES)
	$(GOSH) ../xlink -d gauche -l $(TARGET) $(SCMFILES)

unlink :
	-$(GOSH) ../xlink -d gauche -u $(TARGET) $(SCMFILES)


version=$(shell cat VERSION)
package=gauche-xlib
##  for the maintainer/distributor:
tar:    clean
	make -f ../makefile.packages  version=$(version) package=$(package)

