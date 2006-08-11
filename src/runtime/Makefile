################################################################
#
# Preccs Ver.2 ランタイムライブラリ Makefile
#
# ---
# $Id: Makefile,v 1.5 2006/08/07 09:07:51 hattori Exp $
INSTALLDIR = /usr/local
CC	= gcc
SHELL	= /bin/sh
INCLUDES= -I../../inc
CFLAGS	= -Wall -g $(INCLUDES)

TARGET	= libprcrt.a
SRCS	=		\
	perr.c		\
	alloc.c	\
	gc.c		\
	chan.c		\
	timer.c	\
	proc.c		\
	exec.c		\
	event.c	\
	io.c		\
	dfa.c		\
	sock.c		\
	file.c		\
	wave.c		\
	prcrt.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(AR) rcs $@ $(LIBS) $(OBJS)

clean	:
	rm -fr $(OBJS) $(TARGET)

install	: $(TARGET)
	cp $(TARGET) $(INSTALLDIR)/lib/
	if [ -d $(INSTALLDIR)/include/preccs ];	\
	then \
		cp *.h *.def $(INSTALLDIR)/include/preccs; \
	else \
		mkdir $(INSTALLDIR)/include/preccs; \
		cp *.h *.def $(INSTALLDIR)/include/preccs; \
	fi

clobber	:
	rm -f $(INSTALLDIR)/lib/$(TARGET)
	rm -rf $(INSTALLDIR)/include/preccs
