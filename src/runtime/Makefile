################################################################
#
# Preccs Ver.2 ランタイムライブラリ Makefile
#
# ---
# $Id: Makefile,v 1.5 2006/08/07 09:07:51 hattori Exp $
INSTALLDIR = /usr/local
CC	= gcc
SHELL	= /bin/sh
INCLUDES= -I`pwd`
CFLAGS	= -Wall -g $(INCLUDES)
LIBS	= /mingw/lib/libws2_32.a /mingw/lib/libwinmm.a

TARGET	= libprcrt.a
SRCS	=		\
	perr.c		\
	alloc.c		\
	gc.c		\
	chan.c		\
	proc.c		\
	exec.c		\
	event.c		\
	dfa.c		\
	prcrt.c
SRCS	+=		\
	unix/timer.c	\
	unix/io.c	\
	file.c	\
	sock.c	\
	wave.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	@if [ ! -d .libs ]; then \
		mkdir .libs; \
	fi
	cd .libs && \
	for i in $(LIBS); \
	do \
		$(AR) x $$i; \
	done
	$(AR) rcs $@ $(OBJS) .libs/*.o

clean	:
	rm -fr $(OBJS) $(TARGET)
	rm -rf .libs
