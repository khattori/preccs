################################################################
#
# Preccs Ver.2 ランタイムライブラリ Makefile
#
# ---
# $Id: Makefile,v 1.5 2006/08/07 09:07:51 hattori Exp $
INSTALLDIR = /usr/local
CC	= gcc
SHELL	= /bin/sh
INCLUDES= -I`pwd` -fPIC
CFLAGS	= -Wall -g $(INCLUDES)
LIBS	= -lrt

TARGET	= libprcrt.so
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
	unix/file.c	\
	unix/sock.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -shared -o $@ $(OBJS) $(LIBS)

clean	:
	rm -fr $(OBJS) $(TARGET)
	rm -rf .libs
