################################################################
#
# Preccs Ver.2 ランタイムライブラリ Makefile
#
# ---
# $Id: Makefile,v 1.5 2006/08/07 09:07:51 hattori Exp $
CC	= gcc
SHELL	= /bin/sh
CFLAGS	= -Wall -g $(INCLUDES) -fPIC
INCLUDES= -I.
LIBS	= -lrt
INSTALLDIR = /usr/local

SRCS	=		\
	perr.c		\
	alloc.c		\
	gc.c		\
	chan.c		\
	proc.c		\
	exec.c		\
	event.c		\
	dfa.c		\
	prcrt.c		\
	unix/io.c	\
	unix/sock.c	\
	unix/file.c	\
	unix/timer.c

TARGET	= libprcrt.so

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -shared -o $@ $(OBJS) $(LIBS)

clean	:
	rm -fr $(OBJS) $(TARGET)
	rm -rf .libs


