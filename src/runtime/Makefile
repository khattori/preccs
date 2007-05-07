################################################################
#
# Preccs Ver.2 ランタイムライブラリ Makefile
#
# ---
# $Id: Makefile,v 1.5 2006/08/07 09:07:51 hattori Exp $
CC	= gcc
SHELL	= /bin/sh
INCLUDES= -I.
LDFLAGS	= -g -shared

UNAME	= $(shell uname)
ifneq (,$(findstring Linux,$(UNAME)))
OS	= unix
CFLAGS	= -Wall -g $(INCLUDES) -fPIC
LIBS	= -lrt
TARGET	= libprcrt.so
else
OS	= win32
CFLAGS	= -Wall -g $(INCLUDES)
DEF	= prcrt.def
IMPLIB	= libprcrt.a
LIBS	= -lws2_32 -Wl,--output-def,$(DEF),--out-implib,$(IMPLIB)
TARGET	= prcrt.dll
endif

SRCS	=		\
	perr.c		\
	alloc.c	\
	gc.c		\
	chan.c		\
	proc.c		\
	exec.c		\
	event.c	\
	dfa.c		\
	prcrt.c	\
	${OS}/io.c	\
	${OS}/sock.c	\
	${OS}/file.c	\
	${OS}/timer.c

ifeq ($(OS),win32)
SRCS	+= $(OS)/strerror.c
endif

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean	:
	@rm -fr $(OBJS) $(TARGET) $(IMPLIB) $(DEF)
