################################################################
#
# Preccs Ver.2 ランタイムライブラリ Makefile
#
# ---
# $Id: Makefile,v 1.5 2006/08/07 09:07:51 hattori Exp $
CC	= gcc
SHELL	= /bin/sh
INCLUDES= -I.

UNAME	= $(shell uname)
ifneq (,$(findstring Linux,$(UNAME)))
OS	= unix
CFLAGS	= -Wall -g $(INCLUDES) -fPIC
LDFLAGS	= -g -shared
LIBS	= -lrt
TARGET	= libprcrt.so
DLLWRAP	=
else
OS	= win32
CFLAGS	= -Wall -g $(INCLUDES)
LDFLAGS	= -g
LIBS	= -lws2_32
DEF	= prcrt.def
TARGET	= prcrt.dll
DLLWRAP	= dllwrap -k -def $(DEF) --driver-name
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
	$(DLLWRAP) $(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean	:
	@rm -fr $(OBJS) $(TARGET)


