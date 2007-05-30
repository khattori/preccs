SHELL	= /bin/sh
UNAME	= $(shell uname)

ifeq (,$(findstring Linux,$(UNAME)))
TARGET	:= $(addsuffix .exe,$(TARGET))
endif

CC	= gcc
CFLAGS	= -Wall -g -I../../include
LIBS	:= $(LIBS) -L../../src/runtime -lprcrt
PRCC	= ../../src/compiler/prcc

OBJS	= $(SRCS:.c=.o)

.SUFFIXES: .prc .out.c

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

.prc.out.c:
	$(PRCC) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) *.out.c *~
