CC	= gcc
CFLAGS	= -Wall -g -I../../src/runtime
LIBS	= -L../../src/runtime -lprcrt -lws2_32 -lwinmm
PRCC	= ../../src/compiler/prcc

TARGET	= loop
SRCS	=	\
	loop.c	\
	loop.out.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

loop.out.c	: loop.prc
	$(PRCC) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) loop.out.c *~
