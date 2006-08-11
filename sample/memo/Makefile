CC	= gcc
CFLAGS	= -Wall -g -I../../src/runtime
LIBS	= -L../../src/runtime -lprcrt -lws2_32 -lwinmm
PRCC	= ../../src/compiler/prcc

TARGET	= memo
SRCS	=	\
	memo.c	\
	memo.out.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

memo.out.c	: memo.prc
	$(PRCC) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) memo.out.c *~
