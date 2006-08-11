CC	= gcc
CFLAGS	= -Wall -g -I../../src/runtime
LIBS	= -L../../src/runtime -lprcrt -lws2_32
PRCC	= ../../src/compiler/prcc

TARGET	= timer
SRCS	=	\
	timer.c	\
	timer.out.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

timer.out.c	: timer.prc
	$(PRCC) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) timer.out.c *~
