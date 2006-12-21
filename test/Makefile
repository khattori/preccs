CC	= gcc
CFLAGS	= -Wall -g -I../src/runtime
LIBS	= -L../src/runtime -lprcrt
PRCC	= ../src/compiler/prcc

TARGET	= test
SRCS	=	\
	test.c	\
	test.out.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

test.out.c	: test.prc
	$(PRCC) $(PFLAGS) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) test.out.c *~
