CC	= gcc
CFLAGS	= -Wall -g -I../../src/runtime
LIBS	= -L../../src/runtime -lprcrt
PRCC	= ../../src/compiler/prcc

TARGET	= iotest
SRCS	=	\
	iotest.c	\
	iotest.out.c

OBJS	= $(SRCS:.c=.o)

.SUFFIXES: .prc .out.c

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

.prc.out.c:
	$(PRCC) $(PFLAGS) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) *.out.c *~
