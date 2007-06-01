CC	= gcc
CFLAGS	= -Wall -g -I../include
LIBS	= -L../src/runtime -lprcrt
PRCC	= ../src/compiler/prcc

TARGET	= test
SRCS	=	\
	test.c	\
	test.out.c

OBJS	= $(SRCS:.c=.o)

.SUFFIXES : .prc .out.c
.prc.out.c:
	$(PRCC) $(PFLAGS) $< > $@

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) *.out.c *~
