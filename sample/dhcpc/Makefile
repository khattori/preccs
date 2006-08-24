CC	= gcc
CFLAGS	= -Wall -g -I../../src/runtime
LIBS	= -L../../src/runtime -lprcrt -lws2_32 -lwinmm
PRCC	= ../../src/compiler/prcc

TARGET	= dhcpc
SRCS	=	\
	dhcpc.c	\
	dhcpc.out.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

dhcpc.out.c	: dhcpc.prc
	$(PRCC) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) dhcpc.out.c *~
