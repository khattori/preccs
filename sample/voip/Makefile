CC	= gcc
CFLAGS	= -Wall -g -I../../src/runtime
LIBS	= -L../../src/runtime -lprcrt -lws2_32 -lwinmm
PRCC	= ../../src/compiler/prcc

TARGET	= voip
SRCS	=	\
	g711.c	\
	voip.c	\
	voip.out.c

OBJS	= $(SRCS:.c=.o)

$(TARGET)	: $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIBS) 

voip.out.c	: voip.prc
	$(PRCC) $< > $@

clean	:
	rm -fr $(OBJS) $(TARGET)

clean-all	:
	rm -fr $(OBJS) $(TARGET) voip.out.c *~
