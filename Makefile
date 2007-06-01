SHELL	= /bin/sh
UNAME	= $(shell uname)

ifneq (,$(findstring Linux,$(UNAME)))
PREFIX	= /usr/local
RTTARGET= libprcrt.so
CLTARGET= prcc	
FINALIZE= /sbin/ldconfig -n ${PREFIX}/lib
else
PREFIX	= /mingw
RTTARGET= libprcrt.a
CLTARGET= prcc.exe
FINALIZE= cp src/runtime/prcrt.dll ${SYSTEMROOT}/system32/
endif

all :
	cd src/compiler; touch .depend; make depend; make
	cd src/runtime; make
	mkdir -p include/preccs
	cp src/runtime/*.h include/preccs/

install :
	cp src/compiler/${CLTARGET} ${PREFIX}/bin/
	mkdir -p ${PREFIX}/include/preccs
	cp src/runtime/*.h ${PREFIX}/include/preccs/
	cp src/runtime/${RTTARGET} ${PREFIX}/lib/
	${FINALIZE}

clean :
	cd src/compiler; make clean
	cd src/runtime; make clean
	rm -rf include
