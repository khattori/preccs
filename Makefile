PREFIX=/usr/local

all :
	cd src/compiler; touch .depend; make depend; make
	cd src/runtime; make
	mkdir -p include/preccs
	cp src/runtime/*.h include/preccs/

install :
	cp src/compiler/prcc ${PREFIX}/bin/
	mkdir -p ${PREFIX}/include/preccs
	cp src/runtime/*.h ${PREFIX}/include/preccs/
	cp src/runtime/libprcrt.so ${PREFIX}/lib/
	/sbin/ldconfig -n ${PREFIX}/lib
	
clean :
	cd src/compiler; make clean
	cd src/runtime; make clean
	rm -rf include
