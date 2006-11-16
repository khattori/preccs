all :
	cd src/compiler; touch .depend; make depend; make
	cd src/runtime; make

clean :
	cd src/compiler; make clean
	cd src/runtime; make clean
