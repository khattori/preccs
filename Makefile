all :
	cd src/compiler; touch .depend; make depend; make
	cd src/runtime; make
