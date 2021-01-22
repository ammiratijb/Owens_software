
FORTRAN=gfortran
FFLAGS=-O -ffixed-line-length-132

LIB=../lib
INCLUDE=../Include_Modules

ROUTINES= ask.o \
asktxt.o \
azdist.o \
blank.o \
coef8.o \
coefsh.o \
conversion_moveout.o \
coord.o \
daymo.o \
dfftr.o \
dot.o \
doy.o \
endian_conversion.o \
fsorce.o \
iask.o \
iniocm.o \
juli.o \
locast.o \
lpyr.o \
max.o \
minmax.o \
myfft.o \
npowr2.o \
period.o \
ptrvlt.o \
rdlyrs.o \
rotseis.o \
sac_access.o \
sacio.o \
seisio.o \
sinc.o \
sit2.o \
uscore.o \
yesno.o \
zeerow.o

ask.o: ask.f 
	 ${FORTRAN} ${FFLAGS} -c ask.f
asktxt.o: asktxt.f 
	 ${FORTRAN} ${FFLAGS} -c asktxt.f
azdist.o: azdist.f 
	 ${FORTRAN} ${FFLAGS} -c azdist.f
blank.o: blank.f 
	 ${FORTRAN} ${FFLAGS} -c blank.f
coef8.o: coef8.f 
	 ${FORTRAN} ${FFLAGS} -c coef8.f
coefsh.o: coefsh.f 
	 ${FORTRAN} ${FFLAGS} -c coefsh.f
conversion_moveout.o: conversion_moveout.f 
	 ${FORTRAN} ${FFLAGS} -c conversion_moveout.f
coord.o: coord.f 
	 ${FORTRAN} ${FFLAGS} -c coord.f
daymo.o: daymo.f 
	 ${FORTRAN} ${FFLAGS} -c daymo.f
dfftr.o: dfftr.f 
	 ${FORTRAN} ${FFLAGS} -c dfftr.f
dot.o: dot.f 
	 ${FORTRAN} ${FFLAGS} -c dot.f
doy.o: doy.f 
	 ${FORTRAN} ${FFLAGS} -c doy.f
endian_conversion.o: endian_conversion.f 
	 ${FORTRAN} ${FFLAGS} -c endian_conversion.f
fsorce.o: fsorce.f 
	 ${FORTRAN} ${FFLAGS} -c fsorce.f
iask.o: iask.f 
	 ${FORTRAN} ${FFLAGS} -c iask.f
iniocm.o: iniocm.f 
	 ${FORTRAN} ${FFLAGS} -c iniocm.f
juli.o: juli.f 
	 ${FORTRAN} ${FFLAGS} -c juli.f
locast.o: locast.f 
	 ${FORTRAN} ${FFLAGS} -c locast.f
lpyr.o: lpyr.f 
	 ${FORTRAN} ${FFLAGS} -c lpyr.f
max.o: max.f 
	 ${FORTRAN} ${FFLAGS} -c max.f
minmax.o: minmax.f 
	 ${FORTRAN} ${FFLAGS} -c minmax.f
myfft.o: myfft.f 
	 ${FORTRAN} ${FFLAGS} -c myfft.f
npowr2.o: npowr2.f 
	 ${FORTRAN} ${FFLAGS} -c npowr2.f
period.o: period.f 
	 ${FORTRAN} ${FFLAGS} -c period.f
ptrvlt.o: ptrvlt.f 
	 ${FORTRAN} ${FFLAGS} -c ptrvlt.f
rdlyrs.o: rdlyrs.f 
	 ${FORTRAN} ${FFLAGS} -c rdlyrs.f
rotseis.o: rotseis.f 
	 ${FORTRAN} ${FFLAGS} -c rotseis.f
sac_access.o: sac_access.f 
	 ${FORTRAN} ${FFLAGS} -c sac_access.f
sacio.o: sacio.f 
	 ${FORTRAN} ${FFLAGS} -c sacio.f
seisio.o: seisio.f 
	 ${FORTRAN} ${FFLAGS} -c seisio.f
sinc.o: sinc.f 
	 ${FORTRAN} ${FFLAGS} -c sinc.f
sit2.o: sit2.f 
	 ${FORTRAN} ${FFLAGS} -c sit2.f
uscore.o: uscore.f 
	 ${FORTRAN} ${FFLAGS} -c uscore.f
yesno.o: yesno.f 
	 ${FORTRAN} ${FFLAGS} -c yesno.f
zeerow.o: zeerow.f 
	 ${FORTRAN} ${FFLAGS} -c zeerow.f
#
# for easy making
#
ask: ask.o
asktxt: asktxt.o
azdist: azdist.o
blank: blank.o
coef8: coef8.o
coefsh: coefsh.o
conversion_moveout: conversion_moveout.o
coord: coord.o
daymo: daymo.o
dfftr: dfftr.o
dot: dot.o
doy: doy.o
endian_conversion: endian_conversion.o
fsorce: fsorce.o
iask: iask.o
iniocm: iniocm.o
juli: juli.o
locast: locast.o
lpyr: lpyr.o
max: max.o
minmax: minmax.o
myfft: myfft.o
npowr2: npowr2.o
period: period.o
ptrvlt: ptrvlt.o
rdlyrs: rdlyrs.o
rotseis: rotseis.o
sac_access: sac_access.o
sacio: sacio.o
seisio: seisio.o
sinc: sinc.o
sit2: sit2.o
uscore: uscore.o
yesno: yesno.o
zeerow: zeerow.o
#
# make the library
#

tjolib: ${ROUTINES}
	ar qv libtjo.a *.o

# 
# test before doing an install ... codes in ./Tests
#
install:
	cp sac_access.mod ${INCLUDE}/sac_access.mod
	cp convert_endian.mod ${INCLUDE}/convert_endian.mod
	cp libtjo.a ${LIB}
	cd ${LIB}
	ranlib libtjo.a

clean:
	/bin/rm *.o sac_access.mod convert_endian.mod libtjo.a
