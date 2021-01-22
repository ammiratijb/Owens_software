FORTRAN=gfortran
FFLAGS=-O -ffixed-line-length-132

BIN=../../bin
LIB=../../TJO_Fortran_Library/lib
INCLUDE=../../TJO_Fortran_Library/Include_Modules


ROUTINES= ampcal.o \
anom.o \
mkseis.o \
norvec.o \
raydwn.o \
rayfin.o \
rayup.o \
rcomp.o \
rtoi.o \
snell.o \
timcor.o \
timdis.o \
trnsmt.o \
wrtray.o

ampcal.o: ampcal.f
	${FORTRAN} ${FFLAGS} -c ampcal.f
anom.o: anom.f
	${FORTRAN} ${FFLAGS} -c anom.f
mkseis.o: mkseis.f
	${FORTRAN} ${FFLAGS} -c mkseis.f
norvec.o: norvec.f
	${FORTRAN} ${FFLAGS} -c norvec.f
raydwn.o: raydwn.f
	${FORTRAN} ${FFLAGS} -c raydwn.f
rayfin.o: rayfin.f
	${FORTRAN} ${FFLAGS} -c rayfin.f
rayup.o:  rayup.f
	${FORTRAN} ${FFLAGS} -c rayup.f
rcomp.o:  rcomp.f
	${FORTRAN} ${FFLAGS} -c rcomp.f
rtoi.o:   rtoi.f
	${FORTRAN} ${FFLAGS} -c rtoi.f
snell.o:  snell.f
	${FORTRAN} ${FFLAGS} -c snell.f
timcor.o: timcor.f
	${FORTRAN} ${FFLAGS} -c timcor.f
timdis.o: timdis.f
	${FORTRAN} ${FFLAGS} -c timdis.f
trnsmt.o: trnsmt.f
	${FORTRAN} ${FFLAGS} -c trnsmt.f
wrtray.o: wrtray.f
	${FORTRAN} ${FFLAGS} -c wrtray.f


ray3d:  ray3d_new.f ${ROUTINES}
	${FORTRAN} ${FFLAGS} -I${INCLUDE} ray3d_new.f ${ROUTINES} ${LIB}/libtjo.a -o ray3d

install:
	cp ray3d ${BIN}

clean:
	rm ray3d *.o
