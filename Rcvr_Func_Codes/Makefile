
FORTRAN=gfortran
FFLAGS=-O -ffixed-line-length-132

BIN=../bin
LIB=../TJO_Fortran_Library/lib
INCLUDE=../TJO_Fortran_Library/Include_Modules

iterdecon_batch.o:  iterdecon_batch.f
	${FORTRAN} ${FFLAGS} -I${INCLUDE} -c iterdecon_batch.f
rf_iterdecon.o:  rf_iterdecon.f
	${FORTRAN} ${FFLAGS} -I${INCLUDE} -c rf_iterdecon.f
recipes00.o:  recipes00.f
	${FORTRAN} ${FFLAGS} -c recipes00.f
iterdecon: iterdecon_batch.o recipes00.o
	${FORTRAN} ${FFLAGS} -I${INCLUDE} iterdecon_batch.o recipes00.o ${LIB}/libtjo.a -o iterdecon_batch
rf_iterdecon: rf_iterdecon.o recipes00.o
	${FORTRAN} ${FFLAGS} -I${INCLUDE} rf_iterdecon.o recipes00.o ${LIB}/libtjo.a -o rf_iterdecon
#
#
pwaveqn.o: pwaveqn.f
	${FORTRAN} ${FFLAGS} -I${INCLUDE} -c pwaveqn.f
pwaveqn: pwaveqn.o 
	${FORTRAN} ${FFLAGS} -I${INCLUDE} pwaveqn.o ${LIB}/libtjo.a -o pwaveqn
#
#
install:
	cp iterdecon_batch rf_iterdecon pwaveqn ${BIN}
clean:
	/bin/rm *.o iterdecon_batch rf_iterdecon pwaveqn

#
# These codes have yet to be converted
#

stack:  stack.f ${INCLUDE}/sac_access.mod
	${FORTRAN} ${FFLAGS} -I${INCLUDE} stack.f ${LIB}/libtjo.a -o stack
#
