
#
# Makefile for various seismic utilities
#
# PACKAGES [SOMETIMES] REQUIRED:  TJO_Fortran_Library
#
# 
# This is where the EXECutables reside on YOUR SYSTEM
# If you are using svn, the relative path to the bin directory is
# best.  DO NOT check executables into svn!
#
FORTRAN=gfortran
FFLAGS=-O -ffixed-line-length-132
#
# This is the TJO_Fortran_Library.  
#
EXEC=../bin
TJOLIB=../TJO_Fortran_Library/lib/libtjo.a
BINS=distaz jday pr2vpvs vpvs2pr
#
# Clean up option
#
#
clean:	
	/bin/rm *.o ${BINS}
#
all: pr2vpvs vpvs2pr distaz jday
#
install: ${BINS}
	cp ${BINS} ${EXEC}
#
pr2vpvs.o: pr2vpvs.f
	${FORTRAN} ${FFLAGS} -c pr2vpvs.f
pr2vpvs:  pr2vpvs.o  
	${FORTRAN} ${FFLAGS} pr2vpvs.o -o pr2vpvs
#
vpvs2pr.o: vpvs2pr.f
	  ${FORTRAN} ${FFLAGS} -c vpvs2pr.f
vpvs2pr: vpvs2pr.o 
	 ${FORTRAN} ${FFLAGS} vpvs2pr.o -o vpvs2pr
#
distaz.o: distaz.f ${TJOLIB}
	  ${FORTRAN} ${FFLAGS} -c distaz.f
distaz: distaz.o
	${FORTRAN} ${FFLAGS} distaz.o ${TJOLIB} -o distaz
#
jday.o: jday.f ${TJOLIB}
	  ${FORTRAN} ${FFLAGS} -c jday.f
jday: jday.o
	${FORTRAN} ${FFLAGS} jday.o ${TJOLIB} -o jday
#
#=======================================================
# Codes below this line have not been tested/compiled yet
#  The are in ./Older_Codes at the moment
#=======================================================
#
xysac:  xysac.o 
	${FORTRAN}  xysac.f -lsac -o ${EXEC}/xysac
#
epcsrch:  epcsrch.o  
	${FORTRAN}  epcsrch.o ${USCLIB} -o ${EXEC}epcsrch
#
tjo_mod_ave:  tjo_mod_ave.o 
	${FORTRAN}  tjo_mod_ave.o ${USCLIB} -o ${EXEC}tjo_mod_ave
#
vplot:  vplot.o 
	${FORTRAN}  vplot.o ${USCLIB} -lsac -o ${EXEC}vplot
#
