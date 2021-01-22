      program pr2vpvs
c
c  Program to convert Poisson's ratio to Vp/Vs
c     ratio
c
c  TJOwens, 4/8/96
c
c  updated by tjo to gfortran, 12/2006
c
      character*20 argv
      real vpvs, pr
      integer iargs, iargc, ounit
      ounit=6
      iargs=iargc()
      if(iargs.eq.0) then
 	write(ounit,*) 'Usage: pr2vpvs Poissons ratio'
 	write(ounit,*) 'Returns: Vp/Vs ratio'
         stop
      endif
      call getarg(1,argv)
      read(argv,*) pr
      vpvs=sqrt((pr - 1.)/(pr - .5))
      write(ounit,101) vpvs
 101  format(f5.3)
      stop
      end
