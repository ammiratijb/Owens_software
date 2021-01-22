      program vpvs2pr
c
c  Program to convert Vp/Vs ratios into
c     Poisson's ratios
c
c  TJOwens, 4/8/96
c
      character*20 argv
      real vpvs, pr
      integer iargs, iargc, ounit
      ounit=6
      iargs=iargc()
      if(iargs.eq.0) then
 	write(ounit,*) 'Usage: vpvs2pr Vp/Vs ratio'
 	write(ounit,*) 'Returns: Poissons ratio'
         stop
      endif
      call getarg(1,argv)
      read(argv,*) vpvs
      pr=(1 - .5*(vpvs**2))/(1 - vpvs**2)
      write(ounit,101) pr
 101  format(f5.3)
      stop
      end
