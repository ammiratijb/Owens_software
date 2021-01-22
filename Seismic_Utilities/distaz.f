      program distaz
c
c  Program to return distance, azimuth, and back azimuth
c     between to geographic coordinates
c
c  For seismologic cases, the first point is the station
c
c  T.J.Owens, September 19, 1991
c
c  converted to fortran-95 by tjo, 12/10/06
c  eliminated need for decimal command line entry, tjo, 12/10/06
c
      character*20 argv
      integer iargs, iargc, ounit
      ounit=6
      iargs=iargc()
      if(iargs.eq.0) then
        write(ounit,*) 'Usage: distaz sta_lat sta_lon evt_lat evt_lon'
	write(ounit,*) '       Returns:  Delta Baz Az'
        stop
      endif
      call getarg(1,argv)
      read(argv,*) stnp
      call getarg(2,argv)
      read(argv,*) stnl
      call getarg(3,argv)
      read(argv,*) epilc
      call getarg(4,argv)
      read(argv,*) epipc
  100 format(f10.5)
      call azdist(stnp, stnl, epilc, epipc, delta, az, baz)
      write(ounit,101) delta, baz, az
  101 format(3(f7.3,1x))
      stop
      end
