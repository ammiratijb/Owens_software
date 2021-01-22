      subroutine sacio(file,x,np,dt,inout)
c
c  Subroutine to write out SAC files that does not require access
c  to the SAC library itself.
c
c  tjowens
c  June 3, 2007 - switched to FORTRAN-95, "sac_access" module
c
      use sac_access
      type(sac_header) :: sacio_header
      integer year,jday,hour,min,isec,msec,ounit
      character*8 sta,compnm,evnm
      character file*256
      real x(1)
c
      common /tjocm/ dmin,dmax,dmean,year,jday,hour,min,isec,msec,sta,
     *              compnm,caz,cinc,evnm,bz,del,rayp,dep,decon,agauss,
     *              c,tq,rinstr,dlen,btimey,ty0,ty1,ty2
      common /innout/ inunit,ounit
c **********************************************************************
c
c common block info for link with subroutine sacio
c
c
c **************************************************************************
c
c   parameters are:
c
c      dmin,dmax,dmean = min,max, and mean of data read or written
c      year,jday,hour,min,isec,msec = gmt reference time (all integers)
c      sta = station name (a8)
c      compnm = component name (a8)
c      caz = orientation of component (wrt north)
c      cinc = inclination of component (wrt vertical)
c      evnm = name of event (a8)
c      bz  = back azimuth of from station to event
c      del = distance from station to event in degrees
c      rayp  = ray parameter of arriving phase in j-b earth
c      dep = depth of event in kilometers
c      decon = if = 1. indicates data has been source equalized
c      agauss = width of gaussian used in source equalization if decon = 1.
c      c     = trough filler used in source equalization if decon = 1.
c      tq = t/q value used in synthetic (if used)
c      rinstr = 1. if response of 15-100 system has been put into synthetic
c      dlen  = length of data in secs.
c      btimey = time 0f 1st data point wrt gmt reference time (in secs)
c      ty0,ty1,ty2 = user defined times wrt gmt reference (in secs)
c
c ****************************************************************************
c
c    call to sacio is:
c                      call sacio(file,x,np,dt,inout)
c
c    where file = file to be read or written
c          x    = data array to be used
c          np   = number of points in x
c          dt   = sampling rate for x
c          inout = +1 for reading a sac file
c                = -1 for writing a sac file
c
c ******************************************************************************
      integer blank,iblank
      if(inout.lt.0) goto 1
c
c  read a sac file
c
      infile=1
      iblank=blank(file)
c
      call sac_read(file(1:iblank),x,np,sacio_header,nerr)
c
c  fill tjocm values for compatibility
c
      sta=sacio_header%kstnm
      np=sacio_header%npts
      dt=sacio_header%delta
      dmin=sacio_header%depmin
      dmax=sacio_header%depmax
      dmean=sacio_header%depmen
      year=sacio_header%nzyear
      hour=sacio_header%nzhour
      jday=sacio_header%nzjday
      min=sacio_header%nzmin
      isec=sacio_header%nzsec
      msec=sacio_header%nzmsec
      btimey=sacio_header%b
      dlen=sacio_header%e-b
      compnm=sacio_header%kcmpnm
      caz=sacio_header%cmpaz
      cinc=sacio_header%cmpinc
      evnm=sacio_header%kevnm
      bz=sacio_header%baz
      del=sacio_header%gcarc
      rayp=sacio_header%user0
      dep=sacio_header%user1
      agauss=sacio_header%user2
      c=sacio_header%user3
      if(c.gt.0.) decon=1.0
      tq=sacio_header%user4
      rinstr=sacio_header%user9
      ty0=sacio_header%t0
      ty1=sacio_header%t1
      ty2=sacio_header%t2
      return
c
c write a sac file
c
    1 call sac_init_header(sacio_header)
      sacio_header%npts=np
      sacio_header%kstnm=sta
      sacio_header%delta=dt
      sacio_header%depmin=dmin
      sacio_header%depmax=dmax
      sacio_header%depmen=dmean
      if(year.lt.1960) go to 4
      if(decon.lt..0001) go to 5
      sacio_header%b=0.
      go to 6
    5 sacio_header%nzyear=year
      sacio_header%nzhour=hour
      sacio_header%nzjday=jday
      sacio_header%nzmin=min
      sacio_header%nzsec=isec
      sacio_header%nzmsec=msec
    4 sacio_header%b=btimey
    6 if(abs(ty0).gt..00001) sacio_header%t0=ty0
      if(abs(ty1).gt..00001) sacio_header%t1=ty1
      if(abs(ty2).gt..00001) sacio_header%t2=ty2
      sacio_header%kcmpnm=compnm
      sacio_header%cmpaz=caz
      sacio_header%cmpinc=cinc
      sacio_header%kevnm=evnm
      sacio_header%baz=bz
      if(del.gt..0001) sacio_header%gcarc=del
      sacio_header%user0=rayp
      if(dep.gt..0001) sacio_header%user1=dep
      if(decon.lt..0001) go to 3
      sacio_header%user2=agauss
      sacio_header%user3=c
    3 if(tq.gt..0001) sacio_header%user4=tq
      if(rinstr.gt..0001) sacio_header%user5=rinstr
c
      infile=1
      iblank=blank(file)
c
      call sac_write(file(1:iblank),x,np,sacio_header,nerr)
      return
      end
