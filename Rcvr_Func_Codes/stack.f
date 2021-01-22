      program stack
c
c
c   program stacks seismograms in sac format using the user-defined
c     time t0 in each file as the alignment time
c     **** all seismograms must have a t0 defined in their headers
c
c
      dimension x(5000),sum(5000),wt(300),dt(300),sumsq(5000),var(5000)
      integer blank,ounit
      character file(300)*32,suffix*32,sout*32,varfl*32,varsuf*5
      logical weigh,yesno,norm
c **********************************************************************
c
c common block info for link with subroutine sacio
c
      real instr
      integer year,jday,hour,min,isec,msec
      character*8 sta,cmpnm,evnm
      common /tjocm/ dmin,dmax,dmean,year,jday,hour,min,isec,msec,sta,
     *            cmpnm,az,cinc,evnm,baz,delta,rayp,depth,decon,agauss,
     *              c,tq,instr,dlen,begin,t0,t1,t2
c
c **************************************************************************
c
c   parameters are:
c
c      dmin,dmax,dmean = min,max, and mean of data read or written
c      year,jday,hour,min,isec,msec = gmt reference time (all integers)
c      sta = station name (a8)
c      cmpnm = component name (a8)
c      az = orientation of component (wrt north)
c      cinc = inclination of component (wrt vertical)
c      evnm = name of event (a8)
c      baz  = back azimuth of from station to event
c      delta = distance from station to event in degrees
c      rayp  = ray parameter of arriving phase in j-b earth
c      depth = depth of event in kilometers
c      decon = if = 1. indicates data has been source equalized
c      agauss = width of gaussian used in source equalization if decon = 1.
c      c     = trough filler used in source equalization if decon = 1.
c      tq = t/q value used in synthetic (if used)
c      instr = 1. if response of 15-100 system has been put into synthetic
c      dlen  = length of data in secs.
c      begin = time of 1st data point wrt gmt reference (in secs)
c      t0,t1,t2 = user defined times wrt gmt reference (in secs)
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
      common /stkcmn/ x,sum,sumsq,var
      common /innout/ inunit,ounit
      data varsuf /'_var '/
      inunit=5
      ounit=6
      dtol = .005
      write(ounit,100)
  100 format('Enter Stacking Files Info')
      call asktxt('Enter file suffix: ',suffix)
      ibsuf=blank(suffix)
      do 1 i1=1,300
           call asktxt('File name: ',file(i1))
           ib=blank(file(i1))
           if(ib.eq.0) go to 2
           file(i1)(1:ib + ibsuf)=file(i1)(1:ib)//suffix
    1 continue
    2 nstack=i1-1
      norm=.false.
      norm=yesno('Normalize each trace to its maximum ampl.? ')
      weigh=yesno('Weight the seismograms ? ')
      do 3 i3=1,nstack
         if(weigh) then
            write(ounit,101) file(i3)
  101       format('Enter weight for ',a32)
            wt(i3)=ask('Weight = ')
         else
            wt(i3)=1.
         endif
    3 continue
      nstmin=1201
      nendmx=1201
      do 4 i4=1,nstack
         call zeerow(x,1,5000)
         call sacio(file(i4),x,npts,dt(i4),+1)
         if(t0.eq.-12345.) go to 14
	 if(norm) then
            xnorm=dmax
            if(abs(dmin).gt.xnorm) xnorm=abs(dmin)
            xnorm=xnorm/wt(i4)
            do 5 i5=1,npts
    5          x(i5)=x(i5)/xnorm
         endif
         if(i4.ne.1) then
            do 7 i7=1,i4-1
               if(abs(dt(i4)-dt(i7)).le.dtol) go to 7
                  write(ounit,102)dt(i4),file(i4),dt(i7),file(i7)
  102             format(' incompatible sample rates: dt = ',e15.6,
     *                  ' in ',a32,/,28x,'dt = ',e15.6,' in ',a32)
                  stop
    7          continue
            else
               call asktxt('Stack output file:  ',sout)
               iblank=blank(sout)
               ibvar=blank(varsuf)
               varfl(1:iblank+ibvar)=sout(1:iblank)//varsuf
               call zeerow(sum,1,5000)
               call zeerow(sumsq,1,5000)
            endif
c
c      find data point closest to t0
c
         nlow=(t0-begin)/dt(i4)
         nhi=nlow + 1
         nloc=nlow
         if(t0-float(nlow)*dt(i4).gt.float(nhi)*dt(i4)-t0) nloc=nhi
c
c   identify this point w/ pt 1201 in stack file for now
c
         if(nloc.lt.1201) then
            nst=1201-nloc + 1
            nxst=1
         else
            nst=1
            nxst=nloc - 1201 + 1
         endif
         nend=4801
         if(nst + npts -1.lt.nend) nend=nst + npts + 1
         if(i4.eq.1) nendmn=nend
         ix=nxst
         if(nst.lt.nstmin) nstmin=nst
         if(nend.lt.nendmn) nendmn=nend
         do 8 i8=nst,nend
            sum(i8)=sum(i8) + x(ix)
            sumsq(i8)=sumsq(i8) + x(ix)*x(ix)
            ix=ix+1
    8    continue
    4 continue
c
c delete leading and trailing zeros from stack array
c
      nxst=1
      nst=nstmin
      nend=nendmn
      call zeerow(x,1,5000)
      call zeerow(var,1,5000)
      do 12 i12=nst,nend
         x(nxst)=sum(i12)/float(nstack)
         var(nxst)=float(nstack)*sumsq(i12) - sum(i12)*sum(i12)
         var(nxst)=var(nxst)/(float(nstack)*float(nstack-1))
         nxst=nxst + 1
   12 continue
      npts=nxst - 1
      t0=0.
      t1=0.
      t2=0.
      begin=0.
      year=1959.
      evnm(1:8)='stacked '
      decon=0.
      call minmax(x,npts,dmin,dmax,dmean)
      call sacio(sout,x,npts,dt(1),-1)
      evnm(1:8)='variance'
      call minmax(var,npts,dmin,dmax,dmean)
      call sacio(varfl,var,npts,dt(1),-1)
      stop
   14 write(ounit,104) file(i4)
  104 format(' no t0 in ',a32)
      stop
      end
