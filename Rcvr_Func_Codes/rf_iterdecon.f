******************************************************************************
c
      program rf_iterdecon
c
******************************************************************************
c
c     Chuck Ammon - Saint Louis University
c     November 1996 / September 1998 
c     VERSION 1.04
c
c     Modified by T.J.Owens, Fall 2000 to allow easier batch processing
c     VERSION 1.1
c     Specifically:
c          o process put in loop and arrays zeroed between loops
c          o Added an output file name
c          o put some output values into SAC headers
c		call setfhv('USER7',d_error,nerr)
c		call setfhv('USER8',float(nshifts),nerr)
c		call setfhv('USER9',fit,nerr)
c          o Some questions deleted
c          o Some output deleted, or put into verbose loops
c          o increased MAXG (just because)
c
c     Modified by T.J. Owens, May 2007
c     VERSION 1.2
c          o converted to FORTRAN-95
c          o divorced from the sac libraries, using sac_access now
c
c     Modified by T.J. Owens, July 2007
c     VERSION 2.0 - first version of rf_iterdecon
c          o made assumptions about input files for receiver function analysis
c          o no longer requires pre-rotated files
c          o no longer reads in data multiple times
c          o runs almost a factor of 2 faster than iterdecon
c
c     Based on the Kikuchi and Kanamori (1981) iterative
c      deconvolution algorithm. The output of this code
c      is the deconvolution of the "denominator" from
c      the "numerator"
c
c     Header values from the "numerator" are copied into the
c       decon.out file. The gwidth is stored in user0.
c
c******************************************************************************
c
c     if you choose verbose output
c       the deconvolutions are called d???
c       the predictions are called p???
c       the residuals are called r???
c
c      where ??? is corresponds to the number of "bumps"
c
******************************************************************************
c
c     Because the correlation is computed in the 
c     Frequency Domain, the maximum lag computed is npts / 2
c
c     That is, negative lags are not used - so make sure you wavelet starts
c       before the signal.
c
c     A low-pass Gaussian filter is applied to the signals before the
c      deconvolution is performed. The predicted will not match the
c      original signal, but the filtered signal, which is stored in the
c      file called "numerator".
c
******************************************************************************
c
      use sac_access
      type(sac_header) :: original_header, numerator_header, denominator_header, answer_header, gaussian_header, new_header
      type(sac_header) :: radial_header, z_header, tang_header, n_header, e_header
      integer MAXPTS, MAXG
      parameter(MAXPTS = 8192, MAXG = 400)
      real f(MAXPTS),g(MAXPTS),p(MAXPTS),r(MAXPTS)
      real, dimension(MAXPTS,3) :: data
      real amps(MAXG),perr(MAXG),differr(MAXG)
      real, dimension(3) :: caz
      integer shifts(MAXG)
      character*256 numerator, denominator, outfile, spikefile, base, outdir
      character*12 resfile, filename
      character(len=3), dimension(3) :: sufuc, suflc, suf12, suf23, suffix(3)
      character(len=5) :: outsuffix
      
      integer stdin,stdout,spikes,ounit,inunit,forward,inverse
      integer blank, iblank, baselength, idum, outlength
      logical lpositive, verbose, radial
      
      sufuc(1)='.Z '
      sufuc(2)='.N '
      sufuc(3)='.E '
      suflc(1)='.z '
      suflc(2)='.n '
      suflc(3)='.e '
      suf12(1)='.Z '
      suf12(2)='.1 '
      suf12(3)='.2 '
      suf23(1)='.Z '
      suf23(2)='.2 '
      suf23(3)='.3 '
      
      stdin = 5
      stdout = 6
      ounit = 9
      inunit = 10
      spikes = 3
      forward =  1
      inverse = -1
      gwidth = 2.5
c
c     read in the names of the input files
c      
   9  read(stdin,'(a)',END=99) base
      baselength=blank(base)
c
      read(stdin,*) idum
      if (idum .eq. 0) then
         suffix(1)=sufuc(1)
         suffix(2)=sufuc(2)
         suffix(3)=sufuc(3)
      end if
      if (idum .eq. 1) then
         suffix(1)=suflc(1)
         suffix(2)=suflc(2)
         suffix(3)=suflc(3)
      end if
      if (idum .eq. 2) then
         suffix(1)=suf12(1)
         suffix(2)=suf12(2)
         suffix(3)=suf12(3)
      end if
      if (idum .eq. 3) then
         suffix(1)=suf23(1)
         suffix(2)=suf23(2)
         suffix(3)=suf23(3)
      end if
c     read(stdin,'(a)') outdir
c     print *, "outdir ", outdir(1:50), " end "
c     outlength=blank(outdir)
c     if(outdir(outlength:outlength).ne.'/') then
c        outdir(1:outlength+1)=outdir(1:outlength)//'/'
c        outlength=outlength+1
c     endif
c
      read(stdin,*) idum
      if (idum .eq. 0) then
         radial = .TRUE.
         outsuffix='.itr '
      else
         radial = .FALSE.
         outsuffix='.itt '
      end if
      outfile(1:baselength+5)=base(1:baselength)//outsuffix
c     outfile(1:outlength+5)=outdir(1:outlength)//outsuffix
      lenout=blank(outfile)
      spikefile=outfile(1:lenout)//'_spk'
c
      read(stdin,*) maxbumps
      if(maxbumps .gt. MAXG)then
	write(stdout,*) 'Maximum Number of bumps is ',MAXG
	maxbumps = MAXG
      end if
c
      read(stdin,*) theshift
      read(stdin,*) tol
      read(stdin,*) gwidth
c
      read(stdin,*)idum
      if(idum .eq. 1) then
	lpositive = .false.
      else
	lpositive = .true.
      end if 
c
      read(stdin,*)idum
      if(idum .eq. 0) then
	verbose = .false.
      else
	verbose = .true.
      end if 
c
******************************************************************************
c     
      call zero(data(1,1),MAXPTS)
      call zero(data(1,2),MAXPTS)
      call zero(data(1,3),MAXPTS)
c
      call sac_read(base(1:baselength)//suffix(1),data(1,1),MAXPTS,z_header,nerr)
      if(nerr .lt. 0) then
	write(stdout,*)'Problem reading: ', base(1:iblank)//suffix(1)
	stop
      end if
      call sac_read(base(1:baselength)//suffix(2),data(1,2),MAXPTS,n_header,nerr)
      if(nerr .lt. 0) then
	write(stdout,*)'Problem reading: ', base(1:iblank)//suffix(2)
	stop
      end if
      call sac_read(base(1:baselength)//suffix(3),data(1,3),MAXPTS,e_header,nerr)
      if(nerr .lt. 0) then
	write(stdout,*)'Problem reading: ', base(1:iblank)//suffix(3)
	stop
      end if
c
      caz(1)=z_header%cmpaz
      caz(2)=n_header%cmpaz
      caz(3)=e_header%cmpaz
      baz=z_header%baz
      npts=n_header%npts
c
c rotate and load arrays
c
      call rotseis(data,MAXPTS,3,z_header%baz,caz,npts)
c
c  get the headers oriented correctly
c
      radial_header=n_header
      radial_header%cmpaz=baz-180.
      if(radial_header%cmpaz.lt.0.) radial_header%cmpaz=radial_header%cmpaz + 360.
c
      tang_header=e_header
      tang_header%cmpaz=radial_header%cmpaz + 90.
      if(tang_header%cmpaz.ge.360.) tang_header%cmpaz=360. - tang_header%cmpaz
c
      call zero(f,MAXPTS)
      call zero(g,MAXPTS)
      if(radial) then
         forall (i=1:npts)
            f(i) = data(i,2)
            g(i) = data(i,1)
         end forall
            numerator_header=radial_header
            denominator_header=z_header
       else
         forall (i=1:npts)
            f(i) = data(i,3)
            g(i) = data(i,1)
         end forall
            numerator_header=tang_header
            denominator_header=z_header
       end if
      original_header=numerator_header
            
      npts=numerator_header%npts
      beg=numerator_header%b
      nptsd=denominator_header%npts
      b=denominator_header%b
      dt=denominator_header%delta

c
******************************************************************************
c
c     Find the next power of two greater than the data
c       dimensions - use the numerator, zero pad
c
      n = 1
119   continue
c
      if(n .ge. npts) go to 120
      n = n * 2
      go to 119
c      
120   continue
      if(n .gt. MAXPTS)then
        write(stdout,*) 'Too many points needed.'
        write(stdout,*) 'n = ', n
        stop
      end if
c
******************************************************************************
c     zero-pad the data
c
      npts_orig = npts
      npts = n
c
******************************************************************************
c     FINISHED READING FILES
c      
c     Now begin the cross-correlation procedure
c
c      Put the filter in the signals
c
      call gfilter(f,gwidth,npts,dt)
      call gfilter(g,gwidth,npts,dt)
      if(verbose) then
         call wsac1('numerator',f,npts,beg,dt,nerr)
         call wsac1('observed',f,npts,beg,dt,nerr)
         call wsac1('denominator',g,npts,beg,dt,nerr)
      end if
c
c     compute the power in the "numerator" for error scaling
c
      power = 0
      do 100 i = 1, npts
        power = power + f(i)*f(i)
100   continue
c
c     correlate the signals
c 
      call fcorrelate(f,g,npts,MAXPTS,dt)
      if(verbose) then
         call wsac1('ccor0',g,npts,beg,dt,nerr)
      end if
c
c     find the peak in the correlation
c
      maxlag = npts/2
      if(verbose) then
      write(stdout,'(/,a27,f10.5)') 'The maximum spike delay is ', 
     &   real(maxlag) * dt
      end if
c
      if(lpositive) then
	call getmax(g,maxlag,amps(1),shifts(1))
      else
	call getabsmax(g,maxlag,amps(1),shifts(1))
      end if
      amps(1) = amps(1) / dt
c
      nshifts = 1
c
c     read in the signals again
c
      call zero(f,MAXPTS)
      call zero(g,MAXPTS)

      if(radial) then
         forall (i=1:npts)
            f(i) = data(i,2)
            g(i) = data(i,1)
         end forall
            numerator_header=radial_header
            denominator_header=z_header
       else
         forall (i=1:npts)
            f(i) = data(i,3)
            g(i) = data(i,1)
         end forall
            numerator_header=tang_header
            denominator_header=z_header
       end if
      ndummy=numerator_header%npts
      beg=numerator_header%b
      delta=numerator_header%delta

      b=denominator_header%b
      dt=denominator_header%delta
c
c     compute the predicted deconvolution result
c
      call zero(p,MAXPTS)
      call build_decon(amps,shifts,nshifts,p,npts,gwidth,dt)
      if(verbose) then
          call phs_shift(p,theshift,npts,dt)      
          call wsac1('d001',p,npts,-theshift,dt,nerr)
          call phs_shift(p,-theshift,npts,dt)      
      end if
c
c     convolve the prediction with the denominator signal
c      
      call convolve(p,g,npts,dt)
c
      if(verbose) then
        call wsac1('p001',p,npts,beg,dt,nerr)
      end if
c
c     filter the signals
c     
      call gfilter(f,gwidth,npts,dt)
      call gfilter(g,gwidth,npts,dt)
c
      if(verbose)then
        write(resfile,'(a1,i3.3)') 'r',0
        call wsac1(resfile,f,npts,beg,dt,nerr)
      end if
c      
c     compute the residual (initial error is 1.0)
c
      call getres(f,p,npts,r,sumsq_ip1)
c
      sumsq_i = 1.0
      sumsq_ip1 = sumsq_ip1 / power
      d_error = 100*(sumsq_i - sumsq_ip1) 
c
c Added by tjo
c
      perr(1)=100*sumsq_ip1
      differr(1)=d_error
c
      if(verbose)then
      write(resfile,'(a1,i3.3)') 'r',1
c       call wsac1(resfile,r,npts,beg,dt,nerr)
c     
      write(stdout,1000)
      write(stdout,1001)
     &  resfile, dt*amps(1),(shifts(1)-1)*dt,100*sumsq_ip1,
     &  d_error
1000  format(/,1x,'File',9x,
     & 'Spike amplitude   Spike delay   Misfit   Improvement')
1001  format(1x,a10,2x,e16.9,2x,f10.3,3x,f7.2,'%',3x,f9.4,'%')
      end if
c
******************************************************************************
c    
      do while(d_error .gt. tol .and. nshifts .lt. (maxbumps))
c
        nshifts = nshifts + 1
	sumsq_i = sumsq_ip1
c
        call zero(g,MAXPTS)
        forall (i=1:MAXPTS)
          g(i) = data(i,1)
        end forall
          denominator_header=z_header
c
        ndummy=denominator_header%npts
        b=denominator_header%b
        dt=denominator_header%delta

        call gfilter(g,gwidth,npts,dt)
        call fcorrelate(r,g,npts,MAXPTS,dt)
	if(lpositive)then
	 call getmax(g,maxlag,amps(nshifts),shifts(nshifts))
        else
         call getabsmax(g,maxlag,amps(nshifts),shifts(nshifts))
        end if
        amps(nshifts) = amps(nshifts) / dt
c
        call zero(p,MAXPTS)
        call build_decon(amps,shifts,nshifts,p,npts,gwidth,dt)
	if(verbose)then
          write(filename,'(a1,i3.3)') 'd',nshifts
          call phs_shift(p,theshift,npts,dt)      
          call wsac1(filename,p,npts,-theshift,dt,nerr)
          call phs_shift(p,-theshift,npts,dt)      
        end if
c        
	call zero(g,MAXPTS)
      if(radial) then
         forall (i=1:npts)
            g(i) = data(i,1)
         end forall
            denominator_header=z_header
       else
         forall (i=1:npts)
            g(i) = data(i,1)
         end forall
            denominator_header=z_header
       end if
        ndummy=denominator_header%npts
        b=denominator_header%b
        dt=denominator_header%delta

        call convolve(p,g,npts,dt)
	if(verbose)then
          write(filename,'(a1,i3.3)') 'p',nshifts
          call wsac1(filename,p,npts,beg,dt,nerr)
        end if
c                
        call zero(f,MAXPTS)
      if(radial) then
         forall (i=1:npts)
            f(i) = data(i,2)
         end forall
            numerator_header=radial_header
       else
         forall (i=1:npts)
            f(i) = data(i,3)
         end forall
            numerator_header=tang_header
       end if
        ndummy=numerator_header%npts
        beg=numerator_header%b
        delta=numerator_header%delta

        call gfilter(f,gwidth,npts,dt)
        call getres(f,p,npts,r,sumsq_ip1)
        
        sumsq_ip1 = sumsq_ip1/ power
	d_error = 100*(sumsq_i - sumsq_ip1)
c
        perr(nshifts)=100*sumsq_ip1
        differr(nshifts)=d_error

	 
	if(verbose)then
        write(resfile,'(a1,i3.3)') 'r',nshifts
          call wsac1(resfile,r,npts,beg,dt,nerr)
	
	write(stdout,1001)
     &   resfile,dt*amps(nshifts),(shifts(nshifts)-1)*dt,
     &   100*sumsq_ip1,d_error
	end if
c    
      enddo
c
******************************************************************************
c      
      if(verbose) then
      write(stdout,1010) d_error
1010  format(/,1x,'Last Error Change = ',f9.4,'%',/)
      end if
c
c     if the last change made no difference, drop it
c      
      fit = 100 - 100*sumsq_ip1
c
      if(d_error .le. tol)then
         nshifts = nshifts - 1
         fit = 100 - 100*sumsq_i
      if(verbose) then
         write(stdout,*)'Hit the min improvement tolerance - halting.'
      end if
      end if
c
      if(nbumps .ge. maxbumps)then
      if(verbose) then
         write(stdout,*)'Hit the max number of bumps - halting.'
      end if
      end if
c
      if(verbose) then
      write(stdout,*)'Number of bumps in final result: ', nshifts
      write(stdout,1011) fit
1011  format(1x,'The final deconvolution reproduces ',
     &    f6.1,'% of the signal.',/)
      end if
c
******************************************************************************
c
c     compute the final prediction
c
      call zero(p,MAXPTS)
      call build_decon(amps,shifts,nshifts,p,npts,gwidth,dt)
      call zero(g,MAXPTS)

      if(radial) then
         forall (i=1:npts)
            g(i) = data(i,1)
         end forall
            denominator_header=z_header
       else
         forall (i=1:npts)
            g(i) = data(i,1)
         end forall
            denominator_header=z_header
       end if
      ndummy=denominator_header%npts
      b=denominator_header%b
      dt=denominator_header%delta

      call convolve(p,g,npts,dt)
      if(verbose) then
         call sac_init_header(new_header)
         call wsac1('predicted',p,npts,beg,dt,nerr)
         new_header%delta=dt
         new_header%npts=npts
         new_header%b=beg
         call sac_write('predicted',p,MAXPTS,new_header,nerr)
      end if
      call zero(g,MAXPTS)
c
c     write out the answer
c
      call zero(p,MAXPTS)
      call build_decon(amps,shifts,nshifts,p,npts,gwidth,dt)
      call phs_shift(p,theshift,npts,dt)      
c
c
c changed to Original NPTS by tjo to avoid writing
c out a bunch of zeros
c
      call sac_init_header(answer_header)
      answer_header=original_header
      answer_header%delta=dt
      answer_header%npts=npts_orig
      answer_header%b=-theshift
      theend = -thshift + (npts_orig-1)*dt
      answer_header%e=theend
      answer_header%nzsec=-12345
      answer_header%user0=gwidth
      answer_header%kuser0='Rftn'
      answer_header%kuser1='IT_DECON'
      answer_header%user7=d_error
      answer_header%user8=float(nshifts)
      answer_header%user9=fit
c     print *, answer_header
      call sac_write(outfile(1:lenout),p,MAXPTS,answer_header,nerr)

      open(unit=spikes, file=spikefile, form='formatted')
      write(stdout,2011) nshifts, dt, fit, d_error, 
     * (outfile(m:m), m=1,lenout)
      write(spikes,2011) nshifts, dt, fit, d_error, 
     * (outfile(m:m), m=1,lenout)
c    write(spikes,2011) nshifts, dt, fit, d_error, outfile(1:lenout)
      do 2002 i=1,nshifts
        write(spikes,2001) 
     &      dt*amps(i),(shifts(i)-1)*dt,perr(i),differr(i)
 2002 continue
      close(unit=spikes)
 2001 format(e16.9,1x,f10.4,1x,f8.4,1x,f9.4)
 2011 format(i3,1x,f6.3,1x,f6.1,1x,f7.4,1x,256a1)
c
c     write out the gaussian filter
c
      if(verbose)then
        call sac_init_header(gaussian_header)
        call zero(p,MAXPTS)
        p(1) = 1 / dt
        call phs_shift(p,theshift,npts,dt)      
        call gfilter(p,gwidth,npts,dt)
        call wsac1('thefilter',p,npts,beg,dt,nerr)
        gaussian_header%npts=npts
        gaussian_header%b=beg
        gaussian_header%delta=dt
        call sac_write('thefilter',p,MAXPTS,gaussian_header,nerr)
      end if
     
      go to 9
   99 stop
      end
*
******************************************************************************
******************************************************************************
*
*
*
*
************************************************************************
*
*     correlation routine - correlates f and g and replaces the 
*       g with the cross-correlation the value is normalized
*       by the zero-lag autocorrelation of g
*
************************************************************************
*
      subroutine fcorrelate(f,g,n,MAXPTS,dt)
      real f(MAXPTS), g(MAXPTS), c(8192)
      real sum0, temp
      integer i,n,n2,n2o2
c
c     compute the zero-lag autocorrelation of g
c
      sum0 = 0
      do 1 i = 1, n
        sum0 = sum0 + g(i)*g(i)
1     continue
      sum0 = sum0 * dt
c
c     compute the next power of 2 greater than n
c
      n2 = 1
5     n2 = n2 * 2
      if(n2 .lt. n) go to 5
c     
6     continue
      n2o2 = n2 / 2
c
c     Use the Numerical Recipes routine to compute the cross correlation
c
      call correl(f,g,n2,c)
c 
      temp = dt / sum0
c
      do 20 i = 1,n2
        g(i) = c(i) * temp
20    continue
      
      return
      end   
*
************************************************************************
*
*     zero a real array
*
************************************************************************
*
      subroutine zero(x,n)
      real x(n)
      integer i,n
      
      do 1 i = 1,n
        x(i) = 0
1     continue
      return
      end
*
************************************************************************
*
*     get max value of array and its index
*
************************************************************************
*
      subroutine getmax(x,n,maxvalue,maxindex)
      real x(n), maxvalue
      integer i,n,maxindex
      
      maxvalue = x(1)
      maxindex = 1
      do 20 i = 2, n
	if(x(i) .gt. maxvalue) then
	   maxvalue = x(i)
	   maxindex = i
        end if
20    continue

      return
      end
*
************************************************************************
*
*     find max absolute value of array and its index
*
************************************************************************
*
      subroutine getabsmax(x,n,thevalue,maxindex)
      real x(n), maxvalue, thevalue
      integer i,n,maxindex
      
      maxvalue = abs(x(1))
      maxindex = 1
      thevalue = x(1)
      do 20 i = 2, n
	if(abs(x(i)) .gt. maxvalue) then
	   maxvalue = abs(x(i))
	   thevalue = x(i)
	   maxindex = i
        end if
20    continue

      return
      end
*
*
************************************************************************
*
*     getres
*
************************************************************************
*
      subroutine getres(x,y,n,r,sumsq)
      real x(n), y(n), r(n), sumsq
      integer i,n
      
      sumsq = 0 
      do 20 i = 1, n
       r(i) = x(i) - y(i)
       sumsq = sumsq + r(i)*r(i) 
20    continue

      return
      end
*
************************************************************************
*
*     compute the predicted time series from a set of
*       amplitudes and shifts
*
************************************************************************
*
      subroutine build_decon(amps,shifts,nshifts,p,n,gwidth,dt)
      real p(n), amps(nshifts)
      integer shifts(nshifts)
      integer i, n, nshifts
      
      call zero(p,n)
      do 1 i = 1, nshifts
        p(shifts(i)) = p(shifts(i)) + amps(i)
1     continue

      call gfilter(p,gwidth,n,dt)

      return
      end
*
************************************************************************
*
*     convolve a function with a unit-area Gaussian filter.
*
************************************************************************
*
      subroutine gfilter(x,gwidth_factor,n,dt)
      real x(n), pi, two_pi, gauss, d_omega, omega
      real gwidth, gwidth_factor, sum
      integer i, j, n, n2, halfpts
      integer forward, inverse
c      
      forward = 1
      inverse = -1
      pi = acos(-1.0)
      two_pi = 2 * pi
      sum = 0
c      
      n2 = 1
1     n2 = n2 * 2
      if(n2 .ge. n) goto 2
      go to 1
c      
2     continue
      halfpts = n2 / 2
c
      call realft(x,halfpts,forward)
c
      df = 1 / (float(n2) * dt)
      d_omega = two_pi * df
      gwidth = 4.0*gwidth_factor*gwidth_factor
c 
c     Handle the nyquist frequency
c
      omega = two_pi/(2.0*dt)
      gauss = exp(-omega*omega / gwidth)
      x(2) = x(2) * gauss
c 
      do 5 i = 2, halfpts
          j = i*2
          omega = (i-1) * d_omega
          gauss = exp(-omega*omega / gwidth)
          x(j-1) = x(j-1) * gauss
          x(j)   = x(j)   * gauss
5     continue 
c
      call realft(x,halfpts,inverse)
c      
      scalefactor = dt * (2 * df)
      do 10 i = 1, n
        x(i) = x(i) * scalefactor
10    continue    
c
      return
      end


*
************************************************************************
*
*     replace x with x convolved with y
*
************************************************************************
*
      subroutine convolve(x,y,n,dt)
      real x(n), y(n), dt, scale0, scale1
      integer i, j, n, n2, halfpts
      integer forward, inverse, stderr
c      
      forward = 1
      inverse = -1
      stderr = 6
c      
      if(mod(n,2) .ne. 0) then
        write(stderr,*) 'Error in convolve - n .ne. power of two.'
        stop
      endif
      n2 = n
      halfpts = n2 / 2
c
      call realft(x,halfpts,forward)
      call realft(y,halfpts,forward)
c
      df = 1 / (float(n2) * dt)
c 
c     Handle the zero & nyquist frequency
c
      x(1) = x(1) * y(1)
      x(2) = x(2) * y(2)
c 
      do 5 i = 2, halfpts
          j = i*2
          a = x(j-1)
          b = x(j)
          c = y(j-1)
          d = y(j)
          x(j-1) = a * c - b * d
          x(j)   = a * d + b * c
5     continue  
c
      call realft(x,halfpts,inverse)
      call realft(y,halfpts,inverse)
c      
      scale0 = dt * (2 * df)
      scale1 = dt * scale0
      do 10 i = 1, n
        y(i) = y(i) * scale0
        x(i) = x(i) * scale1
10    continue    
c
      return
      end
*
************************************************************************
*
*     phase shifts a signal
*      
*
************************************************************************
*
      subroutine phs_shift(x,theshift,n,dt)
      real x(n), pi, two_pi, theshift, d_omega, omega
      integer i, j, n, n2, halfpts
      integer forward, inverse
c      
      forward = 1
      inverse = -1
      pi = acos(-1.0)
      two_pi = 2 * pi
c      
      n2 = 1
1     n2 = n2 * 2
      if(n2 .ge. n) goto 2
      go to 1
c      
2     continue
      halfpts = n2 / 2
c
      call realft(x,halfpts,forward)
c
      df = 1 / (float(n2) * dt)
      d_omega = two_pi * df
c 
c     Handle the nyquist frequency
c
      omega = two_pi/(2.0*dt)
      x(2) = x(2) * cos(omega * tshift)
c 
      do 5 i = 2, halfpts
          j = i*2
          omega = (i-1) * d_omega
          a = x(j-1)
          b = x(j)
          c = cos(omega*theshift)
          d = sin(omega*theshift)
          x(j-1) = a*c-b*d
          x(j)   = a*d+b*c
5     continue 
c
      call realft(x,halfpts,inverse)
c      
      scalefactor = dt * (2 * df)
      do 10 i = 1, n
        x(i) = x(i) * scalefactor
10    continue    
c
      return
      end

