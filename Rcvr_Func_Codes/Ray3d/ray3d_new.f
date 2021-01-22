      program ray3d
c
c    calculates travel times, azimuthal anomalies, ray parameter
c      anomalies for primary and multiple converted waves
c      in a dipping structure, implementation of the method of
c      Langston (1977; bssa)
c
c    Written by T.J. Owens, march 1982, revised innumerable times
c                                       since then
c    Version A-1; Revised May 1987
c                 Revised Oct 1989 for round-off problems around
c                   Loop 32 - mod by tjo
c
c    Revised June 2007 [25 years and its only V2.0!]. Changed some things to FORTRAN-95 (mostly IO)
c
      character(len=8), parameter :: VERSION = '2007.154'
      integer, parameter :: MAXPTS    = 8200
      integer, parameter :: MAXRAYS   = 5000
      integer, parameter :: MAXLAYERS = 100
      dimension strike(MAXLAYERS),dip(MAXLAYERS),z(MAXLAYERS),alpha(MAXLAYERS),beta(MAXLAYERS),
     *          rho(MAXLAYERS),eta(3,MAXLAYERS),q(3,MAXRAYS),q0(3),v(2,MAXLAYERS),qv(MAXRAYS),
     *          dist(3),qloc1(3),qloc2(3),a(3,3),iface(MAXRAYS),layer(MAXLAYERS),
     *          mulyr(MAXLAYERS),amag(3,MAXRAYS),hmag(3,MAXRAYS),raymag(3,MAXRAYS),
     *          rayhil(3,MAXRAYS),exmuls(MAXLAYERS),raytim(MAXRAYS),spike(MAXPTS,3),
     *          synth(MAXPTS),direct(3),hilbt(MAXPTS,3),synhil(MAXPTS)
      logical yes,yesno,pors,again,amps,ppps(MAXLAYERS),free,instrm,qcorr,
     *        mormul(MAXLAYERS),amps1,pps2,mormu2
      integer trans,refl,type,ior(3),blank,exmuls,ippps(MAXLAYERS),ounit
      integer lll
      character struc*256,synout*256,title*256,comp(3)*4,spn(3)*6,syn(3)*6,
     *          name*256
c **********************************************************************
c
c common block info for link with subroutine sacio
c
      real instr
      integer year,jday,hour,min,isec,msec
      character*8 sta,cmpnm,evnm
      common /tjocm/ dmin,dmax,dmean,year,jday,hour,min,isec,msec,sta,
     *         cmpnm,caz,cinc,evnm,baz,delta,p0,depth,decon,agauss,
     *              c,tq,instr,dlen,begin,t0,t1,t2
c
c **************************************************************************
c
c   parameter definitions may be found sacio comments
c
      common /cord/ a
      common /amcal/ qloc1,qloc2,vb,va,sinib,sinia,vp1,vs1,rho1,
     *               vp2,vs2,rho2,free,type
      common /transm/ q,qv,v,alpha,beta,rho,strike,dip,iface,jhilb,
     *                amag,hmag,layer,amps,trans,refl,nlyrs
      common /raywrt/ eta,z,raymag,rayhil,raytim,ntim,p0r,pors,
     *                oldlyr,q0,direct,tdirec,baz1
      common /ar5/ exmuls,ppps,mormul,ippps
      common /ar4/ synhil
      common /ar3/ hilbt
      common /ar2/ synth
      common /ar1/ spike
      common /innout/ inunit,ounit
c
c  ray3d generates specific output file names for synthetics
c    spike series are named "spike name"_sp.[zrt]
c    if synthetic is convolved with a source function,
c    the synthetic is "synthetic name"_sy.[zrt]
c    where "spike name" and "synthetic name" are requested by
c    the program.
c
      data comp/'vert','rad ','tang'/,ior/3,1,2/,
     *     spn/'_sp.z ','_sp.r ','_sp.t '/,
     *     syn/'_sy.z ','_sy.r ','_sy.t '/
      rad(deg)=deg/57.2957795
      inunit=5
      ounit=6
      call iniocm
      write(ounit,120) VERSION
c
c   all output is to file ray3d.out
c
      open(unit=9,file='ray3d.out',form='formatted')
      write(9,120) VERSION
  120 format(' Ray tracer for Dipping Structures -- T.J. Owens, VERSION: ', a8,/)
      again=.false.
      call asktxt(' Specify structure file: ',struc)
      call rdlyrs(struc,nlyrs,title,alpha,beta,rho,z,dum1,dum2,strike,dip,-1,ier)
c
c     adjust input values from rdlyrs to necessary form
c
      tmpz1=z(1)
      z(1)=0.
      tmps1=strike(1)
      strike(1)=0.
      tmpd1=dip(1)
      dip(1)=0.
      do 48 i48=2,nlyrs
      tmps2=strike(i48)
      tmpd2=dip(i48)
      strike(i48)=tmps1
      dip(i48)=tmpd1
      tmps1=tmps2
      tmpd1=tmpd2
      tmpz2=z(i48)
      z(i48)=z(i48-1)+tmpz1
      tmpz1=tmpz2
   48 continue
      write(9,778) struc,title,nlyrs
      do 49 i=1,nlyrs
         write(9,779) i,alpha(i),beta(i),rho(i),strike(i),dip(i),z(i)
   49 continue
      write(9,780)
  778 format(' structure file: ',a10,' model ',a10,1x,i2,' layers',/,
     *       ' layer     vp    vs     dens     strike     dip     z')
  779 format(3x,i2,4x,f4.2,3x,f4.2,5x,f4.2,5x,f6.2,4x,f4.1,4x,f5.1)
  780 format(1x,/)
c
c     ask all initial questions
c
    6 p0=ask('Specify ray param. for incident wave: ')
      p0r=p0
      baz=ask('Back azimuth of incident ray: ')
      baz1=baz
      pors=yesno('P-wave (y or n) ? ')
      if(pors) go to 16
         amps=.false.
         go to 15
   16 amps=yesno('Calculate any amplitudes (y or n) ? ')
      if(.not.amps) go to 15
         pps2=yesno('Pp and Ps only (y or n) ? ')
         pamp=ask('Incident p amplitude = ')
   15 sini=p0*alpha(nlyrs)
      if(.not.pors) sini=p0*beta(nlyrs)
      numint = nlyrs -1
      do 22 i22=1,numint
         layer(i22)=i22+1
         mulyr(i22)=0
         ppps(layer(i22))=.false.
         if(pps2) ppps(layer(i22))=.true.
         mormul(layer(i22))=.false.
   22 continue
   64 write(ounit,107)
  107 format(' your layer ray tracing parameters are: ',//,
     *       'interface  ppps  mormul ')
      do 21 i21=1,numint
        write(ounit,105) layer(i21),ppps(layer(i21)),mormul(layer(i21))
   21 continue
  105 format(5x,i3,5x,l1,5x,l1)
      if(yesno('OK ? (y or n) ')) go to 18
      write(ounit,102)
  102 format(' enter the # of interfaces to trace from (i2)')
      read(inunit,103) numin2
      if(numin2.le.0) go to 60
      numint=numin2
      write(ounit,101)
  101 format(' enter the interface numbers (40i2)')
      read(inunit,103) (layer(i),i=1,numint)
  103 format(40i2)
   60 if(.not.yesno('Change PpPs options (y or n) ? ')) go to 70
         write(ounit,108)
  108 format('enter interface #s which need ppps changed from current',
     *       ' value (40i2) ')
         read(inunit,103) (ippps(i),i=1,40)
         do 71 i71=1,numint
            if(ippps(i71).eq.0) go to 70
            if(ppps(ippps(i71))) then
              ppps(ippps(i71))=.false.
            else
              ppps(ippps(i71))=.true.
            endif
   71 continue
   70 mormu2=yesno('Calculate extra multiples ? (y or n) ')
      if(.not.mormu2) go to 69
      write(ounit,104)
  104 format(' enter interface numbers for extra multiple',
     *       '  calculations (40i2)')
      read(inunit,103) (mulyr(i),i=1,30)
      do 20 i20=1,MAXLAYERS
         if(mulyr(i20).ne.0) go to 20
         nmults=i20-1
         go to 61
   20 continue
   61 if(nmults.eq.0) go to 60
      if(yesno('Calculate extra mults for all rays ? ')) go to 62
         write(ounit,106)
  106 format(' enter only interfaces which have rays that need',
     *       ' extra mults tacked on')
      read(inunit,103) (exmuls(i),i=1,40)
      do 72 i72=1,nlyrs
   72 mormul(i72)=.false.
      do 63 i63=1,40
         if(exmuls(i63).eq.0) go to 64
         mormul(exmuls(i63))=.true.
   63 continue
   69 go to 64
   62 do 65 i65=1,numint
   65    mormul(layer(i65))=.true.
      go to 64
   18 nrays=1
      do 181 i181=1,numint
         nr2=9
         if(ppps(layer(i181))) nr2 = 1
         if(mormul(layer(i181))) nr2 = nr2 + nr2*4*nmults
         nrays=nrays + nr2
  181 continue
      if(nrays.le.MAXRAYS) go to 182
        write(ounit,183) nrays
  183   format(' nrays = ',i5,' is too big - try again ')
        go to 64
  182 if(again) go to 14
c
c   calculate layer interface unit normal vectors in global coordinates
c
      do 1 i1=1,nlyrs
         strike(i1)=rad(strike(i1))
         dip(i1)=rad(dip(i1))
         call norvec(strike(i1),dip(i1),eta(1,i1))
    1 continue
c
c   define incident ray unit vector in global coordinates
c
   14 q0(1)=-sini*cos(rad(baz))
      q0(2)=-sini*sin(rad(baz))
      q0(3)=-sqrt(1. - sini*sini)
c
c   set up velocity arrays and other initital conditions
c
      do 2 i2=1,nlyrs
         if(.not.pors) go to 3
            v(1,i2)=alpha(i2)
            v(2,i2)=beta(i2)
            go to 2
    3    v(1,i2)=beta(i2)
         v(2,i2)=alpha(i2)
    2 continue
      trans=1
      refl=-1
      qv(1)=v(1,nlyrs)
      iface(1)=0
      do 17 i17=1,3
      call zeerow(amag(i17,1),1,MAXRAYS)
      call zeerow(amag(i17,1),1,MAXRAYS)
      q(i17,1)=q0(i17)
      if(.not.amps) go to 17
      amag(i17,1)=pamp*q0(i17)
      hmag(i17,1)=0.
      if(i17.lt.3) go to 17
         vp1=alpha(nlyrs)
         vs1=beta(nlyrs)
         rho1=rho(nlyrs)
         free=.false.
         ntim=1
   17 continue
c
c   s t a r t   r a y   t r a c i n g   s e c t i o n
c
c   find ray unit vectors for the direct ray
c
      ihilb=0
      jhilb=0
      iq=1
      call trnsmt(1,nlyrs,iq,1,.true.)
c
c   if iq= -999 then a head wave has been generated and the run will bomb
c
      if(iq.eq.-999) then
         write(ounit,133)
  133    format(' Immediate problems with head waves ',
     *          'on direct wave pass - Check velocity model !!')
         stop
      endif
      nlr=nlyrs
      call rayfin(nlr,1,0,0,0,.true.,.false.)
      amps1=amps
c
c   calculate the other rays, first all the unconverted rays & their
c     multiples, then the converted waves & their multiples
c     loops 50,52, & 53 do extra multiples, if necessary
c
      do 4 i4=1,2
         do 8 i8=1,numint
            amps=amps1
c
c           if doing the converted waves
c               recalculate the necessary q-vectors
c
            if(i4.eq.1) go to 13
               iq=nlyrs - layer(i8) + 1
               if(.not.amps) go to 28
               vp1=alpha(nlyrs-iq+1)
               vs1=beta(nlyrs-iq+1)
               rho1=rho(nlyrs-iq+1)
   28       loopst=iq
            call trnsmt(loopst,nlyrs,iq,i4,.true.)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
            if(iq.eq.-999) go to 8
c
c           print results for direct converted waves
c
            call rayfin(nlr,i4,0,0,layer(i8),.false.,.false.)
                  if(.not.mormul(layer(i8))) go to 13
                  iqmul=iq
                  do 66 i66=1,nmults
                     if(mulyr(i66).eq.layer(i8).and.
     *                 (.not.ppps(layer(i8)))) go to 66
                     iqi=iqmul
                     do 67 i67=1,2
                        vs1=beta(1)
                        vp1=alpha(1)
                        rho1=rho(1)
                        rho2=0.0
                        vp2=0.
                        vs2=0.
                        call raydwn(iqi,i67,mulyr(i66),iq)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
                        if(iq.eq.-999) go to 66
                        miqdwn=iq
                        do 68 i68=1,2
                           call rayup(miqdwn,i68,mulyr(i66),iq)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
                           if(iq.eq.-999) go to 68
                           call rayfin(iq,0,i67,i68,mulyr(i66),
     *                                 .false.,.true.)
   68                   continue
   67                continue
   66             continue
   13       if(ppps(layer(i8))) amps=.false.
            do 10 i10=1,2
               vs1=beta(1)
               vp1=alpha(1)
               rho1=rho(1)
               rho2=0.0
               vp2=0.
               vs2=0.
               call raydwn(nlyrs,i10,layer(i8),iq)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
               if(iq.eq.-999) go to 10
               iqdown=iq
               do 11 i11=1,2
                  call rayup(iqdown,i11,layer(i8),iq)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
                  if(iq.eq.-999) go to 11
                  call rayfin(iq,i4,i10,i11,layer(i8),.false.,.true.)
                  if(.not.mormul(layer(i8))) go to 11
                  iqmul=iq
                  do 50 i50=1,nmults
                     iqi=iqmul
                     do 52 i52=1,2
                        vs1=beta(1)
                        vp1=alpha(1)
                        rho1=rho(1)
                        rho2=0.0
                        vp2=0.
                        vs2=0.
                        call raydwn(iqi,i52,mulyr(i50),iq)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
                        if(iq.eq.-999) go to 52
                        miqdwn=iq
                        do 53 i53=1,2
                           call rayup(miqdwn,i53,mulyr(i50),iq)
c
c   if iq = -999, then problem phases exist -- this and all subsequent rays
c                      are skipped
c
                           if(iq.eq.-999) go to 53
                           call rayfin(iq,0,i52,i53,mulyr(i50),
     *                                 .false.,.true.)
   53                   continue
   52                continue
   50             continue
   11          continue
   10       continue
    8    continue
    4 continue
      amps=amps1
      if(.not.amps) go to 29
      ntim=ntim-1
c
c  ray3d.amps can be a big file if many rays are traced
c     use with caution
c
      yes=yesno('Create ray3d.amps ? ')
      if(.not.yes) go to 180
      open(unit=8,file='ray3d.amps',form='formatted')
      write(8,788) struc,title,nlyrs,p0r,baz
  788 format(' file: ',a10,' model ',a10,1x,i2,' layers ',
     *       ' ray parameter ',f7.5,' back az. ',f6.2)
  180 do 27 i27=1,ntim
         call rtoi(raymag(1,i27),cos(rad(baz)),sin(rad(baz)),-1.,
     *             .false.)
         call rtoi(rayhil(1,i27),cos(rad(baz)),sin(rad(baz)),-1.,
     *             .false.)
         rayhil(3,i27)=-rayhil(3,i27)
         raymag(3,i27)=-raymag(3,i27)
         if(yes) write(8,122) i27,(raymag(j,i27),j=1,3),
     *                 (rayhil(j,i27),j=1,3),raytim(i27)
   27 continue
  122 format(1x,i3,1x,7e15.7)
      if(jhilb.eq.1) write(ounit,781)
  781 format(' phase shifted arrivals exist ')
      yes=yesno('Save this spike ? ')
      if(.not.yes) go to 29
      dt=ask('Sampling rate (sec): ')
      dura=ask('Signal duration (secs): ')
      delay=ask('First arrival delay: ')
      npts=ifix(dura/dt + .5) + 1.
      begin = 0.
      do 30 i30=1,3
         call zeerow(spike(1,i30),1,MAXPTS)
         if(jhilb.eq.1) call zeerow(hilbt(1,i30),1,npts)
   30 continue
c
c Section below modified on 10/12/89 on Sun-4 to avoid roundoff
c problems identified by John Cassidy at UBC and known to occur
c on Sun-3 versions of ray3d
c Modifications taken from ray3d subroutine in timinv.f
c
      dtby2 = dt/2.
      do 32 j32=1,ntim
         itinc=0
         rtpdel=raytim(j32) + delay
         isampi=rtpdel/dt
         raytm=dt*float(isampi) +dtby2
         if(raytm.lt.rtpdel) itinc=1
         irayl=isampi + itinc + 1
         do 33 i33=1,3
            spike(irayl,i33)=spike(irayl,i33) + raymag(i33,j32)
            if(jhilb.eq.0) go to 33
               hilbt(irayl,i33)=hilbt(irayl,i33) + rayhil(i33,j32)
   33       continue
c
c  END of 10/12/89 modifications
c
   32    continue
      sta=struc(1:8)
      year=1983
      jday=1
      hour=0
      min=0
      isec=0
      msec=0
      call asktxt('Spike output file: ',synout)
      iblank=blank(synout)
      if(iblank.lt.2) go to 35
      call asktxt('Spike name: ',name)
      evnm=name(1:8)
      do 34 i34=1,3
         cmpnm=comp(i34)
         goto (40,41,42) i34
   40    cinc=0.
         caz=0.
         go to 43
   41    caz=baz+180.
         cinc=90.
         go to 43
   42    caz=baz+270.
         cinc=90.
   43    if(caz.gt.360.) caz=caz-360.
         synout(1:iblank+6)=synout(1:iblank)//spn(i34)
         call minmax(spike(1,ior(i34)),npts,dmin,dmax,dmean)
         call sacio(synout,spike(1,ior(i34)),npts,dt,-1)
   34 continue
   35 yes=yesno('Convolve w/ source function ? ')
      if(.not.yes) go to 29
      instrm=yesno('Include 15-100 instrm response ? ')
      qcorr=yesno('Include futterman q ? ')
      if(qcorr) tq=ask('t/q = ')
      nft=npowr2(npts)
      call asktxt('Synthetic output file: ',synout)
      call asktxt('Synth name: ',name)
      kst=0
      evnm=name(1:8)
      sta=struc(1:8)
      if(instrm) instr=1.
      iblank=blank(synout)
      ist=-1
      do 36 i36=1,3
         call zeerow(synth,1,MAXPTS)
         if(jhilb.eq.1) call zeerow(synhil,1,MAXPTS)
         do 37 i37=1,npts
            if(jhilb.eq.1) synhil(i37)=hilbt(i37,ior(i36))
   37       synth(i37)=spike(i37,ior(i36))
         call mkseis(synth,synhil,instrm,qcorr,tq,nft,dt,kst,jhilb)
         cmpnm=comp(i36)
         goto (44,45,46) i36
   44    cinc=0.
         caz=0.
         go to 47
   45    caz=baz+180.
         cinc=90.
         go to 47
   46    caz=baz+270.
         cinc=90.
   47    if(caz.gt.360.) caz=caz-360.
         synout(1:iblank+6)=synout(1:iblank)//syn(i36)
         call minmax(synth,npts,dmin,dmax,dmean)
         call sacio(synout,synth,npts,dt,-1)
   36 continue
      go to 35
   29 again=yesno('Trace another in the same model ? (y or n) ')
      if(again) go to 6
      close(unit=9)
      if(amps) close(unit=8)
      stop
      end
