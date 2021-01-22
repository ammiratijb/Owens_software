      subroutine rayfin(iq,i4,i10,i11,lnumbr,dflag,mflag)
      integer, parameter :: MAXRAYS   = 5000
      integer, parameter :: MAXLAYERS = 100
      dimension strike(MAXLAYERS),dip(MAXLAYERS),z(MAXLAYERS),alpha(MAXLAYERS),beta(MAXLAYERS),
     *          eta(3,MAXLAYERS),q(3,MAXRAYS),q0(3),v(2,MAXLAYERS),qv(MAXRAYS),raydis(3),
     *          qloc1(3),qloc2(3),a(3,3),iface(MAXRAYS),layer(MAXLAYERS),
     *          amag(3,MAXRAYS),hmag(3,MAXRAYS),raymag(3,MAXRAYS),rayhil(3,MAXRAYS),
     *          raytim(MAXRAYS),direct(3),rho(MAXLAYERS)
      logical pors,amps,free,dflag,mflag
      integer trans,refl,type
      common /cord/ a
      common /amcal/ qloc1,qloc2,vb,va,sinib,sinia,vp1,vs1,rho1,
     *               vp2,vs2,rho2,free,type
      common /transm/ q,qv,v,alpha,beta,rho,strike,dip,iface,jhilb,
     *                amag,hmag,layer,amps,trans,refl,nlyrs
      common /raywrt/ eta,z,raymag,rayhil,raytim,ntim,p0,pors,
     *                oldlyr,q0,direct,tdirec,baz
      call zeerow(raydis,1,3)
      call timdis(raydis,q,3,MAXRAYS,qv,iq,time,
     *            iface,eta,3,MAXLAYERS,z,lnumbr,dislyr,deplyr)
      tmpdis=sqrt(raydis(1)**2 + raydis(2)**2)
      init=1
      if(.not.dflag) go to 1
         tdirec=time
         time=0.
         init=0.
         do 3 i3=1,3
    3    direct(i3)=raydis(i3)
         go to 2
    1 time=time + timcor(direct,raydis,q0,v(1,nlyrs))-tdirec
      if(.not.dflag.and..not.mflag) init=0
    2 call anom(q(1,iq),qv(iq),azanom,panom,angle)
      if(.not.amps) go to 26
         free=.true.
         type=0
         do 52 i52=1,3
   52    qloc1(i52)=q(i52,iq)
         vb=qv(iq)
         sinib=angle
         va=0.
         sinia=0.
         vp1=alpha(1)
         vs1=beta(1)
         rho1=rho(1)
         rho2=0.
         vp2=0.
         vs2=0.
         call ampcal(amag(1,iq),hmag(1,iq),
     *               raymag(1,ntim),rayhil(1,ntim),
     *               0.,0.,ihilb)
         if(ihilb.eq.1) jhilb=1
         free=.false.
         raytim(ntim)=time
         ntim=ntim+1
   26 continue
c
c  left over from testing
c     write(67,*) ntim,lnumbr,dislyr,deplyr
c
      call wrtray(lnumbr,azanom,panom,time,baz,p0,pors,
     *            init,i4,i10,i11,oldlyr,angle,tdirec)
      return
      end
