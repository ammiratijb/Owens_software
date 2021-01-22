      subroutine raydwn(iqref,i10,lyref,iq)
c
c **************
c
c     subroutine to reflect a ray from the free surface then
c                propagate it down to a designated interface
c
c **************
c
      integer, parameter :: MAXRAYS   = 5000
      integer, parameter :: MAXLAYERS = 100
      dimension strike(MAXLAYERS),dip(MAXLAYERS),alpha(MAXLAYERS),beta(MAXLAYERS),
     *          rho(MAXLAYERS),q(3,MAXRAYS),v(2,MAXLAYERS),qv(MAXRAYS),
     *          qloc1(3),qloc2(3),a(3,3),iface(MAXRAYS),layer(MAXLAYERS),
     *          amag(3,MAXRAYS),hmag(3,MAXRAYS)
      logical amps,free
      integer trans,refl,type,itype
      common /cord/ a
      common /amcal/ qloc1,qloc2,vb,va,sinib,sinia,vp1,vs1,rho1,
     *               vp2,vs2,rho2,free,type
      common /transm/ q,qv,v,alpha,beta,rho,strike,dip,iface,jhilb,
     *                amag,hmag,layer,amps,trans,refl,nlyrs
      iq=iqref
c
c  take ray down to the reflecting interface --
c
c   do reflection from free surface first
c
      call coord(q(1,iq),strike(1),dip(1),qloc1,'local',
     *           .false.)
      vb=qv(iq)
      va=v(i10,1)
      type=refl
      itype=type
      call snell(qloc1,vb,qloc2,va,itype,sinib,sinia)
c
c   if itype returns as -999, then a problem phase exists
c      iq is flagged for return to main program -- ray will be skipped
c
      if(itype.eq.-999) then
         iq=-999
         return
      endif
      if(.not.amps) go to 20
         free=.true.
         call ampcal(amag(1,iq),hmag(1,iq),
     *               amag(1,iq+1),hmag(1,iq+1),
     *               strike(1),dip(1),ihilb)
         if(ihilb.eq.1) jhilb=1
         free=.false.
   20    qv(iq+1)=va
         call coord(qloc2,strike(1),dip(1),q(1,iq+1),'globe'
     *              ,.true.)
         iq=iq+1
         iface(iq)=1
c
c   now transmit wave down to reflecting interface
c
      if(lyref.eq.2) return
c
c  iq could be returned as -999 from s/r trnsmt -- ray would be skipped
c
      call trnsmt(2,lyref,iq,i10,.false.)
      return
      end
