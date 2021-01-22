      subroutine rayup(iqref,i11,lyref,iq)
c
c **************
c
c     subroutine to reflect a ray off an interface at depth then
c                transmit it back up to the free surface
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
      vp1=alpha(lyref-1)
      vs1=beta(lyref-1)
      rho1=rho(lyref-1)
c
c  do the reflection off the interface first
c
      j12=lyref
      call coord(q(1,iq),strike(j12),dip(j12),qloc1,
     *           'local',.false.)
      vb=qv(iq)
      va=v(i11,j12-1)
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
      if(.not.amps) go to 22
         vp2=alpha(j12)
         vs2=beta(j12)
         rho2=rho(j12)
         call ampcal(amag(1,iq),hmag(1,iq),
     *               amag(1,iq+1),hmag(1,iq+1),
     *               strike(j12),dip(j12),ihilb)
         if(ihilb.eq.1) jhilb=1
   22 qv(iq+1)=va
      call coord(qloc2,strike(j12),dip(j12),q(1,iq+1),
     *           'globe',.true.)
      iq=iq+1
      iface(iq)=j12
c
c now transmit wave back to surface
c
      if(lyref.eq.2) return
c
c   iq could be returned as -999 from s/r trnsmt -- ray would be skipped
c
      call trnsmt(2,lyref,iq,i11,.true.)
      return
      end
