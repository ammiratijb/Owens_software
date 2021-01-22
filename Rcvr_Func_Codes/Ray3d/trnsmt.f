      subroutine trnsmt(loopst,looped,iq,iv,up)
c
c *******************
c
c     calculates the amplitude of a wave transmitted through
c     a stack of layers
c
c *******************
c
      integer, parameter :: MAXRAYS   = 5000
      integer, parameter :: MAXLAYERS = 100
      dimension strike(MAXLAYERS),dip(MAXLAYERS),alpha(MAXLAYERS),beta(MAXLAYERS),
     *          rho(MAXLAYERS),q(3,MAXRAYS),v(2,MAXLAYERS),qv(MAXRAYS),
     *          qloc1(3),qloc2(3),a(3,3),iface(MAXRAYS),layer(MAXLAYERS),
     *          amag(3,MAXRAYS),hmag(3,MAXRAYS)
      logical amps,free,up
      integer trans,refl,type,itype
      common /cord/ a
      common /amcal/ qloc1,qloc2,vb,va,sinib,sinia,vp1,vs1,rho1,
     *               vp2,vs2,rho2,free,type
      common /transm/ q,qv,v,alpha,beta,rho,strike,dip,iface,jhilb,
     *                amag,hmag,layer,amps,trans,refl,nlyrs
      do 7 i7=loopst,looped-1
         j7=looped - i7 + 1
         if(.not.up) j7=i7
         k7=j7-1
         if(.not.up) k7=k7+1
         call coord(q(1,iq),strike(j7),dip(j7),qloc1,'local',
     *              .false.)
         vb=qv(iq)
         va=v(iv,k7)
         itype=trans
         call snell(qloc1,vb,qloc2,va,itype,sinib,sinia)
c
c   if itype returns as -999, then a problem phase exists
c      iq is flagged for return to main program -- ray will be skipped
c
         if(itype.eq.-999) then
            iq=-999
            return
         endif
         if(.not.amps) go to 19
            vp2=alpha(k7)
            vs2=beta(k7)
            rho2=rho(k7)
            type=trans
            call ampcal(amag(1,iq),hmag(1,iq),
     *                  amag(1,iq+1),hmag(1,iq+1)
     *                 ,strike(j7),dip(j7),ihilb)
            if(ihilb.eq.1) jhilb=1
            vp1=vp2
            vs1=vs2
            rho1=rho2
   19    qv(iq+1)=va
         call coord(qloc2,strike(j7),dip(j7),q(1,iq+1),'globe',
     *              .true.)
         iq=iq+1
         iface(iq)=j7
    7 continue
      return
      end
