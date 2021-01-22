      subroutine ampcal(amagb,hmagb,amaga,hmaga,strike,dip,ihilb)
c
c subroutine to calculate amplitudes for rays from ray3d
c
c   i n p u t
c
c
      dimension qb(3),qa(3),amagb(3),amaga(3),r3(3),rt(3),at(3),ai(3),
     *          a(3,3),hmaga(3),hmagb(3),rth(3),ht(3)
      logical free
      integer type,ounit
      common /cord/ a
      common /amcal/ qb,qa,vb,va,sinib,sinia,vp1,vs1,rho1,
     *               vp2,vs2,rho2,free,type
      common /innout/ inunit,ounit
      call zeerow(r3,1,3)
      call zeerow(rt,1,3)
      call zeerow(at,1,3)
      call zeerow(ai,1,3)
      call zeerow(ht,1,3)
      call zeerow(rth,1,3)
      rshph=0.
      rph=0.
      rphx=0.
      rphy=0.
      rphz=0.
      rmag=0.
      ncode=0
      eps=.0001
      ihilb=0
      rshmag=0.
      pi=3.14159
c
c vertical incidence case requires special treatment
c
      if(sinib.gt.eps) then
         cosphi=-qb(1)/sinib
         sinphi=-qb(2)/sinib
      else
         cosphi=-1.
         sinphi=0.
      endif
      nd=0
      if(abs(qb(3))/qb(3).gt.0) nd=1
      p=sinib/vb
      if(free) go to 10
      ro2=rho2
c
c   find ncode for non-free surface case
c
      if(abs(vb-vp1).gt.eps) go to 1
        call rcomp(ai,1,nd,sinib,.true.)
        if(type.lt.0) go to 2
        if(abs(va-vp2).lt.eps) ncode=3
        if(abs(va-vs2).lt.eps) ncode=4
        go to 3
    2   if(abs(va-vp1).lt.eps) ncode=1
        if(abs(va-vs1).lt.eps) ncode=2
        go to 3
    1 if(abs(vb-vs1).gt.eps) go to 4
        call rcomp(ai,2,nd,sinib,.true.)
        if(type.lt.0) go to 5
        if(abs(va-vs2).lt.eps) ncode=8
        if(abs(va-vp2).lt.eps) ncode=7
        go to 3
    5   if(abs(va-vp1).lt.eps) ncode=5
        if(abs(va-vs1).lt.eps) ncode=6
    3 ncase=0
      if(ncode.eq.0) go to 4
      if(ncode.le.4) go to 7
         ncase=4
         go to 7
c
c  find ncode for free surface case
c
   10 ro2=0.0
      vp2=0.
      vs2=0.
      if(type.eq.0) go to 15
      if(abs(vb-vp1).gt.eps) go to 12
         call rcomp(ai,1,nd,sinib,.true.)
         if(abs(va-vs1).lt.eps) ncode=2
         if(abs(va-vp1).lt.eps) ncode=1
         go to 13
   12    if(abs(vb-vs1).gt.eps) go to 4
         call rcomp(ai,2,nd,sinib,.true.)
         if(abs(va-vs1).lt.eps) ncode=4
         if(abs(va-vp1).lt.eps) ncode=3
   13 ncase=0
      if(ncode.eq.0) go to 4
      if(ncode.le.2) go to 7
         ncase=2
         go to 7
c
c
c  f i n d  f r e e  s u r f a c e  e f f e c t
c
c
   15 if(abs(vb-vp1).lt.eps) go to 16
      if(abs(vb-vs1).lt.eps) go to 17
      go to 4
   16 call rcomp(ai,1,nd,sinib,.true.)
      call coef8(p,vp1,vs1,rho1,vp2,vs2,0.0,5,nd,rx,rphx)
      call coef8(p,vp1,vs1,rho1,vp2,vs2,0.0,6,nd,rz,rphz)
      ry=0.
      rphy=0.
      go to 18
   17 call rcomp(ai,2,nd,sinib,.true.)
      call coef8(p,vp1,vs1,rho1,vp2,vs2,0.0,7,nd,rx,rphx)
      call coef8(p,vp1,vs1,rho1,vp2,vs2,0.0,8,nd,rz,rphz)
      call coefsh(p,vs1,rho1,vs2,0.0,2,ry,rphy)
   18 if(abs(rphx+pi).gt.eps) go to 22
        rphx=0.
        rx=-rx
   22 if(abs(rphy+pi).gt.eps) go to 23
        rphy=0.
        ry=-ry
   23 if(abs(rphz+pi).gt.eps) go to 24
        rphz=0.
        rz=-rz
   24 do 19 i19=1,3
         rth(i19)=hmagb(i19)
   19    rt(i19)=amagb(i19)
c
c  rt is in global coordinates, but this is equivalent to interface
c    coordinates for the free surface. so transform rt directly to
c    the ray coordinate system
c
      call rtoi(rt,cosphi,sinphi,qb(3),.false.)
      call rtoi(rth,cosphi,sinphi,qb(3),.false.)
      phck=0.
      phck=abs(rphz)+abs(rphx)+abs(rphy)
      if(phck.gt.eps) ihilb=1
      dotar=dot(ai,rt)
c
c vertical incidence can sometimes blow up this step
c    check first
c
      if(abs(dotar).lt.eps) go to 56
      dotar=abs(dotar)/dotar
   56 doth=dot(ai,rth)
      if(abs(doth).lt.eps) go to 26
      doth=abs(doth)/doth
   26 amh=sqrt(rth(1)*rth(1) + rth(3)*rth(3))*doth
      amb=sqrt(rt(1)*rt(1) + rt(3)*rt(3))*dotar
      amaga(1)=rx*(amb*cos(rphx) - amh*sin(rphx))
      amaga(2)=ry*(rt(2)*cos(rphy) - rth(2)*sin(rphy))
      amaga(3)=rz*(amb*cos(rphz) - amh*sin(rphz))
      hmaga(1)=rx*(amh*cos(rphx) + amb*sin(rphx))
      hmaga(2)=ry*(rth(2)*cos(rphy) + rt(2)*sin(rphy))
      hmaga(3)=rz*(amh*cos(rphz) + amb*sin(rphz))
      call rtoi(amaga,cosphi,sinphi,qb(3),.true.)
      call rtoi(hmaga,cosphi,sinphi,qb(3),.true.)
      return
c
c
c  g e n e r a l  c o e f i c i e n t  c a l c u l a t i o n
c
c  first find rt, the incident displacement vector in ray coordinates
c        &    rth, the distorted displacement vector in ray coordinates
c
    7 call coord(amagb,strike,dip,rt,'local',.true.)
      call coord(hmagb,strike,dip,rth,'local',.true.)
      call rtoi(rt,cosphi,sinphi,qb(3),.false.)
      call rtoi(rth,cosphi,sinphi,qb(3),.false.)
      call coef8(p,vp1,vs1,rho1,vp2,vs2,ro2,ncode,nd,rmag,rph)
      call rcomp(r3,ncode-ncase,nd,sinia,.false.)
      if(abs(rph + pi).gt.eps) go to 20
         rph=0.
         rmag=-rmag
   20 at(2)=0.0
c
c  if incident & resulting waves are both s-waves, find sh coeficient
c
      if(ncode.le.4) go to 9
      if(ncode.eq.6) then
          ncodsh=1
      elseif(ncode.eq.8) then
          ncodsh=2
      else
          go to 9
      endif
      call coefsh(p,vs1,rho1,vs2,ro2,ncodsh,rshmag,rshph)
      at(2)=rshmag*(rt(2)*cos(rshph)-rth(2)*sin(rshph))
      ht(2)=rshmag*(rth(2)*cos(rshph)-rt(2)*sin(rshph))
      if(abs(rshph+pi).lt.eps) go to 9
      if(rshph.gt.eps) ihilb=1
    9 dotar=dot(ai,rt)
c
c vertical incidence can sometimes blow up this step
c    check first
c
      if(abs(dotar).lt.eps) go to 55
      dotar=abs(dotar)/dotar
   55 amb=sqrt(rt(1)*rt(1) + rt(3)*rt(3))*dotar
      doth=dot(ai,rth)
      if(abs(doth).lt.eps) go to 25
      doth=abs(doth)/doth
   25 amh=sqrt(rth(1)*rth(1) + rth(3)*rth(3))*doth
      atmag=rmag*(amb*cos(rph)-amh*sin(rph))
      htmag=rmag*(amh*cos(rph)+amb*sin(rph))
      if(rph.gt.eps) ihilb=1
      at(1)=atmag*r3(1)
      at(3)=atmag*r3(3)
      ht(1)=htmag*r3(1)
      ht(3)=htmag*r3(3)
      call rtoi(at,cosphi,sinphi,qb(3),.true.)
      call rtoi(ht,cosphi,sinphi,qb(3),.true.)
      call coord(at,strike,dip,amaga,'globe',.true.)
      call coord(ht,strike,dip,hmaga,'globe',.true.)
      return
    4 write(ounit,102) va,vb,vp1,vs1,vp2,vs2
  102 format(' ncode = 0 for ',6f6.2)
      return
      end
