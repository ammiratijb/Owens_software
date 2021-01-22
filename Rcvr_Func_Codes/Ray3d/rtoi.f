      subroutine rtoi(r,cosp,sinp,qb,dirtcn)
c
c   transforms a vector r from the ray coordinate system
c     to the interface coordinate system and vice versa
c
c   if dirtcn = .true.  ray => interface
c      dirtcn = .false. interface => ray
c
c   qb is the z component of the ray in the interface system
c
      dimension r(3)
      logical dirtcn
      q=abs(qb)/qb
      r(3)=r(3)*(-q)
      if(dirtcn) go to 1
      xr=r(1)*cosp + r(2)*sinp
      yr=r(1)*sinp - r(2)*cosp
      r(1)=xr*q
      r(2)=yr
      return
    1 xr=r(1)*q
      xl=+xr*cosp + r(2)*sinp
      yl= xr*sinp - r(2)*cosp
      r(1)=xl
      r(2)=yl
      return
      end
