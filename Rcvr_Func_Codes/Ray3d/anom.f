      subroutine anom(q,v,az,p,sini)
c
c   calculates the azimuth and ray parameter of a ray defined by q
c     in a medium of velocity v, assuming the surface is horizontal
c
      dimension q(3)
      deg(rad)=rad*57.2957795
      cosi=-q(3)
      sini=sqrt(1.-cosi*cosi)
      p=sini/v
c
c as always vertical incidence case is special
c
      if(sini.gt..0001) then
         sinb=-q(2)/sini
         cosb=-q(1)/sini
         az=atan2(sinb,cosb)
         az=deg(az)
      else
         az=0.0
      endif
  101 format(1x,5e15.7)
      return
      end
