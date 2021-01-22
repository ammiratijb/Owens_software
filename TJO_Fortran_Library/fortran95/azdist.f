      subroutine azdist(stalat, stalon, evtlat, evtlon,
     *                  delta, az, baz)
c
c Subroutine to calculate the Great Circle Arc distance
c    between two sets of geographic coordinates
c
c Given:  stalat => Latitude of first point (+N, -S) in degrees
c	  stalon => Longitude of first point (+E, -W) in degrees
c	  evtlat => Latitude of second point
c	  evtlon => Longitude of second point
c
c Returns:  delta => Great Circle Arc distance in degrees
c	    az    => Azimuth from pt. 1 to pt. 2 in degrees
c	    baz   => Back Azimuth from pt. 2 to pt. 1 in degrees
c
c If you are calculating station-epicenter pairs, pt. 1 is the station
c
c Equations take from Bullen, pages 154, 155
c
c T. Owens, September 19, 1991
c           Sept. 25 -- fixed az and baz calculations
c           Dec. 2006, changed for fortran95 
c           May, 2007 -- added predel to get around OSX acos round-off NaN issue
c
      double precision scolat, slon, ecolat, elon
      double precision a,b,c,d,e,aa,bb,cc,dd,ee,g,gg,h,hh,k,kk
      double precision rhs1,rhs2,sph,rad,del,daz,dbaz,pi
c
      pi=3.141592654
      piby2=pi/2.
      rad=2.*pi/360.
c
c scolat and ecolat are the geocentric colatitudes
c as defined by Richter (pg. 318)
c
c Earth Flattening of 1/298.257 take from Bott (pg. 3)
c
      sph=1.0/298.257
c
      scolat=piby2 - atan((1.-sph)*(1.-sph)*tan(dble(stalat)*rad))
      ecolat=piby2 - atan((1.-sph)*(1.-sph)*tan(dble(evtlat)*rad))
      slon=dble(stalon)*rad
      elon=dble(evtlon)*rad
c
c  a - e are as defined by Bullen (pg. 154, Sec 10.2)
c     These are defined for the pt. 1
c
      a=sin(scolat)*cos(slon)
      b=sin(scolat)*sin(slon)
      c=cos(scolat)
      d=sin(slon)
      e=-cos(slon)
      g=-c*e
      h=c*d
      k=-sin(scolat)
c
c  aa - ee are the same as a - e, except for pt. 2
c
      aa=sin(ecolat)*cos(elon)
      bb=sin(ecolat)*sin(elon)
      cc=cos(ecolat)
      dd=sin(elon)
      ee=-cos(elon)
      gg=-cc*ee
      hh=cc*dd
      kk=-sin(ecolat)
c
c  Bullen, Sec 10.2, eqn. 4
c
      predel=a*aa + b*bb + c*cc
      if(abs(predel+1.).lt..000001) then
        predel=-1.
      endif
      if(abs(predel-1.).lt..000001) then
        predel=1.
      endif
      del=acos(predel)
      delta=del/rad
c
c  Bullen, Sec 10.2, eqn 7 / eqn 8
c
c    pt. 1 is unprimed, so this is technically the baz
c
c  Calculate baz this way to avoid quadrant problems
c
      rhs1=(aa-d)*(aa-d)+(bb-e)*(bb-e)+cc*cc - 2.
      rhs2=(aa-g)*(aa-g)+(bb-h)*(bb-h)+(cc-k)*(cc-k) - 2.
      dbaz=atan2(rhs1,rhs2)
      if(dbaz.lt.0.0d0) dbaz=dbaz+2*pi
      baz=dbaz/rad
c
c  Bullen, Sec 10.2, eqn 7 / eqn 8
c
c    pt. 2 is unprimed, so this is technically the az
c
      rhs1=(a-dd)*(a-dd)+(b-ee)*(b-ee)+c*c - 2.
      rhs2=(a-gg)*(a-gg)+(b-hh)*(b-hh)+(c-kk)*(c-kk) - 2.
      daz=atan2(rhs1,rhs2)
      if(daz.lt.0.0d0) daz=daz+2*pi
      az=daz/rad
c
c   Make sure 0.0 is always 0.0, not 360.
c
      if(abs(baz-360.).lt..00001) baz=0.0
      if(abs(az-360.).lt..00001) az=0.0
      return
      end
