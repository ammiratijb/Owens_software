      subroutine snell(qb,vb,qa,va,itype,sinib,sinia)
c
c   calculates the ray unit normal vector, qa resulting from an
c     incident unit normal vector, qb interacting with a velocity
c     interface.  the medium velocity of qb is vb, the medium
c     velocity of qa is va
c
      dimension qb(3),qa(3)
      integer ounit
      common /innout/ inunit,ounit
      torr=float(itype)
      sinib=sqrt(1.-qb(3)*qb(3))
c
c   check for near-vertical incidence.  If sinib < 0.002, then ray is
c     set to true vertical incidence, to avoid instabilities in the
c     calculation of the factor "a" below.  This corresponds to angles
c     of incidence of less than 0.11 degrees, so this manipulation should
c     not cause any significant errors
c***********************************
      if(sinib.le..002) then
         sinib=0.
         qb(3)=abs(qb(3))/qb(3)
         qb(2)=0.
         qb(1)=0.
         sinia=0.
         qa(1)=qb(1)
         qa(2)=qb(2)
         qa(3)=torr*qb(3)
         return
      endif
c************************************
c  process all other rays 
c
      sinia=va*sinib/vb
c
c   check for problems with head waves +/or post critically reflected converted
c             phases
c    if any exist, flag the ray and return
c
      if(sinia.ge.1.00) then
         write(ounit,100) vb,va,qb(3)
         if(torr.lt.0..and.qb(3).lt.0.) write(ounit,101)
         if(torr.lt.0..and.qb(3).ge.0.) write(ounit,102)
         if(torr.gt.0.) write(ounit,103)
         itype = -999
         return
      endif
      if(sinia.lt..0001) then
        a=0.
      else
         a=sinia/sqrt(qb(1)*qb(1) + qb(2)*qb(2))
         qa(1)=a*qb(1)
         qa(2)=a*qb(2)
         qa(3)=torr*(qb(3)/abs(qb(3)))*sqrt(1. - sinia*sinia)
      endif
      return
  100 format(' For vb => va of',f6.3,' => ',f6.3,' and qb = ',f6.4)
  101 format('    ===>  A free surface s-to-p reflection is critical ')
  102 format('    ===>  An internal s-to-p reflection is critical ')
  103 format('    ===>  A head wave has been generated')
      end
