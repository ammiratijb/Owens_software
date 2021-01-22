      subroutine timdis(dist,q,ii,jj,vel,n,time,
     *                  iface,eta,kk,ll,z,lnumbr,dislyr,deplyr)
c
c   calculates the point a ray, specified by the n ray unit normals
c     given in q, enters the layered medium and its travel-time in
c     the layered system
c
      dimension q(ii,jj),vel(n),iface(n),eta(kk,ll),z(1),dist(3)
      time=0.
c
c   calculates time & dist for the nth to 2nd q-vectors since vector
c     #1 is the incident ray
c
      do 1 i1=1,n-1
         j1=n - i1 + 1
         unum=eta(3,iface(j1))*(z(iface(j1))-dist(3))
     *       -eta(2,iface(j1))*dist(2)
     *       -eta(1,iface(j1))*dist(1)
         u=unum/dot(eta(1,iface(j1)),q(1,j1))
         do 2 i2=1,3
    2       dist(i2)=dist(i2) + u*q(i2,j1)
         time=abs(u)/vel(j1)  + time
         if(iface(j1).eq.lnumbr) then
            dislyr=sqrt(dist(1)**2 + dist(2)**2)
            deplyr=dist(3)
         endif
    1 continue
      if(lnumbr.eq.0) then
          deplyr=dist(3)
          dislyr=sqrt(dist(1)**2 + dist(2)**2)
      endif
      return
      end
