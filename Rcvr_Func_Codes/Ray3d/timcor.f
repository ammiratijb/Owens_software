      function timcor(x1,x2,q0,v)
c
c  finds the time diference between a ray which enters the
c   layering at point x2 to one which enters the layering at
c   x1 if the half space unit ray vector is q0 and the half
c   space velocity is v
c
      dimension x1(3),x2(3),q0(3),r(3)
      do 1 i=1,3
   1  r(i)=x2(i)-x1(i)
      corr=dot(r,q0)
      timcor=corr/v
      return
      end
