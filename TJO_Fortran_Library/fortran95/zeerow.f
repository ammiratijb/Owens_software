      subroutine zeerow(x,start,end)
c
c formerly called zero, name changed 10/89 to avoid
c conflicts with SAC routine names
c
      dimension x(1)
      integer start,end
      do 1 i=start,end
    1 x(i)=0.
      return
      end
