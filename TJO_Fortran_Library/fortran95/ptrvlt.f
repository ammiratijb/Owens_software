      real function ptrvlt(gcarc,depth,itbl)
      dimension time1(21), time2(21)
      character*10 skip
      integer row1, col1, col2, ounit
      common /innout/ inunit,ounit
      open(unit=itbl,file='/seis/SeisNet/tables/herrin.p')
c
c   Tables only apply to certain ranges
c
      if(gcarc.ge.100..or.depth.gt.800.) then
	  write(ounit,*) 'Out of Range of ptravlt',gcarc,depth
	  ptravlt=0.0
	  return
      endif
c
c  Figure out table row
c
      row1=gcarc/0.5 + 2
      gc1=(row1 -2)/2
      gc2=gc1+0.5
c
c  Figure out table column
c
      if(depth-150.) 1,2,3
    2 col1=7
      col2=8
      dpth1=150.
      dpth2=200.
      go to 4
c
c  Depth greater than 150
c
    3 col1=depth/50 +5
      col2=col1+1
      dpth1=float(col1 - 5)*50.
      dpth2=dpth1 + 50.
      go to 4
c
c Depth less than 150
c
    1 if(depth.le.15.) then
        col1=1
        col2=2
        dpth1=0.
        dpth2=15.
      elseif(depth.le.40.) then
        col1=2
        col2=3
        dpth1=15.
        dpth2=40.
      elseif(depth.le.50.) then
        col1=3
        col2=4
        dpth1=40.
        dpth2=50.
      elseif(depth.le.75.) then
        col1=4
        col2=5
        dpth1=50.
        dpth2=75.
      elseif(depth.le.100.) then
        col1=5
        col2=6
        dpth1=75.
        dpth2=100.
      elseif(depth.le.125.) then
        col1=6
        col2=7
        dpth1=125.
        dpth2=150.
      endif
c
c  read the table
c
c  skip to row1
c
    4 do 5 i=1,row1-1
         read(itbl,101) skip
    5 continue
  101 format(a10)
c
c     read row1 and row2
c
      read(itbl,100) (time1(i),i=1,21)
      read(itbl,100) (time2(i),i=1,21)
  100 format(6x,21f8.3)
      close(itbl)
c
c   Now interpolate
c
      dint=dpth2-dpth1
      ddiff=depth-dpth1
      gcint=gc2 - gc1
      gcdiff=gcarc-gc1
      drat=ddiff/dint
      gcrat=gcdiff/gcint
      a=time1(col2) - time1(col1)
      b=time2(col2) - time2(col1)
      early=time1(col1) + drat*a
      late= time2(col1) + drat*b
      ptrvlt=early + (late-early)*gcdiff
      return
      end
