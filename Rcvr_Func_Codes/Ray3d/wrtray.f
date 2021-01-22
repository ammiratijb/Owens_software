      subroutine wrtray(lyr,az,p,time,baz,p0,pors,init,i4,i10,i11
     *                  ,oldlyr,sini,tdirec)
c
c  writes the results of a ray tracing loop into unit 10
c
      dimension type(2),wave(4,2),prim(2,2)
      logical pors,emult
      character type*1,prim*2,wave*3
      data type/'p','s'/,prim/'pp','ss','ps','sp'/,
     *     wave/'pmp','pms','smp','sms','sms','smp','pms','pmp'/
      angle=asin(sini)
      angle=angle*57.2957795
      emult=.false.
      if(i4.ne.0) go to 8
         emult=.true.
         go to 1
    8 if(init.ne.0) go to 1
         iprim=1
         if(.not.pors) iprim=2
         if(i4.ne.1) go to 6
            itype=1
            if(.not.pors) itype=2
            write(9,100) type(itype),baz,p0,tdirec
            t1=0.
            write(9,102)
            write(9,101) prim(iprim,i4),t1,az,p,angle
            oldlyr=0
            return
    6    write(9,103) lyr
         write(9,102)
         write(9,104) prim(iprim,i4),lyr,time,az,p,angle
         oldlyr=lyr
         return
    1 ip=1
      if(.not.pors) ip=2
      if(i10.ne.1) goto 2
         if(i11.eq.1) go to 3
            iwave=2
            go to 5
    3       iwave=1
            go to 5
    2 if(i11.eq.1) go to 4
         iwave=4
         go to 5
    4    iwave=3
    5 if(emult) go to 9
      if(lyr.eq.oldlyr) go to 7
        write(9,103) lyr
         write(9,102)
         oldlyr=lyr
    7 write(9,105) prim(ip,i4),wave(iwave,ip),lyr,time,az,p,angle
      return
    9 if(iwave.eq.1.and.ip.eq.1) write(9,106) lyr
      write(9,107) wave(iwave,ip),time,az,p,angle
      return
  100 format(///' incident ',a1,'-wave, back azimuth: ',f6.2,
     *       ' ray parameter: ',f7.4,/,' direct arrival spends ',f7.3,
     *       ' secs in layering',/,' all times relative to direct ray'
     *       ,/)
  101 format(5x,a2,5x,'direct',2x,f7.3,3x,f7.2,7x,f7.4,6x,f5.2)
  102 format(' wave type   layer    time     azimuth     ray param.',
     *       '   angle')
  103 format(1x,/,' layer ',i2)
  104 format(5x,a2,7x,i2,4x,f7.3,3x,f7.2,7x,f7.4,6x,f5.2)
  105 format(3x,a2,a3,6x,i2,4x,f7.3,3x,f7.2,7x,f7.4,6x,f5.2)
  106 format(63x,'extra multiples from layer ',i2,/,
     *       63x,' type    time        az.          p         angle')
  107 format(64x,a3,3x,f7.3,4x,f7.2,6x,f7.4,6x,f5.2)
      end
