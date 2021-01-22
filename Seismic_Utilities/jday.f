      program jday
c
c  Program to convert to and from Julian Date
c
c  T.J.Owens, October 9, 1991
c
      character*3 mon(12), capmon(12), month, mixmon(12)
      character*20 argv
      character*2 op
      character*3 cmon
      integer iargs, iargc, doy, ounit
      data mon/'jan','feb','mar','apr','may','jun','jul','aug',
     *         'sep','oct','nov','dec'/
      data capmon/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     *         'SEP','OCT','NOV','DEC'/
      data mixmon/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     *         'Sep','Oct','Nov','Dec'/
      ounit=6
      iargs=iargc()
      if(iargs.eq.0) then
        write(ounit,*) 'Usage:    jday -j jday yr'
	write(ounit,*) '       OR jday -n mn iday yr'
	write(ounit,*) '       OR jday -a MON iday yr'
	write(ounit,*) ' '
	write(ounit,*) 'where yr   => 4 digit year (i.e. 1991)'
	write(ounit,*) '      jday => Julian day of year'
	write(ounit,*) '      mn   => integer month number'
	write(ounit,*) '      iday => integer day of month'
	write(ounit,*) '      MON  => 3 character month (i.e. SEP or sep)'
        stop
      endif
      call getarg(1,argv)
      read(argv,100) op
  100 format(a2)
  101 format(i3)
  102 format(i4)
  103 format(i2)
  104 format(a3)
      if(op(1:2).eq.'-j')  then
	 call getarg(2,argv)
	 read(argv,101) julday
	 if(julday.le.0.or.julday.gt.366) then
	    write(ounit,*) 'Bad Jday: ',julday
	    stop
         endif
	 call getarg(3,argv)
	 read(argv,102) iyr
	 call juli(iyr,julday,month,iday,monum)
	 month=capmon(monum)
      elseif (op(1:2).eq.'-n') then
	 call getarg(2,argv)
	 read(argv,103) monum
	 if(monum.le.0.or.monum.gt.12) then
	    write(ounit,*) 'Bad Month: ',monum
	    stop
         endif
	 call getarg(3,argv)
	 read(argv,103) iday
	 call getarg(4,argv)
	 read(argv,102) iyr
	 julday=doy(monum,iday,iyr)
	 month=capmon(monum)
      elseif (op(1:2).eq.'-a') then
	 call getarg(2,argv)
	 read(argv,104) cmon
	 call getarg(3,argv)
	 read(argv,103) iday
	 call getarg(4,argv)
	 read(argv,102) iyr
	 monum=0
         do 1 i=1,12
	 if(        cmon(1:3).eq.mon(i)(1:3)
     *         .or. cmon(1:3).eq.capmon(i)(1:3)
     *         .or. cmon(1:3).eq.mixmon(i)(1:3)) monum=i
    1    continue
	 if(monum.eq.0) then
	    write(ounit,*) 'Bad Month: ',cmon
	    stop
         endif
	 julday=doy(monum,iday,iyr)
	 month=capmon(monum)
      else
        write(ounit,*) 'Usage:    jday -j jday yr'
	write(ounit,*) '       OR jday -n mn iday yr'
	write(ounit,*) '       OR jday -a MON iday yr'
	write(ounit,*) ' '
	write(ounit,*) 'where yr   => 4 digit year (i.e. 1991)'
	write(ounit,*) '      jday => Julian day of year'
	write(ounit,*) '      mn   => integer month number'
	write(ounit,*) '      iday => integer day of month'
	write(ounit,*) '      MON  => 3 character month (i.e. SEP or sep)'
        stop
      endif
      if(julday.lt.10) then
         write(ounit,105) iyr,julday,month,monum,iday,month,iday,julday,iyr
      elseif (julday.lt.100) then
         write(ounit,106) iyr,julday,month,monum,iday,month,iday,julday,iyr
      else
         write(ounit,107) iyr,julday,month,monum,iday,month,iday,julday,iyr
      endif
  105 format(i4,1x,"00",i1,1x,a3,1x,i2,1x,i2,
     *       "  ===> ",a3,1x,i2," (",i3,"), ",i4)
  106 format(i4,1x,"0",i2,1x,a3,1x,i2,1x,i2,
     *       "  ===> ",a3,1x,i2," (",i3,"), ",i4)
  107 format(i4,1x,i3,1x,a3,1x,i2,1x,i2,
     *       "  ===> ",a3,1x,i2," (",i3,"), ",i4)
      stop
      end
