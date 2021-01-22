      module sac_access
c
c Module to set up a data structure for SAC headers
c
c Revisions:
c     May 28, 2007 - TJO - original code
c     June 12, 2007 - TJO - added endianness detection on sac_read
c     June 13, 2007 - TJO - added ability to read non-native endian files
c
      type :: sac_header
c
c *********************
c    SAC HEADER VARIABLES
c **********************
c
      real :: delta,     depmin,    depmax,    scale,     odelta    
      real :: b,         e,         o,         a,         internal1 
      real :: t0,        t1,        t2,        t3,        t4        
      real :: t5,        t6,        t7,        t8,        t9        
      real :: f,         resp0,     resp1,     resp2,     resp3     
      real :: resp4,     resp5,     resp6,     resp7,     resp8     
      real :: resp9,     stla,      stlo,      stel,      stdp      
      real :: evla,      evlo,      evel,      evdp,      unused1   
      real :: user0,     user1,     user2,     user3,     user4     
      real :: user5,     user6,     user7,     user8,     user9     
      real :: dist,      az,        baz,       gcarc,     internal2 
      real :: internal3, depmen,    cmpaz,     cmpinc,    unused2   
      real :: unused3,   unused4,   unused5,   unused6,   unused7   
      real :: unused8,   unused9,   unused10,  unused11,  unused12  
      integer :: nzyear,    nzjday,    nzhour,    nzmin,     nzsec     
      integer :: nzmsec,    nvhdr, internal5, internal6, npts      
      integer :: internal7, internal8, unused13,  unused14,  unused15  
      integer :: iftype,    idep,      iztype,    unused16,  iinst     
      integer :: istreg,    ievreg,    ievtyp,    iqual,     isynth    
      integer :: unused17,  unused18,  unused19,  unused20,  unused21  
      integer :: unused22,  unused23,  unused24,  unused25,  unused26  
      logical :: leven,     lpspol,    lovrok,    lcalda,    unused27  
      character(len=8) :: kstnm
      character(len=16) ::  kevnm
      character(len=8) :: khole,  ko,     ka              
      character(len=8) :: kt0,    kt1,    kt2              
      character(len=8) :: kt3,    kt4,    kt5              
      character(len=8) :: kt6,    kt7,    kt8              
      character(len=8) :: kt9,    kf,     kuser0           
      character(len=8) :: kuser1, kuser2, kcmpnm           
      character(len=8) :: knetwk, kdatrd, kinst            
c
c ********************
c    END SAC HEADER VARIABLES
c ********************
c
      end type sac_header
      logical, parameter  :: YES_ENDIAN_CONVERSION = .TRUE.
      logical, parameter  :: NO_ENDIAN_CONVERSION = .FALSE.
      contains
      subroutine sac_read(file,x,MAXPTS,read_header,nerr)
c ****************************************************************************
c
c    call to sac_read is:
c
c    where file        = file to be read or written
c          x           = data array to be used
c          MAXPTS      = maximum dimension of x
c          read_header = module of type "sac_header", defined in sac_access
c          nerr        = error detection
c                      nerr=1    - too many points in file
c                      nerr=2    - endianness incompatibility
c                      nerr=-999 - not a SAC file
c
c ******************************************************************************
c
c  Subroutine to read SAC files that does not require access to the SAC library itself.
c
c  tjowens
c  May 28, 2007  - TJO - converted to sac_access module
c  June 13, 2007 - TJO - added ENDIAN conversion
c
      use convert_endian
      type(sac_header) :: read_header
      logical :: do_endian_conversion
      integer, parameter :: VERSION = 6
      character file*256
      integer :: MAXPTS, nerr
      real    :: x(MAXPTS)
      integer :: infile, header_size, blank,iblank
c
c  read a sac file
c
      infile=1
      nerr=0
      iblank=blank(file)
      open(file=file(1:iblank),unit=infile,form='unformatted',access='stream')

      call sac_init_header(read_header)

      inquire(iolength=header_size) read_header
      read(infile,pos=1) read_header
c
c  check for endianness compatibility
c
      do_endian_conversion=NO_ENDIAN_CONVERSION
      if(read_header%nvhdr.ne.VERSION) then
         if(convert_endian_integer(read_header%nvhdr).eq.VERSION) then
c           print *, '  NOTE    ', file(1:iblank), ' ==> s/r sac_read doing ENDIAN CONVERSION'
            nerr=2
            do_endian_conversion=YES_ENDIAN_CONVERSION
         else
            print *, file(1:iblank), ' => NOT A SAC FILE ... skipping'
            nerr=-999
            return
         endif
      endif
c
      if(do_endian_conversion) call convert_endian_sac_header(read_header)
c
      if(read_header%npts.gt.MAXPTS) then
        print *, 'MAX PTS EXCEEDED FOR ', file(1:iblank), ' MAXPTS =', MAXPTS, ' npts = ', read_header%npts
        nerr=1
        return
      endif

      read(infile, pos=header_size+1) (x(i),i=1,read_header%npts)
c
      if(do_endian_conversion) call convert_endian_real_array(x,read_header%npts,MAXPTS)
c
      close(infile)
      return
c
      end subroutine sac_read
c
      subroutine sac_write(file,x,MAXPTS,write_header,nerr)
c ****************************************************************************
c
c    call to sac_write is:
c
c    where file         = file to be read or written
c          x            = data array to be used
c          MAXPTS       = maximum array dimension of x
c          write_header = module of type "sac_header", defined in sac_access
c          nerr         = error index
c
c ******************************************************************************
c
c  Subroutine to write SAC files that does not require access to the SAC library itself.
c
c  Assumes you have filled write_header structure with the "right" stuff
c
c  tjowens
c  May 28, 2007 - TJO - converted to sac_access module
c
      type(sac_header) :: write_header
      character(len=256) file
      integer MAXPTS, nerr
      real x(MAXPTS)
      integer infile, header_size, blank, iblank

      nerr=0
      if(write_header%npts.gt.MAXPTS) then
        print *, 'MAX PTS EXCEEDED FOR ', file(1:iblank), 'npts = ', write_header%npts
        nerr=1
        return
      endif

      infile=1
      iblank=blank(file)
      open(file=file(1:iblank),unit=infile,form='unformatted',access='stream')

      inquire(iolength=header_size) write_header
      write(infile, pos=1) write_header  

      write(infile,pos=header_size+1) (x(i),i=1,write_header%npts)

      close(infile)
      return
c
      end subroutine sac_write
c
      subroutine wsac1(file,x,MAXPTS,begin,delta,nerr)
c ***************************************************************************
c
c     wsac1 is to duplicate a standard SAC routine for simplicity in writing
c     SAC files without messing with the header
c
c ****************************************************************************
c
c     tjowens, June 1, 2007 - initial version
c
      type(sac_header) :: write_header
      character(len=256) file
      integer MAXPTS, nerr
      real x(MAXPTS)
      integer infile, header_size, blank, iblank

      infile=1
      iblank=blank(file)
      
      call sac_init_header(write_header)
      write_header%npts=MAXPTS
      write_header%b=begin
      write_header%delta=delta

      open(file=file(1:iblank),unit=infile,form='unformatted',access='stream')

      inquire(iolength=header_size) write_header
      write(infile, pos=1) write_header  

      write(infile,pos=header_size+1) (x(i),i=1,write_header%npts)

      close(infile)
      return
c
      end subroutine wsac1
c
      subroutine sac_init_header(new_header)
c ****************************************************************************
c
c    call to sac_init_header is:
c
c    where new_header = module of type "sac_header", defined in sac_access
c
c ******************************************************************************
c
c  Subroutine to read SAC files that does not require access to the SAC library itself.
c
c  tjowens
c  May 28, 2007 - TJO - converted to sac_access module
c
      type(sac_header) :: new_header
      new_header%delta=-12345
      new_header%depmin=-12345
      new_header%depmax=-12345
      new_header%scale=-12345
      new_header%odelta=-12345
      new_header%b=-12345
      new_header%e=-12345
      new_header%o=-12345
      new_header%a=-12345
      new_header%internal1=-12345
      new_header%t0=-12345
      new_header%t1=-12345
      new_header%t2=-12345
      new_header%t3=-12345
      new_header%t4=-12345
      new_header%t5=-12345
      new_header%t6=-12345
      new_header%t7=-12345
      new_header%t8=-12345
      new_header%t9=-12345
      new_header%f=-12345
      new_header%resp0=-12345
      new_header%resp1=-12345
      new_header%resp2=-12345
      new_header%resp3=-12345
      new_header%resp4=-12345
      new_header%resp5=-12345
      new_header%resp6=-12345
      new_header%resp7=-12345
      new_header%resp8=-12345
      new_header%resp9=-12345
      new_header%stla=-12345
      new_header%stlo=-12345
      new_header%stel=-12345
      new_header%stdp=-12345
      new_header%evla=-12345
      new_header%evlo=-12345
      new_header%evel=-12345
      new_header%evdp=-12345
      new_header%unused1=-12345
      new_header%user0=-12345
      new_header%user1=-12345
      new_header%user2=-12345
      new_header%user3=-12345
      new_header%user4=-12345
      new_header%user5=-12345
      new_header%user6=-12345
      new_header%user7=-12345
      new_header%user8=-12345
      new_header%user9=-12345
      new_header%dist=-12345
      new_header%az=-12345
      new_header%baz=-12345
      new_header%gcarc=-12345
      new_header%internal2=-12345
      new_header%internal3=-12345
      new_header%depmen=-12345
      new_header%cmpaz=-12345
      new_header%cmpinc=-12345
      new_header%unused2=-12345
      new_header%unused3=-12345
      new_header%unused4=-12345
      new_header%unused5=-12345
      new_header%unused6=-12345
      new_header%unused7=-12345
      new_header%unused8=-12345
      new_header%unused9=-12345
      new_header%unused10=-12345
      new_header%unused11=-12345
      new_header%unused12=-12345
      new_header%nzyear=-12345
      new_header%nzjday=-12345
      new_header%nzhour=-12345
      new_header%nzmin=-12345
      new_header%nzsec=-12345
      new_header%nzmsec=-12345
      new_header%nvhdr=6
      new_header%internal5=0
      new_header%internal6=0
      new_header%npts=-12345
      new_header%internal7=-12345
      new_header%internal8=-12345
      new_header%unused13=-12345
      new_header%unused14=-12345
      new_header%unused15=-12345
      new_header%iftype=1
      new_header%idep=-12345
      new_header%iztype=9
      new_header%unused16=-12345
      new_header%iinst=-12345
      new_header%istreg=-12345
      new_header%ievreg=-12345
      new_header%ievtyp=-12345
      new_header%iqual=-12345
      new_header%isynth=-12345
      new_header%unused17=-12345
      new_header%unused18=-12345
      new_header%unused19=-12345
      new_header%unused20=-12345
      new_header%unused21=-12345
      new_header%unused22=-12345
      new_header%unused23=-12345
      new_header%unused24=-12345
      new_header%unused25=-12345
      new_header%unused26=-12345
      new_header%leven=.TRUE.
      new_header%lpspol=.FALSE.
      new_header%lovrok=.TRUE.
      new_header%lcalda=.TRUE.
      new_header%unused27=.FALSE.
      new_header%kstnm='-12345'
      new_header%kevnm='-12345'
      new_header%khole='-12345'
      new_header%ko='-12345'
      new_header%ka='-12345'
      new_header%kt0='-12345'
      new_header%kt1='-12345'
      new_header%kt2='-12345'
      new_header%kt3='-12345'
      new_header%kt4='-12345'
      new_header%kt5='-12345'
      new_header%kt6='-12345'
      new_header%kt7='-12345'
      new_header%kt8='-12345'
      new_header%kt9='-12345'
      new_header%kf='-12345'
      new_header%kuser0='-12345'
      new_header%kuser1='-12345'
      new_header%kuser2='-12345'
      new_header%kcmpnm='-12345'
      new_header%knetwk='-12345'
      new_header%kdatrd='-12345'
      new_header%kinst='-12345'
      return
c
      end subroutine sac_init_header
c
      subroutine convert_endian_sac_header(existing_header)
c ******************************************************************************
c
c  Subroutine to convert reals and integers of an existing sac header
c
c  tjowens
c  June 13, 2007 - TJO - original code
c
      use convert_endian
      type(sac_header) :: existing_header
c
      existing_header%delta=convert_endian_real(existing_header%delta)
      existing_header%depmin=convert_endian_real(existing_header%depmin)
      existing_header%depmax=convert_endian_real(existing_header%depmax)
      existing_header%scale=convert_endian_real(existing_header%scale)
      existing_header%odelta=convert_endian_real(existing_header%odelta)
      existing_header%b=convert_endian_real(existing_header%b)
      existing_header%e=convert_endian_real(existing_header%e)
      existing_header%o=convert_endian_real(existing_header%o)
      existing_header%a=convert_endian_real(existing_header%a)
      existing_header%internal1=convert_endian_real(existing_header%internal1)
      existing_header%t0=convert_endian_real(existing_header%t0)
      existing_header%t1=convert_endian_real(existing_header%t1)
      existing_header%t2=convert_endian_real(existing_header%t2)
      existing_header%t3=convert_endian_real(existing_header%t3)
      existing_header%t4=convert_endian_real(existing_header%t4)
      existing_header%t5=convert_endian_real(existing_header%t5)
      existing_header%t6=convert_endian_real(existing_header%t6)
      existing_header%t7=convert_endian_real(existing_header%t7)
      existing_header%t8=convert_endian_real(existing_header%t8)
      existing_header%t9=convert_endian_real(existing_header%t9)
      existing_header%f=convert_endian_real(existing_header%f)
      existing_header%resp0=convert_endian_real(existing_header%resp0)
      existing_header%resp1=convert_endian_real(existing_header%resp1)
      existing_header%resp2=convert_endian_real(existing_header%resp2)
      existing_header%resp3=convert_endian_real(existing_header%resp3)
      existing_header%resp4=convert_endian_real(existing_header%resp4)
      existing_header%resp5=convert_endian_real(existing_header%resp5)
      existing_header%resp6=convert_endian_real(existing_header%resp6)
      existing_header%resp7=convert_endian_real(existing_header%resp7)
      existing_header%resp8=convert_endian_real(existing_header%resp8)
      existing_header%resp9=convert_endian_real(existing_header%resp9)
      existing_header%stla=convert_endian_real(existing_header%stla)
      existing_header%stlo=convert_endian_real(existing_header%stlo)
      existing_header%stel=convert_endian_real(existing_header%stel)
      existing_header%stdp=convert_endian_real(existing_header%stdp)
      existing_header%evla=convert_endian_real(existing_header%evla)
      existing_header%evlo=convert_endian_real(existing_header%evlo)
      existing_header%evel=convert_endian_real(existing_header%evel)
      existing_header%evdp=convert_endian_real(existing_header%evdp)
      existing_header%unused1=convert_endian_real(existing_header%unused1)
      existing_header%user0=convert_endian_real(existing_header%user0)
      existing_header%user1=convert_endian_real(existing_header%user1)
      existing_header%user2=convert_endian_real(existing_header%user2)
      existing_header%user3=convert_endian_real(existing_header%user3)
      existing_header%user4=convert_endian_real(existing_header%user4)
      existing_header%user5=convert_endian_real(existing_header%user5)
      existing_header%user6=convert_endian_real(existing_header%user6)
      existing_header%user7=convert_endian_real(existing_header%user7)
      existing_header%user8=convert_endian_real(existing_header%user8)
      existing_header%user9=convert_endian_real(existing_header%user9)
      existing_header%dist=convert_endian_real(existing_header%dist)
      existing_header%az=convert_endian_real(existing_header%az)
      existing_header%baz=convert_endian_real(existing_header%baz)
      existing_header%gcarc=convert_endian_real(existing_header%gcarc)
      existing_header%internal2=convert_endian_real(existing_header%internal2)
      existing_header%internal3=convert_endian_real(existing_header%internal3)
      existing_header%depmen=convert_endian_real(existing_header%depmen)
      existing_header%cmpaz=convert_endian_real(existing_header%cmpaz)
      existing_header%cmpinc=convert_endian_real(existing_header%cmpinc)
      existing_header%unused2=convert_endian_real(existing_header%unused2)
      existing_header%unused3=convert_endian_real(existing_header%unused3)
      existing_header%unused4=convert_endian_real(existing_header%unused4)
      existing_header%unused5=convert_endian_real(existing_header%unused5)
      existing_header%unused6=convert_endian_real(existing_header%unused6)
      existing_header%unused7=convert_endian_real(existing_header%unused7)
      existing_header%unused8=convert_endian_real(existing_header%unused8)
      existing_header%unused9=convert_endian_real(existing_header%unused9)
      existing_header%unused10=convert_endian_real(existing_header%unused10)
      existing_header%unused11=convert_endian_real(existing_header%unused11)
      existing_header%unused12=convert_endian_real(existing_header%unused12)
      existing_header%nzyear=convert_endian_integer(existing_header%nzyear)
      existing_header%nzjday=convert_endian_integer(existing_header%nzjday)
      existing_header%nzhour=convert_endian_integer(existing_header%nzhour)
      existing_header%nzmin=convert_endian_integer(existing_header%nzmin)
      existing_header%nzsec=convert_endian_integer(existing_header%nzsec)
      existing_header%nzmsec=convert_endian_integer(existing_header%nzmsec)
      existing_header%nvhdr=convert_endian_integer(existing_header%nvhdr)
      existing_header%internal5=convert_endian_integer(existing_header%internal5)
      existing_header%internal6=convert_endian_integer(existing_header%internal6)
      existing_header%npts=convert_endian_integer(existing_header%npts)
      existing_header%internal7=convert_endian_integer(existing_header%internal7)
      existing_header%internal8=convert_endian_integer(existing_header%internal8)
      existing_header%unused13=convert_endian_integer(existing_header%unused13)
      existing_header%unused14=convert_endian_integer(existing_header%unused14)
      existing_header%unused15=convert_endian_integer(existing_header%unused15)
      existing_header%iftype=convert_endian_integer(existing_header%iftype)
      existing_header%idep=convert_endian_integer(existing_header%idep)
      existing_header%iztype=convert_endian_integer(existing_header%iztype)
      existing_header%unused16=convert_endian_integer(existing_header%unused16)
      existing_header%iinst=convert_endian_integer(existing_header%iinst)
      existing_header%istreg=convert_endian_integer(existing_header%istreg)
      existing_header%ievreg=convert_endian_integer(existing_header%ievreg)
      existing_header%ievtyp=convert_endian_integer(existing_header%ievtyp)
      existing_header%iqual=convert_endian_integer(existing_header%iqual)
      existing_header%isynth=convert_endian_integer(existing_header%isynth)
      existing_header%unused17=convert_endian_integer(existing_header%unused17)
      existing_header%unused18=convert_endian_integer(existing_header%unused18)
      existing_header%unused19=convert_endian_integer(existing_header%unused19)
      existing_header%unused20=convert_endian_integer(existing_header%unused20)
      existing_header%unused21=convert_endian_integer(existing_header%unused21)
      existing_header%unused22=convert_endian_integer(existing_header%unused22)
      existing_header%unused23=convert_endian_integer(existing_header%unused23)
      existing_header%unused24=convert_endian_integer(existing_header%unused24)
      existing_header%unused25=convert_endian_integer(existing_header%unused25)
      existing_header%unused26=convert_endian_integer(existing_header%unused26)
      end subroutine convert_endian_sac_header
      end module sac_access
