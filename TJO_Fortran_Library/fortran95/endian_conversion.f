      module convert_endian
c
c Module of endian conversion routines
c
c Revisions:
c     June 13, 2007 - TJO - original code
c
      contains
      function convert_endian_integer(input)
      integer :: input, output
      call mvbits(input,0,8,output,24)
      call mvbits(input,8,8,output,16)
      call mvbits(input,16,8,output,8)
      call mvbits(input,24,8,output,0)
      convert_endian_integer=output
      return
      end function convert_endian_integer
c
      function convert_endian_real(input)
      integer, parameter :: INTEGER_MOLD = 0
      real, parameter :: REAL_MOLD = 0.0
      real :: input, output
      integer :: xfer_in, xfer_out
      xfer_in=transfer(input,INTEGER_MOLD)
      xfer_out=convert_endian_integer(xfer_in)
      convert_endian_real=transfer(xfer_out,REAL_MOLD)
      return
      end function convert_endian_real
c
      subroutine convert_endian_real_array(x,npts,MAXPTS)
      integer :: npts, MAXPTS
      real :: x(MAXPTS)
      if(npts.gt.MAXPTS) then
        print *, 'MAXPTS exceeded in endian_convert_array.  npts= ', npts, ' MAXPTS = ', MAXPTS
        return
      endif
      do 1 i=1,npts
         x(i)=convert_endian_real(x(i))
    1 continue
      return
      end subroutine convert_endian_real_array
c
      end module convert_endian
