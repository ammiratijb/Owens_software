      subroutine asktxt(quest,answer)
c
c   interactive i-o for character strings
c
      character answer*(*),quest*(*)
      integer ounit
      common /innout/ inunit,ounit
      write(ounit,'(A)',ADVANCE='NO') quest(1:len(quest))
      read(inunit,'(A)') answer
      return
      end
