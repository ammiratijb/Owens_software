      function ask(quest)
c
c   interactive i-o for real numbers
c
      character quest*(*)
      integer ounit
      common /innout/ inunit,ounit
      write(unit=ounit,fmt="(A)",ADVANCE="NO") quest(1:len(quest))
      read(inunit,*) anser
      ask=anser
      return
      end
