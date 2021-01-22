      function iask(quest)
c
c      interactive i-o for integers
c
      integer inunit,ounit
      integer blank, period
      character quest*(*)
      character answer*32
      common /innout/ inunit,ounit
    1 write(unit=ounit,fmt="(A)",ADVANCE="NO") quest(1:len(quest))
      read(inunit,'(A)') answer
c
c Insist that the answer be an integer.  This test assumes someone enters a real number.
c If someone answers with a character string, this test fails and the code crashes.
c Maybe next time I'll fix that.
c
      if(period(answer).le.blank(answer)) then
        write(unit=ounit, fmt='(A)') "   **** Question requires an INTEGER response, try again ****"
        go to 1
      else
      read(answer, 100) iask
  100 format(i32)
      endif
      return
      end
