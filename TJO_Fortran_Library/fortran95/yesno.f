      logical function yesno(quest)
c
c   interactive i-o for logical variables
c    yesno must be declared logical in calling program
c
      character quest*(*),answer*1
      logical lanswr
      integer ounit
      common /innout/ inunit,ounit
      write(unit=ounit,fmt='(A)',ADVANCE="NO") quest(1:len(quest))
      read(inunit,200) answer
      lanswr=.false.
      if(answer.eq.'y') lanswr=.true.
      yesno=lanswr
  200 format(a1)
      return
      end
