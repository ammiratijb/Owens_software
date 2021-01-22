      subroutine norvec(strike,dip,eta)
c
c  calculates the interface unit normal vector, given the layer
c    strike and dip in radians
c
      dimension eta(3)
      sins=sin(strike)
      coss=cos(strike)
      sind=sin(dip)
      cosd=cos(dip)
      eta(1)=sind*sins
      eta(2)=-sind*coss
      eta(3)=cosd
      return
      end
