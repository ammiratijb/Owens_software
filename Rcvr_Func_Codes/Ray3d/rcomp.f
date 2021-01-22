      subroutine rcomp(r3,ncode,nd,sini,incdnt)
c
c   resolves a reflection coeficient r from s/r coef8 into
c     x and z components (in the ray coordinate system)
c     given the resulting ray type:
c       reflected p => ncode = 1
c       reflected s => ncode = 2
c       transmitted p => ncode = 3
c       transmitted s => ncode = 4
c
      dimension r3(3)
      logical incdnt
      cosi=sqrt(1. - sini*sini)
      r3(2)=0.
      if(incdnt) go to 10
      if(nd.ne.0) go to 5
      go to (1,2,3,4) ncode
    1 r3(3)=cosi
      r3(1)=sini
      return
    2 r3(3)=sini
      r3(1)=-cosi
      return
    3 r3(3)=-cosi
      r3(1)=sini
      return
    4 r3(3)=sini
      r3(1)=cosi
      return
    5 go to (6,7,8,9) ncode
    6 r3(3)=cosi
      r3(1)=-sini
      return
    7 r3(3)=-sini
      r3(1)=-cosi
      return
    8 r3(3)=-cosi
      r3(1)=-sini
      return
    9 r3(3)=-sini
      r3(1)=cosi
      return
   10 if(nd.ne.0) go to 11
      go to (3,4) ncode
   11 go to (8,9) ncode
      end
