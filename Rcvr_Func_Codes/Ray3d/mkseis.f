      subroutine mkseis(x,y,instrm,qcorr,tq,nft,dt,kst,ihilb)
      complex x(1),wave,fsorce,y(1)
      dimension trap(4)
      logical instrm,qcorr,ounit
      common /innout/ inunit,ounit
      data pi/3.141592654/
      fcut=.004
      nfpts=nft/2 + 1
      fny=1./(2.*dt)
      delf=fny/float(nft/2)
      call dfftr(x,nft,'forward',dt)
      if(ihilb.eq.1) call dfftr(y,nft,'forward',dt)
      if(kst.gt.0) go to 6
    1 isorfn=iask('Pick source wavelet (1-7,not 6): ')
      if(isorfn.eq.6) go to 1
      wave=fsorce(isorfn,0.,0.,kst,a,b,tt,wo,trap)
    6 do 2 i=1,nfpts
         f=float(i-1)*delf
         wave=(1.,0.)
         wave=fsorce(isorfn,f,0.,kst,a,b,tt,wo,trap)
         xr=1.
         xi=0.
         if(.not.instrm) got o 3
            call seisio(f,3000.,xr,xi,+1)
    3    if(.not.qcorr) go to 4
            if(f.lt.fcut) go to 4
               wave=wave*cmplx(exp(-pi*f*tq),0.)
    5          dfac=f*tq*alog(abs(f/fcut)**2-1.)
               dr=cos(dfac)
               di=sin(dfac)
               wave=wave*cmplx(dr,di)
    4     x(i)=wave*x(i)*cmplx(xr,xi)
          if(ihilb.eq.0) go to 2
          x(i)=x(i) + y(i)*cmplx(aimag(wave),-real(wave))*cmplx(xr,xi)
    2 continue
      call dfftr(x,nft,'inverse',delf)
      return
      end
