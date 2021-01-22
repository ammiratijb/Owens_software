      subroutine conversion_moveout(phase,vp,vs,h,ray_parameter,time,distance,ierr)
c
c calculates time and sampling distance for various converted phases
c
c Valid Values of "phase":
c
c     1 = Ps-P 
c     2 = PpPmp - P
c     3 = PpPms - P
c     4 = PpSms - P
c     5 = PsSms - P
c
c  tj owens, 8/2/07 - after old phasefit and moveout codes
c
      real :: vp,vs,h,ray_parameter,time,distance
      integer :: phase,ierr
c
      vpvs=vp/vs
      raysq=ray_parameter*ray_parameter
      ierr=0
c
      p_vslow=sqrt((1./(vp*vp)) - raysq)
      s_vslow=sqrt((1./(vs*vs)) - raysq)
      sqpvp=sqrt(1. - (vp*vp*raysq))      
      sqpvs=sqrt(1. - (vs*vs*raysq))         
c
      xp=ray_parameter*h/p_vslow
      tp=h/(vp*sqpvp)      
c
      xs=ray_parameter*h/s_vslow
      ts=h/(vs*sqpvs)         
c
      select case (phase)
c
      case (1)
         time=h*(s_vslow-p_vslow)
         distance=xs
c
      case (2)
         time=2*h*p_vslow
         distance=3*xp
c
      case (3)
         time=h*s_vslow + h*p_vslow
         distance=2*xp + xs
c
      case (4)
         time=2*h*s_vslow
         distance=xp + 2*xs
c
      case (5)
         time=3*h*s_vslow - h*p_vslow
         distance=3*xs
c
      case default
         time=-999.
         distance=-999.
         ierr=1
c
      end select
      return
      end subroutine conversion_moveout
