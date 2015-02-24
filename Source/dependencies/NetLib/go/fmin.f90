module fminMod

   use NWTC_Library
   use fminfcn 
   implicit none
   private

   

   public :: fmin
   public :: local_min
   
   contains
   
!  To get d1mach, mail netlib
!       send d1mach from core
   real(ReKi) function fmin(ax,bx, tol, fcnArgs)
      real(ReKi),         intent(in   ) :: ax,bx,tol
      type(fmin_fcnArgs), intent(inout) :: fcnArgs   ! inout because calling routine may set  data (errStat and errMsg)
      
!
!      an approximation  x  to the point where  f  attains a minimum  on
!  the interval  (ax,bx)  is determined.
!
!  input..
!
!  ax    left endpoint of initial interval
!  bx    right endpoint of initial interval
!  f     function subprogram which evaluates  f(x)  for any  x
!        in the interval  (ax,bx)
!  tol   desired length of the interval of uncertainty of the final
!        result (.ge.0.)
!
!  output..
!
!  fmin  abcissa approximating the point where  f  attains a
!        minimum
!
!      the method used is a combination of  golden  section  search  and
!  successive parabolic interpolation.  convergence is never much slower
!  than  that  for  a  fibonacci search.  if  f  has a continuous second
!  derivative which is positive at the minimum (which is not  at  ax  or
!  bx),  then  convergence  is  superlinear, and usually of the order of
!  about  1.324....
!      the function  f  is never evaluated at two points closer together
!  than  eps*abs(fmin)+(tol/3), where eps is  approximately  the  square
!  root  of  the  relative  machine  precision.   if   f   is a unimodal
!  function and the computed values of   f   are  always  unimodal  when
!  separated  by  at least  eps*abs(x)+(tol/3), then  fmin  approximates
!  the abcissa of the global minimum of  f  on the interval  ax,bx  with
!  an error less than  3*eps*abs(fmin)+tol.  if   f   is  not  unimodal,
!  then fmin may approximate a local, but perhaps non-global, minimum to
!  the same accuracy.
!      this function subprogram is a slightly modified  version  of  the
!  algol  60 procedure  localmin  given in richard brent, algorithms for
!  minimization without derivatives, prentice-hall, inc. (1973).
!
!
      real(ReKi)  a,b,c,d,e,eps,xm,p,q,r,tol1,t2,u,v,w,fu,fv,fw,fx,x,tol3
      
      integer info
      
      !real(ReKi)  dabs,dsqrt,d1mach
!
!  c is the squared inverse of the golden ratio
      c=0.5_ReKi*(3.0_ReKi-sqrt(5.0_ReKi))
!
!  eps is approximately the square root of the relative machine
!  precision.
!
   10 eps=1.2e-16  !epsilon(1.0_ReKi)  !d1mach(4)
      tol1=eps +1.0_ReKi
      eps=sqrt(eps)
!
      a=ax
      b=bx
      v=a+c*(b-a)
      w=v
      x=v
      e=0.0_ReKi
      fx=fmin_fcn(x, info, fcnArgs)
      fv=fx
      fw=fx
      tol3=tol/3.0_ReKi
!
!  main loop starts here
!
     
   20 xm=0.5_ReKi*(a+b)
      tol1=eps*abs(x)+tol3
      t2=2.0_ReKi*tol1
!
!  check stopping criterion
!
      if (abs(x-xm).le.(t2-0.5_ReKi*(b-a))) then
         go to 190
      end if
      
      p=0.0_ReKi
      q=0.0_ReKi
      r=0.0_ReKi
      if (abs(e).le.tol1) go to 50
!
!  fit parabola
!
      r=(x-w)*(fx-fv)
      q=(x-v)*(fx-fw)
      p=(x-v)*q-(x-w)*r
      q=2.0_ReKi*(q-r)
      if (q.le.0.0_ReKi) go to 30
      p=-p
      go to 40
   30 q=-q
   40 r=e
      e=d
   50 if ((abs(p).ge.abs(0.5_ReKi*q*r)).or.(p.le.q*(a-x)).or.(p.ge.q*(b-x))) go to 60
!
!  a parabolic-interpolation step
!
      d=p/q
      u=x+d
!
!  f must not be evaluated too close to ax or bx
!
      if (((u-a).ge.t2).and.((b-u).ge.t2)) go to 90
      d=tol1
      if (x.ge.xm) d=-d
      go to 90
!
!  a golden-section step
!
   60 if (x.ge.xm) go to 70
      e=b-x
      go to 80
   70 e=a-x
   80 d=c*e
!
!  f must not be evaluated too close to x
!
   90 if (abs(d).lt.tol1) go to 100
      u=x+d
      go to 120
  100 if (d.le.0.0_ReKi) go to 110
      u=x+tol1
      go to 120
  110 u=x-tol1
  120 fu=fmin_fcn(u, info, fcnArgs) !f(u)
!
!  update  a, b, v, w, and x
!
      if (fx.gt.fu) go to 140
      if (u.ge.x) go to 130
      a=u
      go to 140
  130 b=u
  140 if (fu.gt.fx) go to 170
      if (u.ge.x) go to 150
      b=x
      go to 160
  150 a=x
  160 v=w
      fv=fw
      w=x
      fw=fx
      x=u
      fx=fu
      go to 20
  170 if ((fu.gt.fw).and.(w.ne.x)) go to 180
      v=w
      fv=fw
      w=u
      fw=fu
      go to 20
  180 if ((fu.gt.fv).and.(v.ne.x).and.(v.ne.w)) go to 20
      v=u
      fv=fu
      go to 20
!
!  end of main loop
!
  190 fmin=x
      return
   end function fmin
     
   function local_min ( a, b,  t, x, fcnArgs )
   
!*****************************************************************************80
!
!! LOCAL_MIN seeks a local minimum of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    The method used is a combination of golden section search and
!    successive parabolic interpolation.  Convergence is never much slower
!    than that for a Fibonacci search.  If F has a continuous second
!    derivative which is positive at the minimum (which is not at A or
!    B), then convergence is superlinear, and usually of the order of
!    about 1.324....
!
!    The values EPS and T define a tolerance TOL = EPS * abs ( X ) + T.
!    F is never evaluated at two points closer than TOL.  
!
!    If F is a unimodal function and the computed values of F are always
!    unimodal when separated by at least SQEPS * abs ( X ) + (T/3), then
!    LOCAL_MIN approximates the abscissa of the global minimum of F on the 
!    interval [A,B] with an error less than 3*SQEPS*abs(LOCAL_MIN)+T.  
!
!    If F is not unimodal, then LOCAL_MIN may approximate a local, but 
!    perhaps non-global, minimum to the same accuracy.
!
!    Thanks to Jonathan Eggleston for pointing out a correction to the 
!    golden section step, 01 July 2013.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    01 July 2013
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization Without Derivatives,
!    Dover, 2002,
!    ISBN: 0-486-41998-3,
!    LC: QA402.5.B74.
!
!  Parameters:
!
!    Input, real ( ReKi ) A, B, the endpoints of the interval.
!
!    Input, real ( ReKi ) EPS, a positive relative error tolerance.
!    EPS should be no smaller than twice the relative machine precision,
!    and preferably not much less than the square root of the relative
!    machine precision.
!
!    Input, real ( ReKi ) T, a positive absolute error tolerance.
!
!    Input, external real ( ReKi ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose local minimum is being sought.
!
!    Output, real ( ReKi ) X, the estimated value of an abscissa
!    for which F attains a local minimum value in [A,B].
!
!    Output, real ( ReKi ) LOCAL_MIN, the value F(X).
!
  implicit none
  type(fmin_fcnArgs), intent(inout) :: fcnArgs   ! inout because calling routine may set  data (errStat and errMsg)
  real ( ReKi ) a
  real ( ReKi ) b
  real ( ReKi ) c
  real ( ReKi ) d
  real ( ReKi ) e
  real ( ReKi ) eps
  !real ( ReKi ) f
  real ( ReKi ) fu
  real ( ReKi ) fv
  real ( ReKi ) fw
  real ( ReKi ) fx
  real ( ReKi ) local_min
  real ( ReKi ) m
  real ( ReKi ) p
  real ( ReKi ) q
  real ( ReKi ) r
  real ( ReKi ) sa
  real ( ReKi ) sb
  real ( ReKi ) t
  real ( ReKi ) t2
  real ( ReKi ) tol
  real ( ReKi ) u
  real ( ReKi ) v
  real ( ReKi ) w
  real ( ReKi ) x
  integer info
  eps  = epsilon(1.0_ReKi)
!
!  C is the square of the inverse of the golden ratio.
!
  c = 0.5D+00 * ( 3.0D+00 - sqrt ( 5.0D+00 ) )

  sa = a
  sb = b
  x = sa + c * ( b - a )
  w = x
  v = w
  e = 0.0D+00
  fx = fmin_fcn(x, info, fcnArgs)
  fw = fx
  fv = fw

  do

    m = 0.5D+00 * ( sa + sb ) 
    tol = eps * abs ( x ) + t
    t2 = 2.0D+00 * tol
!
!  Check the stopping criterion.
!
    if ( abs ( x - m ) <= t2 - 0.5D+00 * ( sb - sa ) ) then
      return
    end if
!
!  Fit a parabola.
!
    r = 0.0D+00
    q = r
    p = q

    if ( tol < abs ( e ) ) then

      r = ( x - w ) * ( fx - fv )
      q = ( x - v ) * ( fx - fw )
      p = ( x - v ) * q - ( x - w ) * r
      q = 2.0D+00 * ( q - r )

      if ( 0.0D+00 < q ) then
        p = - p
      end if

      q = abs ( q )

      r = e
      e = d

    end if

    if ( abs ( p ) < abs ( 0.5D+00 * q * r ) .and. &
         q * ( sa - x ) < p .and. &
         p < q * ( sb - x ) ) then
!
!  Take the parabolic interpolation step.
!
      d = p / q
      u = x + d
!
!  F must not be evaluated too close to A or B.
!
      if ( ( u - sa ) < t2 .or. ( sb - u ) < t2 ) then

        if ( x < m ) then
          d = tol
        else
          d = - tol
        end if

      end if
!
!  A golden-section step.
!
    else

      if ( x < m ) then
        e = sb - x
      else
        e = sa - x
      end if

      d = c * e

    end if
!
!  F must not be evaluated too close to X.
!
    if ( tol <= abs ( d ) ) then
      u = x + d
    else if ( 0.0D+00 < d ) then
      u = x + tol
    else
      u = x - tol
    end if

    fu = fmin_fcn(u, info, fcnArgs) !f ( u )
!
!  Update A, B, V, W, and X.
!
    if ( fu <= fx ) then

      if ( u < x ) then
        sb = x
      else
        sa = x
      end if

      v = w
      fv = fw
      w = x
      fw = fx
      x = u
      fx = fu

    else

      if ( u < x ) then
        sa = u
      else
        sb = u
      end if

      if ( fu <= fw .or. w == x ) then
        v = w
        fv = fw
        w = u
        fw = fu
      else if ( fu <= fv .or. v == x .or. v == w ) then
        v = u
        fv = fu
      end if

    end if

  end do

  local_min = fx

  return
end function local_min
end module fminMod
