real(ReKi) function InductionBrentsSolve( min, max)
        
        !verifyInterval(min, max);

        !double ret = Double.NaN;

        real(ReKi) yMin, yMax
        
        = f.value(min);
        double yMax = f.value(max);

        // Verify bracketing
        double sign = yMin * yMax;
        if (sign > 0) {
            // check if either value is close to a zero
            if (Math.abs(yMin) <= functionValueAccuracy) {
                setResult(min, 0);
                ret = min;
            } else if (Math.abs(yMax) <= functionValueAccuracy) {
                setResult(max, 0);
                ret = max;
            } else {
                // neither value is close to zero and min and max do not bracket root.
                throw MathRuntimeException.createIllegalArgumentException(
                        NON_BRACKETING_MESSAGE, min, max, yMin, yMax);
            }
        } else if (sign < 0){
            // solve using only the first endpoint as initial guess
            ret = solve(f, min, yMin, max, yMax, min, yMin);
        } else {
            // either min or max is a root
            if (yMin == 0.0) {
                ret = min;
            } else {
                ret = max;
            }
        }

        return ret;
   end function InductionBrentsSolve
   
real(ReKi) function BrentSolve(maximalIterationCount, x0, y0, x1, y1, x2, y2)
   integer,    intent(in) :: maximalIterationCount
   real(ReKi), intent(in) :: x0,y0,x1,y1,x2,y2
   
   real(ReKi) :: delta
   real(ReKi) :: oldDelta
   integer    :: i
   real(ReKi) :: dx, r, r1, r2, p, p1
   real(ReKi) :: tolerance
   
   delta    = x1 - x0
   oldDelta = delta
   i        = 0
   
   do while (i < maximalIterationCount) 
      if (abs(y2) < abs(y1)) then
            ! use the bracket point if is better than last approximation
         x0 = x1
         x1 = x2
         x2 = x0
         y0 = y1
         y1 = y2
         y2 = y0
      end if
      
      if (abs(y1) <= functionValueAccuracy) then
            ! Avoid division by very small values. Assume
            ! the iteration has converged (the problem may
            ! still be ill conditioned)
            setResult(x1, i)
            return 
      end if
      dx = x2 - x1;
      tolerance = Math.max(relativeAccuracy * abs(x1), absoluteAccuracy);
      if ( abs(dx) <= tolerance ) then
            setResult(x1, i)
            return 
      end if
      if ( (abs(oldDelta) < tolerance) .or. (abs(y0) <= Math.abs(y1))) then
            ! Force bisection.
            delta    = 0.5 * dx
            oldDelta = delta;
      else 
            r3 = y1 / y0
            
            ! the equality test (x0 == x2) is intentional,
            ! it is part of the original Brent's method,
            ! it should NOT be replaced by proximity test
            if ( EqualRealNos(x0, x2) ) then
               ! Linear interpolation.
               p  = dx * r3
               p1 = 1.0 - r3
            else 
! Inverse quadratic interpolation.
               r1 = y0 / y2
               r2 = y1 / y2
               p  = r3 * (dx * r1 * (r1 - r2) - (x1 - x0) * (r2 - 1.0))
               p1 = (r1 - 1.0) * (r2 - 1.0) * (r3 - 1.0)
            end if
            if (p > 0.0_ReKi) then
               p1 = -p1
            else 
               p  = -p
            end if
            
            if ( ( 2.0 * p >= ( 1.5 * dx * p1 - abs(tolerance * p1) ) ) .or. ( p >= abs(0.5 * oldDelta * p1) ) ) then
               ! Inverse quadratic interpolation gives a value
               ! in the wrong direction, or progress is slow.
               ! Fall back to bisection.
               delta    = 0.5 * dx
               oldDelta = delta
            else 
               oldDelta = delta
               delta = p / p1
            end if
      end if
      ! Save old X1, Y1
      x0 = x1
      y0 = y1
      ! Compute new X1, Y1
      if ( abs(delta) > tolerance) then
            x1 = x1 + delta
      else if (dx > 0.0) then
            x1 = x1 + 0.5 * tolerance
      else if (dx <= 0.0) then
            x1 = x1 - 0.5 * tolerance
      end if
      y1 = f.value(x1)
      if ( (y1 > 0) == (y2 > 0) ) then
            x2 = x0
            y2 = y0
            delta = x1 - x0
            oldDelta = delta
      end if
      i = i + 1
   end while
            
   ErrMsg = throw new MaxIterationsExceededException(maximalIterationCount);
end function
   
   
