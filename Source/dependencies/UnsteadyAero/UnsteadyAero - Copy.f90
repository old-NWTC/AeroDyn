module UnsteadyAero
      
implicit none 

private

INTEGER, PARAMETER              :: R8Ki     = SELECTED_REAL_KIND( 14, 300 )     ! Kind for eight-byte floating-point numbers
INTEGER, PARAMETER              :: SiKi     = SELECTED_REAL_KIND(  6,  30 )     ! Kind for four-byte, floating-point numbers
INTEGER, PARAMETER              :: ReKi     = SiKi                              ! Default kind for floating-point numbers
INTEGER, PARAMETER              :: DbKi     = R8Ki                              ! Default kind for double floating-point numbers

public :: Test_Cn


   contains

real(ReKi) function Get_ds( U, c, dt )
   ! Called by : Get_Alpha_e, UnsteadyAero_UpdateStates
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: dt
   Get_ds = 2.0_ReKi*U*dt/c
end function Get_ds

real(ReKi) function Get_Beta_M( M ) 
   ! Called by : NONE
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: M
   Get_Beta_M = sqrt(1 - M**2)
end function Get_Beta_M

real(ReKi) function Get_Beta_M_Sqrd( M ) 
   ! Called by : Get_k_Alpha, Get_T_q, Get_Alpha_e
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: M
   Get_Beta_M_Sqrd = 1 - M**2
end function Get_Beta_M_Sqrd

real(ReKi) function Get_k_Alpha( M, a_s, c, C_nalpha ) 
   ! Called by : Get_T_Alpha, UnsteadyAero_UpdateStates
   ! Calls  to : Get_Beta_M_Sqrd
   real(ReKi), intent(in   ) :: M                               ! Mach number
   real(ReKi), intent(in   ) :: a_s                             ! speed of sound (m/s)
   real(ReKi), intent(in   ) :: c                               ! chord length (m)
   real(ReKi), intent(in   ) :: C_nalpha                        ! 
   
   
   real(ReKi)                :: k_alpha
   real(ReKi)                :: T_I
   real(ReKi)                :: beta_M_sqrd
   
   ! Implements equation 1.10a
   
   beta_M_sqrd   = Get_Beta_M_Sqrd(M)
   Get_k_Alpha   = 1.0_ReKi / ( (1.0_ReKi - M) + 0.5_ReKi*C_nalpha * M**2 * beta_M_sqrd *  0.413_ReKi  )
end function Get_k_Alpha

real(ReKi) function Get_T_Alpha( M, a_s, c, C_nalpha ) 
   ! Called by : UnsteadyAero_UpdateStates
   ! Calls  to : Get_k_Alpha

   real(ReKi), intent(in   ) :: M                               ! Mach number
   real(ReKi), intent(in   ) :: a_s                             ! speed of sound (m/s)
   real(ReKi), intent(in   ) :: c                               ! chord length (m)
   real(ReKi), intent(in   ) :: C_nalpha                        ! 
   
   
   real(ReKi)                :: k_alpha
   real(ReKi)                :: T_I
   real(ReKi)                :: beta_M_sqrd
   
   ! Implements equation 1.10a
   
   k_alpha       = Get_k_Alpha( M, a_s, c, C_nalpha ) 
   T_I           = c / a_s
   Get_T_Alpha   = k_alpha * T_I
   
end function Get_T_Alpha   


real(ReKi) function Get_T_q(M, a_s, c, C_nalpha) 
   ! Called by : UnsteadyAero_UpdateStates
   ! Calls  to : Get_Beta_M_Sqrd

   real(ReKi), intent(in   ) :: M                               ! Mach number
   real(ReKi), intent(in   ) :: a_s                             ! speed of sound (m/s)
   real(ReKi), intent(in   ) :: c                               ! chord length (m)
   real(ReKi), intent(in   ) :: C_nalpha                        ! 
   
   
   real(ReKi)                :: k_q
   real(ReKi)                :: T_I
   real(ReKi)                :: beta_M_sqrd
   
   ! Implements equation 1.10b
   
   beta_M_sqrd   = Get_Beta_M_Sqrd(M)
   k_q           = 1.0_ReKi / ( (1.0_ReKi - M) + C_nalpha * M**2 * beta_M_sqrd *  0.413_ReKi  )
   T_I           = c / a_s
   Get_T_q       = k_q * T_I
   
end function Get_T_q   


   
real(ReKi) function Get_Pitchrate( alpha_cur, alpha_prev, U, c )
   ! this is the non-dimensional pitching rate, given as q in the documentation: equation 1.8
   !
   ! Called by : UnsteadyAero_UpdateStates
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: alpha_cur                         ! angle of attack, current timestep  (radians)
   real(ReKi), intent(in   ) :: alpha_prev                        ! angle of attack, previous timestep  (radians)
   real(ReKi), intent(in   ) :: U                               ! air velocity magnitude relative to the airfoil (m/s)
   real(ReKi), intent(in   ) :: c                               ! chord length (m)

      ! Implements equation 1.8
   Get_Pitchrate = ( alpha_cur - alpha_prev ) * c / U  
   
end function Get_Pitchrate

real(ReKi) function Get_Cn_Alpha_nc( T_alpha, alpha_cur, alpha_prev, alpha_minus2, Kprime_alpha_prev, dt )
   ! Called by : Get_Cn_Alpha_q_nc
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: T_alpha                         ! mach-dependent time constant related to alpha and non-circulatory terms (???)
   real(ReKi), intent(in   ) :: alpha_cur                         ! angle of attack, current timestep   (radians)
   real(ReKi), intent(in   ) :: alpha_prev                        ! angle of attack, previous timestep  (radians)
   real(ReKi), intent(in   ) :: alpha_minus2                      ! angle of attack, two timesteps ago  (radians)
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: dt                              ! time between simulation steps [note, the same dt is assumed between all alpha values] (sec)
   
   real(ReKi)                :: K_alpha
   real(ReKi)                :: K_alpha_prev
   real(ReKi)                :: Kprime_alpha
   
   ! Implements equation 1.18
   K_alpha         = ( alpha_cur  - alpha_prev   ) / dt
   K_alpha_prev    = ( alpha_prev - alpha_minus2 ) / dt
   Kprime_alpha    = Kprime_alpha_prev * exp( -dt/T_alpha ) + ( K_alpha - K_alpha_prev ) *  exp( -dt / ( 2.0_ReKi*T_alpha ) )
   Get_Cn_Alpha_nc = 4 * T_alpha * ( K_alpha - Kprime_alpha )
   
end function Get_Cn_Alpha_nc 

real(ReKi) function Get_Cn_q_nc( T_q, q_cur, q_prev, q_minus2, Kprima_q_prev, dt )
   ! Called by : Get_Cn_Alpha_q_nc
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: T_q                             ! mach-dependent time constant related to alpha and non-circulatory terms (???)
   real(ReKi), intent(in   ) :: q_cur                           ! non-dimensional pitching rate, current timestep   (radians)
   real(ReKi), intent(in   ) :: q_prev                          ! non-dimensional pitching rate, previous timestep  (radians)
   real(ReKi), intent(in   ) :: q_minus2                        ! non-dimensional pitching rate, two timesteps ago  (radians)
   real(ReKi), intent(in   ) :: Kprima_q_prev
   real(ReKi), intent(in   ) :: dt                              ! time between simulation steps [note, the same dt is assumed between all q values] (sec)
   
   real(ReKi)                :: K_q
   real(ReKi)                :: K_q_prev
   real(ReKi)                :: Kprime_q
   
   ! Implements equation 1.19
   K_q         = ( q_cur  - q_prev   ) / dt
   K_q_prev    = ( q_prev - q_minus2 ) / dt
   Kprime_q    = Kprima_q_prev * exp( -dt/T_q ) + ( K_q - K_q_prev ) *  exp( -dt / ( 2.0_ReKi*T_q ) )
   Get_Cn_q_nc = 4 * T_q * ( K_q - Kprime_q )
   
end function Get_Cn_q_nc 

real(ReKi) function Get_Cn_Alpha_q_nc( T_alpha, alpha_cur, alpha_prev, alpha_minus2, Kprime_alpha_prev, Kprime_q_prev, T_q, q_cur, q_prev, q_minus2, dt )
   ! Called by : Get_Cn_pot, Get_Cn
   ! Calls  to : Get_Cn_Alpha_nc, Get_Cn_q_nc
   real(ReKi), intent(in   ) :: T_alpha                         ! mach-dependent time constant related to alpha and non-circulatory terms (???)
   real(ReKi), intent(in   ) :: alpha_cur                         ! angle of attack, current timestep   (radians)
   real(ReKi), intent(in   ) :: alpha_prev                        ! angle of attack, previous timestep  (radians)
   real(ReKi), intent(in   ) :: alpha_minus2                      ! angle of attack, two timesteps ago  (radians)
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: T_q                             ! mach-dependent time constant related to alpha and non-circulatory terms (???)
   real(ReKi), intent(in   ) :: q_cur                           ! non-dimensional pitching rate, current timestep   (radians)
   real(ReKi), intent(in   ) :: q_prev                          ! non-dimensional pitching rate, previous timestep  (radians)
   real(ReKi), intent(in   ) :: q_minus2                        ! non-dimensional pitching rate, two timesteps ago  (radians)
   real(ReKi), intent(in   ) :: dt                              ! time between simulation steps [note, the same dt is assumed between all q values] (sec)
   
   ! Implements equation 1.17
   Get_Cn_Alpha_q_nc = Get_Cn_Alpha_nc( T_alpha, alpha_cur, alpha_prev, alpha_minus2, Kprime_alpha_prev, dt ) + Get_Cn_q_nc( T_q, q_cur, q_prev, q_minus2, Kprime_q_prev, dt )

end function Get_Cn_Alpha_q_nc



real(ReKi) function Get_Alpha_e( dt, U, M, c, alpha_cur, alpha_prev, alpha0, X_1_prev, X_2_prev, b1, b2, A1, A2 )
   ! Called by : Get_Cn_alpha_q_circ
   ! Calls  to : Get_ds, Get_Beta_M_Sqrd
   ! Implements equation 1.14
   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2

   
   real(ReKi)                :: dalpha
   real(ReKi)                :: X_1
   real(ReKi)                :: X_2
   real(ReKi)                :: ds
   real(ReKi)                :: beta_M_sqrd
   
   dalpha        = alpha_cur - alpha_prev
   ds          = Get_ds(U, c, dt)
   beta_M_sqrd = Get_Beta_M_Sqrd(M)
   X_1         = X_1_prev * exp(-b1*beta_M_sqrd*ds) + A1*exp(-0.5_ReKi*b1*beta_M_sqrd*ds)*dalpha
   X_2         = X_2_prev * exp(-b2*beta_M_sqrd*ds) + A2*exp(-0.5_ReKi*b2*beta_M_sqrd*ds)*dalpha
   Get_Alpha_e = (alpha_cur - alpha0) - X_1 - X_2
   
end function Get_Alpha_e
   
real(ReKi) function Get_Cn_alpha_q_circ( dt, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha0, X_1_prev, X_2_prev, b1, b2, A1, A2 )
   ! Called by : Get_Cn_pot
   ! Calls  to : Get_Alpha_e
   ! Implements equation 1.13
   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   
   Get_Cn_alpha_q_circ = C_nalpha_circ * Get_Alpha_e( dt, U, M, c, alpha_cur, alpha_prev, alpha0, X_1_prev, X_2_prev, b1, b2, A1, A2 )
   
end function Get_Cn_alpha_q_circ


real(ReKi) function Get_Cn_pot( dt, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, &
                               q_cur, q_prev, q_minus2, T_alpha, T_q, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, b1, b2, A1, A2 )
   ! Called by : Get_Cn_prime
   ! Calls  to : Get_Cn_alpha_q_circ, Get_Cn_Alpha_q_nc
   ! Implements equation 1.20
   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha_minus2
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: q_cur
   real(ReKi), intent(in   ) :: q_prev
   real(ReKi), intent(in   ) :: q_minus2
   real(ReKi), intent(in   ) :: T_alpha
   real(ReKi), intent(in   ) :: T_q
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   
   Get_Cn_pot = Get_Cn_alpha_q_circ(dt, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha0, X_1_prev, X_2_prev, b1, b2, A1, A2) + &
                Get_Cn_Alpha_q_nc( T_alpha, alpha_cur, alpha_prev, alpha_minus2, Kprime_alpha_prev, Kprime_q_prev, T_q, q_cur, q_prev, q_minus2, dt )
   
end function Get_Cn_pot
   
!==============================================================================
! TE Flow Separation Equations                                                !
!==============================================================================

real(ReKi) function Get_f(alpha, alpha1, S1, S2)
   ! Called by : Get_f_prime
   ! Calls  to : NONE
   ! Implements Equation 1.30
   real(ReKi), intent(in   ) :: alpha
   real(ReKi), intent(in   ) :: alpha1
   real(ReKi), intent(in   ) :: S1
   real(ReKi), intent(in   ) :: S2

   real(ReKi)                :: dalpha
   
   dalpha = alpha - alpha1
   
   if (alpha > alpha1) then
      Get_f = 1 - 0.3_ReKi*exp(dalpha/S1)
   else
      Get_f = 0.04_ReKi + 0.66_ReKi*exp(dalpha/S2)
   end if
   
end function Get_f

real(ReKi) function Get_Dp(ds, T_p, Dp_prev, Cn_pot, Cn_pot_prev)
   ! Called by : Get_Cn_prime
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: T_p
   real(ReKi), intent(in   ) :: Dp_prev
   real(ReKi), intent(in   ) :: Cn_pot
   real(ReKi), intent(in   ) :: Cn_pot_prev
   
   ! Implements Equation 1.32b
   Get_Dp = Dp_prev * exp(-ds/T_p) + (Cn_pot - Cn_pot_prev) * exp(-0.5_ReKi*ds*T_p)

end function Get_Dp



real(ReKi) function Get_Cn_prime( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                                 q_prev, q_minus2, T_alpha, T_q, T_p, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev,Dp_prev, Cn_pot_prev, b1, &
                                 b2, A1, A2 )
   ! Called by : Get_f_prime, UnsteadyAero_UpdateStates
   ! Calls  to : Get_Cn_pot, Get_Dp
   ! Implements Equation 1.32a
   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha_minus2
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: q_cur
   real(ReKi), intent(in   ) :: q_prev
   real(ReKi), intent(in   ) :: q_minus2
   real(ReKi), intent(in   ) :: T_alpha
   real(ReKi), intent(in   ) :: T_q
   real(ReKi), intent(in   ) :: T_p
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: Dp_prev
   real(ReKi), intent(in   ) :: Cn_pot_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   
   real(ReKi)                :: Cn_pot
   real(ReKi)                :: Dp
   
   
   ! Implements equation 1.32a
   Cn_pot = Get_Cn_pot( dt, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                        q_prev, q_minus2, T_alpha, T_q, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, b1, b2, A1, A2 ) 
   Dp     = Get_Dp    ( ds, T_p, Dp_prev, Cn_pot, Cn_pot_prev )
   Get_Cn_prime = Cn_pot -  Dp

end function Get_Cn_prime

real(ReKi) function Get_alpha_f( Cn_prime, C_nalpha, alpha0 )
   ! Called by : Get_f_prime
   ! Calls  to : NONE
   ! Implements Equation 1.31
   real(ReKi), intent(in   ) :: Cn_prime
   real(ReKi), intent(in   ) :: C_nalpha
   real(ReKi), intent(in   ) :: alpha0
   
   Get_alpha_f = Cn_prime / C_nalpha + alpha0
   
end function Get_alpha_f

real(ReKi) function Get_f_prime( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, &
                                q_cur, q_prev, q_minus2, T_alpha, T_q, T_p, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, b1, b2, A1, &
                                A2, C_nalpha, alpha1, S1, S2 )
   ! Called by : Get_f_primeprime
   ! Calls  to : Get_Cn_prime, Get_alpha_f, Get_f
   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha_minus2
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: q_cur
   real(ReKi), intent(in   ) :: q_prev
   real(ReKi), intent(in   ) :: q_minus2
   real(ReKi), intent(in   ) :: T_alpha
   real(ReKi), intent(in   ) :: T_q
   real(ReKi), intent(in   ) :: T_p
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: Dp_prev
   real(ReKi), intent(in   ) :: Cn_pot_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   real(ReKi), intent(in   ) :: C_nalpha
   real(ReKi), intent(in   ) :: alpha1
   real(ReKi), intent(in   ) :: S1
   real(ReKi), intent(in   ) :: S2
      
   real(ReKi)                :: alpha_f
   real(ReKi)                :: Cn_prime
   
   Cn_prime = Get_Cn_prime( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                           q_prev, q_minus2, T_alpha, T_q, T_p, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, b1, b2, A1, A2 )
   
   
   alpha_f  = Get_alpha_f( Cn_prime, C_nalpha, alpha0 )

   ! Use equation 1.30 to obtain f_prime, substituting alpha_f for alpha in the call to Get_f()
   Get_f_prime = Get_f(alpha_f, alpha1, S1, S2)

end function Get_f_prime


real(ReKi) function Get_Df(ds, T_f, Df_prev, fprime, fprime_prev)
   ! Called by : Get_f_primeprime
   ! Calls  to : NONE
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: T_f
   real(ReKi), intent(in   ) :: Df_prev
   real(ReKi), intent(in   ) :: fprime
   real(ReKi), intent(in   ) :: fprime_prev
   ! Implements Equation 1.33b

   real(ReKi)                :: factor
   factor = -ds/T_f
   Get_Df = Df_prev * exp(factor) + (fprime - fprime_prev)*exp(0.5_ReKi*factor)
   
end function Get_Df

real(ReKi) function Get_f_primeprime( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                        q_prev, q_minus2, T_alpha, T_q, T_p, T_f, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, Df_prev, fprime_prev, b1, b2, A1, A2, C_nalpha, &
                        alpha1, S1, S2 )
   ! Called by : Get_Cv, UnsteadyAero_UpdateStates
   ! Calls  to : Get_f_prime, Get_Df
   ! Implements Equation 1.33a

   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha_minus2
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: q_cur
   real(ReKi), intent(in   ) :: q_prev
   real(ReKi), intent(in   ) :: q_minus2
   real(ReKi), intent(in   ) :: T_alpha
   real(ReKi), intent(in   ) :: T_q
   real(ReKi), intent(in   ) :: T_p
   real(ReKi), intent(in   ) :: T_f
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: Dp_prev
   real(ReKi), intent(in   ) :: Cn_pot_prev
   real(ReKi), intent(in   ) :: Df_prev
   real(ReKi), intent(in   ) :: fprime_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   real(ReKi), intent(in   ) :: C_nalpha
   real(ReKi), intent(in   ) :: alpha1
   real(ReKi), intent(in   ) :: S1
   real(ReKi), intent(in   ) :: S2
   
   real(ReKi)                :: fprime
   fprime = Get_f_prime(dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                        q_prev, q_minus2, T_alpha, T_q, T_p, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, b1, b2, A1, A2, C_nalpha, &
                        alpha1, S1, S2)
   Get_f_primeprime =  fprime - Get_Df(ds, T_f, Df_prev, fprime, fprime_prev)

end function Get_f_primeprime



!==============================================================================
! Dynamic Stall Equations                                                     !
!==============================================================================

real(ReKi) function Get_Cv( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                             q_prev, q_minus2, T_alpha, T_q, T_p, T_f, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, Df_prev, fprime_prev, b1, b2, A1, A2, C_nalpha, &
                             alpha1, S1, S2, DSmod )
   ! Called by : Get_Cn
   ! Calls  to : Get_f_primeprime

   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha_minus2
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: q_cur
   real(ReKi), intent(in   ) :: q_prev
   real(ReKi), intent(in   ) :: q_minus2
   real(ReKi), intent(in   ) :: T_alpha
   real(ReKi), intent(in   ) :: T_q
   real(ReKi), intent(in   ) :: T_p
   real(ReKi), intent(in   ) :: T_f
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: Dp_prev
   real(ReKi), intent(in   ) :: Cn_pot_prev
   real(ReKi), intent(in   ) :: Df_prev
   real(ReKi), intent(in   ) :: fprime_prev
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   real(ReKi), intent(in   ) :: C_nalpha
   real(ReKi), intent(in   ) :: alpha1
   real(ReKi), intent(in   ) :: S1
   real(ReKi), intent(in   ) :: S2
   integer   , intent(in   ) :: DSMod
   
   !real(ReKi)                :: C_nalpha_circ
   real(ReKi)                :: f_primeprime
   
   
   !C_nalpha_circ = Get_Cn_alpha_q_circ(M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha0, X_1_prev, X_2_prev, b1, b2, A1, A2)
   f_primeprime  = Get_f_primeprime( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                        q_prev, q_minus2, T_alpha, T_q, T_p, T_f, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, Df_prev, fprime_prev, b1, b2, A1, A2, C_nalpha, &
                        alpha1, S1, S2 )
   
   if ( DSMod == 0 ) then
      ! no dynamic stall
      Get_Cv   = C_nalpha_circ
   else if ( DSMod == 1 ) then
      ! use equation 1.47
      Get_Cv   = C_nalpha_circ * ( 0.25*f_primeprime - 0.5*sqrt(f_primeprime) )  ! ( 1 - ( 0.5_ReKi + 0.5_ReKi*sqrt(f_primeprime) ) )**2
   else if ( DSMod == 2 ) then
      ! use equation 1.48
      Get_Cv   = C_nalpha_circ * ( 4.0_ReKi - 8.0_ReKi*sqrt(f_primeprime) + f_primeprime ) / 9.0_ReKi    ! ( 1 - (1 + 2.0_ReKi*sqrt(f_primeprime) )/3.0_ReKi  )**2
   else
      ! implementation error!  Cannot have DSMod other than 0,1,2
   end if
   

end function Get_Cv

real(ReKi) function Get_Cn_v(Cv, Cv_prev, Cn_v_prev, ds, T_V, doAccel)
   ! Called by : Get_Cn
   ! Calls  to : NONE
   ! Implements Equation 1.45 or 1.49
   real(ReKi), intent(in   ) :: Cv
   real(ReKi), intent(in   ) :: Cv_prev
   real(ReKi), intent(in   ) :: Cn_v_prev
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: T_V
   logical   , intent(in   ) :: doAccel
   
   
   real(ReKi)                :: factor
   
   factor = -ds/T_V
   if (doAccel) then
      Get_Cn_v = Cn_v_prev*exp(2.0_ReKi*factor)   ! Eqn 1.49
   else      
      Get_Cn_v = Cn_v_prev*exp(factor) + (Cv - Cv_prev)*exp(0.5_ReKi*factor)  ! Eqn 1.45
   end if
   
end function Get_Cn_v
   
real(ReKi) function Get_Cn( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                             q_prev, q_minus2, T_alpha, T_q, T_p, T_f, T_V, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, Df_prev, fprime_prev, Cv_prev, Cn_v_prev, b1, b2, A1, A2, C_nalpha, &
                             alpha1, S1, S2, DSMod, doAccel )
   ! Called by : NONE
   ! Calls  to : Get_Cv, Get_Cn_Alpha_q_nc, Get_Cn_v

   real(ReKi), intent(in   ) :: dt
   real(ReKi), intent(in   ) :: ds
   real(ReKi), intent(in   ) :: U
   real(ReKi), intent(in   ) :: M
   real(ReKi), intent(in   ) :: c
   real(ReKi), intent(in   ) :: C_nalpha_circ
   real(ReKi), intent(in   ) :: alpha_cur
   real(ReKi), intent(in   ) :: alpha_prev
   real(ReKi), intent(in   ) :: alpha_minus2
   real(ReKi), intent(in   ) :: alpha0
   real(ReKi), intent(in   ) :: q_cur
   real(ReKi), intent(in   ) :: q_prev
   real(ReKi), intent(in   ) :: q_minus2
   real(ReKi), intent(in   ) :: T_alpha
   real(ReKi), intent(in   ) :: T_q
   real(ReKi), intent(in   ) :: T_p
   real(ReKi), intent(in   ) :: T_f
   real(ReKi), intent(in   ) :: T_V
   real(ReKi), intent(in   ) :: X_1_prev
   real(ReKi), intent(in   ) :: X_2_prev
   real(ReKi), intent(in   ) :: Kprime_alpha_prev
   real(ReKi), intent(in   ) :: Kprime_q_prev
   real(ReKi), intent(in   ) :: Dp_prev
   real(ReKi), intent(in   ) :: Cn_pot_prev
   real(ReKi), intent(in   ) :: Df_prev
   real(ReKi), intent(in   ) :: fprime_prev
   real(ReKi), intent(in   ) :: Cv_prev
   real(ReKi), intent(in   ) :: Cn_v_prev
   real(ReKi), intent(in   ) :: T_V
   real(ReKi), intent(in   ) :: b1
   real(ReKi), intent(in   ) :: b2
   real(ReKi), intent(in   ) :: A1
   real(ReKi), intent(in   ) :: A2
   real(ReKi), intent(in   ) :: C_nalpha
   real(ReKi), intent(in   ) :: alpha1
   real(ReKi), intent(in   ) :: S1
   real(ReKi), intent(in   ) :: S2
   integer   , intent(in   ) :: DSMod
   logical   , intent(in   ) :: doAccel
   
   
   real(ReKi)                :: Cv
   real(ReKi)                :: Cn_alpha_q_nc
   
   ! Implements equation 1.50
   
   Cv            = Get_Cv( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
                             q_prev, q_minus2, T_alpha, T_q, T_p, T_f, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, &
                             Cn_pot_prev, Df_prev, fprime_prev, b1, b2, A1, A2, C_nalpha, &
                             alpha1, S1, S2, DSMod  ) + &
   Cn_alpha_q_nc = Get_Cn_Alpha_q_nc( T_alpha, alpha_cur, alpha_prev, alpha_minus2, Kprime_alpha_prev, Kprime_q_prev, T_q, q_cur, q_prev, q_minus2, dt ) + &
   Get_Cn        = Cv + Cn_alpha_q_nc +  Get_Cn_v( Cv, Cv_prev, Cn_v_prev, ds, T_V, doAccel )
   
   
end function Get_Cn
   
subroutine UnsteadyAero_UpdateStates()

   ! Called by : DRIVER
   ! Calls  to : Get_ds, Get_Pitchrate, Get_T_Alpha, Get_T_q, Get_Cn_prime, Get_f_primeprime, Get_k_Alpha

   ! We need to loop over all blade stations and update the states for each station
do i = 1, numNodes
   
   
   real(ReKi)                :: ds
   real(ReKi)                :: Cn_prime
   real(ReKi)                :: M
   real(ReKi)                :: q_cur
   real(ReKi)                :: 
!if (useDynStall) then
   M       = u%U(i) / p%a_s
   ds      = Get_ds( u%U(i), p%c(i), p%dt )
   q_cur   = Get_Pitchrate( u%alpha(i), z%alpha_prev(i), u%U(i), p%c(i) )
   T_alpha = Get_T_Alpha( M, p%a_s, p%c(i), p%C_nalpha(i) )
   T_q     = Get_T_q( M, p%a_s, p%c(i), p%C_nalpha(i) )
   ! Compute Cn_prime using Eqn 1.32a
   Cn_prime = Get_Cn_prime( p%dt, ds, u%U(i), M, p%c(i), p%C_nalpha_circ(i), u%alpha(i), z%alpha_prev(i), z%alpha_minus2(i), p%alpha0(i), q_cur, &
                           z%q_prev(i), z%q_minus2(i), T_alpha, T_q, p%T_p(i), z%X_1_prev(i), z%X_2_prev(i), z%Kprime_alpha_prev(i), &
                           z%Kprime_q_prev(i),z%Dp_prev(i), z%Cn_pot_prev(i), p%b1(i), p%b2(i), p%A1(i), p%A2(i) )
   if ( Cn_prime > Cn1 ) then
      LESF = .TRUE.
   else
      LESF = .FALSE.
   end if
   
   ! Compute f_primeprime using Eqn 1.33a
   f_primeprime = Get_f_primeprime( p%dt, ds, u%U(i), M, p%c(i), p%C_nalpha_circ(i), u%alpha(i), z%alpha_prev(i), z%alpha_minus2(i), p%alpha0(i), q_cur, &
                        z%q_prev(i), z%q_minus2(i), T_alpha, T_q, p%T_p(i), z%T_f(i), z%X_1_prev(i), z%X_2_prev(i), z%Kprime_alpha_prev(i), &
                        z%Kprime_q_prev(i),z%Dp_prev(i), z%Cn_pot_prev(i), z%Df_prev(i), z%fprime_prev(i), p%b1(i), p%b2(i), p%A1(i), p%A2(i), p%C_nalpha(i), &
                        p%alpha1, p%S1, p%S2 )
   
   if (f_primeprime > z%f_primeprime_prev(i)) then
      TESF = .TRUE.
   else
      TESF = .FALSE.
   end if
   
   ! Need to determine how to set VRTX flag!!!
   
   ! Need K_alpha
   k_alpha = Get_k_Alpha( M, a_s, c, C_nalpha ) 
   if (TESF) then
      if (k_alpha < 0.0_ReKi) then 
         sigma1 = 2.0_ReKi
      else if (.not. LESF) then
         sigma1 = 1.0_ReKi
      else if (f_primeprime < 0.7_ReKi) then
         sigma1 = 2.0_ReKi
      else
         sigma1 = 1.75_ReKi
         T_f    = 1.75_ReKi
      end if
   else
      sigma1 = 0.5_ReKi
      if (.not. LESF) then
         sigma1 = 0.5_ReKi
      else if (VRTX .and. ( (tau_V <= T_VL) .and. (tau_V >= 0.0_ReKi) ) ) then
         sigma1 = 0.25_ReKi
      else if (k_alpha > 0.0_ReKi) then
         sigma1 = 0.75
      end if
   end if
   
   if ( (tau_V <= 2.0_ReKi*T_VL) .and. (tau_V >= 0.0_ReKi) ) then
      sigma3 = 3.0_ReKi
   else if (.not. TESF) then
      sigma3 = 4.0_ReKi
   else if ( (tau_V <= T_VL) .and. (tau_V >= 0.0_ReKi) ) then
      if (k_alpha < 0.0_ReKi) then
         sigma3 = 2.0_ReKi
      else
         sigma3 = 1.0_ReKi
      end if
   else if (k_alpha < 0.0_ReKi) then
      if (TESF) then
         sigma3 = 4.0_ReKi
      else
         sigma3 = 1.0_ReKi
      end if
      
   end if
   
   ! Compute quantities based on sigma calculations
   
   ! Chicken and egg problem.  These quantities are needed for the above calculations, but are not determined until sigma1/3 are known
   T_f = T_f0 / sigma1
   T_V = T_V0 / sigma3
   
   ! Update the states
   z%alpha_minus2(i)    = z%alpha_prev(i) 
   z%alpha_prev(i)      = u%alpha(i)
   z%q_minus2(i)        = z%q_prev(i)
   z%q_prev(i)          = q_cur
   z%X_1_prev           =
   z%X_2_prev           =
   z%Kprime_alpha_prev  =
   z%Kprime_q_prev      =
   z%Dp_prev            =
   z%Cn_pot_prev        =
   z%T_f                = T_f
   z%f_primeprime_prev  =
   z%Df_prev            =
   z%fprime_prev        =
   
!end if  ! useDynStall
end do

end subroutine UnsteadyAero_UpdateStates
   
 integer function Test_Cn()
 
   ! Driver constants /  parameters
   real(ReKi)  :: dt                = 1.0_ReKi
   real(ReKi)  :: a_s               = 1000.0_ReKi  ! speed of sound
   
   ! Inputs
   real(ReKi)  :: alpha_cur
   real(ReKi)  :: q_cur
   real(ReKi)  :: M  
   real(ReKi)  :: U
   
   
   ! States
   real(ReKi)  :: alpha_prev
   real(ReKi)  :: alpha_minus2
   real(ReKi)  :: q_prev
   real(ReKi)  :: q_minus2
   real(ReKi)  :: X_1_prev
   real(ReKi)  :: X_2_prev
   real(ReKi)  :: Kprime_alpha_prev
   real(ReKi)  :: Kprime_q_prev
   real(ReKi)  :: Dp_prev
   real(ReKi)  :: Cn_pot_prev
   real(ReKi)  :: Df_prev
   real(ReKi)  :: fprime_prev
   
   
   
   ! Parameters
   real(ReKi)  :: c   
   
   real(ReKi)  :: T_p           ! empirical, specific to a given Airfoil ??  
   real(ReKi)  :: T_f           ! function of M, Re, and specific for a given Airfoil
   real(ReKi)  :: C_nalpha_circ ! function of s, M, and airfoil
   real(ReKi)  :: b1
   real(ReKi)  :: b2
   real(ReKi)  :: A1
   real(ReKi)  :: A2
   real(ReKi)  :: C_nalpha
   real(ReKi)  :: alpha1
   real(ReKi)  :: alpha0
   real(ReKi)  :: S1
   real(ReKi)  :: S2
   integer     :: DSMod
 
   
   
   ! Local derived quantities
   
   real(ReKi)  :: ds
   real(ReKi)  :: T_alpha       
   real(ReKi)  :: T_q           
   real(ReKi)  :: dalpha
   real(ReKi)  :: Cn
   
   real(ReKi)  :: T_alphaVer
   
   ! Set Parameters
   c             = 1.0_ReKi
   T_p           = 1.0_ReKi
   T_f           = 1.0_ReKi
   C_nalpha_circ = 1.0_ReKi
   b1            = 1.0_ReKi
   b2            = 1.0_ReKi
   A1            = 1.0_ReKi
   A2            = 1.0_ReKi
   C_nalpha      = 1.0_ReKi
   alpha1        = 1.0_ReKi
   alpha0        = 1.0_ReKi
   S1            = 1.0_ReKi
   S2            = 1.0_ReKi
   DSMod         = 1
   
   ! Initialize states
   alpha_prev    = 0.0_ReKi
   alpha_minus2  = 0.0_ReKi
   q_prev        = 0.0_ReKi
   q_minus2      = 0.0_ReKi
   X_1_prev      = 0.0_ReKi
   X_2_prev      = 0.0_ReKi
   Kprime_alpha_prev = 0.0_ReKi
   Kprime_q_prev = 0.0_ReKi
   Dp_prev       = 0.0_ReKi
   Cn_pot_prev   = 0.0_ReKi
   Df_prev       = 0.0_ReKi
   fprime_prev   = 0.0_ReKi
   
   
   ! set inputs
   alpha_cur     = 0.1_ReKi
   
   
   U             = 10.0_ReKi
   M             = U / a_s
   q_cur         = Get_Pitchrate( alpha_cur, alpha_prev, U, c )
   dalpha   = alpha_cur - alpha_prev
   ds      = Get_ds(U, c, dt)
   T_alpha = Get_T_Alpha( M, a_s, c, C_nalpha )
   T_alphaVer = 1/ ( (1-M) + .5*M**2*(1-M**2)*.413)* c / a_s
   T_q     = Get_T_q( M, a_s, c, C_nalpha )
   
   Cn = Get_Cn( dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
               q_prev, q_minus2, T_alpha, T_q, T_p, T_f, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, Df_prev, fprime_prev, b1, b2, A1, A2, C_nalpha, &
               alpha1, S1, S2, DSMod )
               !dt, ds, U, M, c, C_nalpha_circ, alpha_cur, alpha_prev, alpha_minus2, alpha0, q_cur, &
               !              q_prev, q_minus2, T_alpha, T_q, T_p, T_f, X_1_prev, X_2_prev, Kprime_alpha_prev, Kprime_q_prev, Dp_prev, Cn_pot_prev, Df_prev, fprime_prev, b1, b2, A1, A2, C_nalpha, &
               !              alpha1, S1, S2, DSMod )
   
   Test_Cn = 1
   
 end function Test_Cn
   
   
   
   
   
end module UnsteadyAero