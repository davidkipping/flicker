PROGRAM logrhostar

 ! === AUTHOR ===
 ! David M. Kipping, Harvard-Smithsonian Center for Astrophysics
 ! dkipping@cfa.harvard.edu

 ! === DESCRIPTION ===
 ! logrhostar is a very simple code which calculates the logarithm (base 10)
 ! of the mean stellar density of a star, and associated uncertainty, given
 ! an inputted flicker value and associated uncertainty. The probability
 ! distribution of the derived log-base-10 mean stellar density may be assumed 
 ! to be normally distributed with a mean and standard deviation given by the 
 ! outputs. That is to say, the mean stellar density has a log-normal prior.

 ! === COMPILING & RUNNING ===
 ! gfortran -o logrhostar logrhostar.f90
 ! ./logrhostar

 ! === REFERENCE ===
 ! Use of this code should cite:
 ! Kipping, D. M., Bastien, F. A., Stassun, K. G., Chaplin, W. J., Huber, D.,
 ! Buchhave, L. A., 2014, ApJL, In Press (astro-ph:1403.5264)

 ! === INPUTS ===
 ! f8 = 8-hour flicker of Kepler time series, in parts per million [ppm]. 
 !      To calculate f8, follow the description in Kipping et al. (2014)
 ! f8_error = Standard deviation of the 8-hour flicker, in ppm

 ! === OUTPUTS ===
 ! logrho = Log-base-10 of the mean stellar density, determined using flicker
 ! logrho_error = Error in the log-base-10 of the mean stellar density, as
 !                determined using flicker

 implicit none

 REAL(8) :: f8, f8_error
 LOGICAL :: process
 REAL(8) :: logrho, logrho_error
 REAL(8), PARAMETER :: a = 5.412913272982632D0
 REAL(8), PARAMETER :: b = -1.8500498927543465D0
 REAL(8), PARAMETER :: model_error = 0.13756D0
 REAL(8), PARAMETER :: ln10 = 2.302585092994046D0
 REAL(8), PARAMETER :: logf8min = 1.2D0
 REAL(8), PARAMETER :: logf8max = 2.2D0

 process = .TRUE.
 
 write(*,*) 'Enter flicker value [ppm]'
 read(*,*) f8
 write(*,*) 'Enter flicker error [ppm]'
 read(*,*) f8_error

 logrho = DLOG10(f8)
 IF( logrho .LT. logf8min .OR. logrho .GT. logf8max ) THEN
   write(*,*) 'Inputted flicker exceeds calibration limits; aborting'
   process = .FALSE.
 ELSE IF( f8_error .LT. 0.0D0 ) THEN
   write(*,*) 'Negative flicker error; aborting'
   process = .FALSE.
 END IF

 IF( process ) THEN
   logrho = a + b*logrho 
   logrho_error = DABS( (b*f8_error)/(f8*ln10) )
   logrho_error = DSQRT( logrho_error**2 + model_error**2 )
   write(*,*) 'log_10( rho* [kg/m^3] ) = ',logrho,'+/-',logrho_error
 END IF

END PROGRAM logrhostar
