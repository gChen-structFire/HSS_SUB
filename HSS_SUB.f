      SUBROUTINE UFIELD(FIELD,KFIELD,NSECPT,KSTEP,KINC,TIME,NODE,
     1 COORDS,TEMP,DTEMP,NFIELD)
!
      INCLUDE 'ABA_PARAM.INC'
!
      DIMENSION FIELD(NSECPT,NFIELD), TIME(2), COORDS(3),
     1 TEMP(NSECPT),DTEMP(NSECPT)
	  
!     Store the largest temperature ever reached to FV1
      IF (FIELD(1,1) < (TEMP(1)+DTEMP(1))) THEN !Add dT to get FV at current time
	    FIELD(1,1) = (TEMP(1)+DTEMP(1)) ! NT11
	  END IF
!
      RETURN
      END
	  
!------------------------------------------------------------------------------
      
	  SUBROUTINE vuhard(
! Read only -
     *     nblock, 
     *     jElem, kIntPt, kLayer, kSecPt, 
     *     lAnneal, stepTime, totalTime, dt, cmname,
     *     nstatev, nfieldv, nprops, 
     *     props, tempOld, tempNew, fieldOld, fieldNew,
     *     stateOld,
     *     eqps, eqpsRate,
! Write only -
     *     yield, dyieldDtemp, dyieldDeqps,
     *     stateNew )
!
      INCLUDE 'vaba_param_dp.inc'
!
      DIMENSION props(nprops), tempOld(nblock), tempNew(nblock),
     1   fieldOld(nblock,nfieldv), fieldNew(nblock,nfieldv),
     2   stateOld(nblock,nstatev), eqps(nblock), eqpsRate(nblock),
     3   yield(nblock), dyieldDtemp(nblock), dyieldDeqps(nblock,2),
     4   stateNew(nblock,nstatev), jElem(nblock)
!
!     Init lookup table variables
	  REAL*8, DIMENSION(15) :: lutkb, lutKB_TEMP, LUTE, LUTE_TEMP
	  REAL*8, DIMENSION(6) :: LUTKP, LUTKP_TEMP
	  REAL*8, DIMENSION(2) :: YIELD_OUTPUT
      REAL*8, DIMENSION(2) :: LUTEP_U, LUTEP_U_TEMP, EP_Y_INTERP_TEMP, EP_Y_INTERP_STRAIN, F_Y_INTERP_TEMP
	  REAL*8 :: EP_Y, EP_T
      REAL*8 :: F_T_LIM
      REAL*8 :: kelvinOffset
      LOGICAL :: KELVIN
      CHARACTER*80 cmname
      INTEGER :: POST_YIELD_MODEL

      !Set to 0 to allow the stress-curve to reduce to zero after strain = 0.1.
	  !Set to 1 to set a constant stress beyond strain = 0.1 such that stress = MIN(500 MPa, f_y)
	  !Set to 2 to set a constant stress beyond yield such that stress = f_y
      POST_YIELD_MODEL = 0
      
      !Set this to .TRUE. if your temperature data is in Kelvin. Set to .FALSE. if it is in degress Celsius.
      KELVIN = .TRUE.
      kelvinOffset = 273.15
      
      !Strain limits, Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model, F. Hanus, G. Zilli, J.-M. Franssen (2011) [-]
	  EP_Y  = 0.02
	  EP_T  = 0.1
      !Maximum stress at strain = EP_T [Pa]
      F_T_LIM = 500000000.

!     Lookup table values
      !Bolt reduction factor
	  lutkb =          (/    1.0,  0.968,  0.952,  0.935,  0.903,  0.775,   0.55,   0.22,   0.16,    0.1,   0.067,   0.033,     0.0,     0.0,     0.0 /) !EC3-1-8
	  lutKB_TEMP =     (/ 293.15, 373.15, 423.15, 473.15, 573.15, 673.15, 773.15, 873.15, 923.15, 973.15, 1073.15, 1173.15, 1273.15, 1373.15, 1473.15 /) !EC3-1-8 [K]
      !elastic modulus
	  LUTE  =          (/    1.0,    1.0,  0.950,  0.900,  0.800,  0.700,  0.600,  0.310,  0.220,  0.130,   0.090,  0.0675,  0.0450,  0.0225,     0.0 /) !EC3-1-8
	  LUTE_TEMP =      (/ 293.15, 373.15, 423.15, 473.15, 573.15, 673.15, 773.15, 873.15, 923.15, 973.15, 1073.15, 1173.15, 1273.15, 1373.15, 1473.15 /) !EC3-1-8 [K]
      !proportional limit
      LUTKP =          (/    0.9,    0.8,   0.75,   0.75,     0.6,     0.6 /) ! Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model, F. Hanus, G. Zilli, J.-M. Franssen (2011)
      LUTKP_TEMP =     (/ 293.15, 473.15, 673.15, 873.15, 1073.15, 1173.15 /) ! Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model, F. Hanus, G. Zilli, J.-M. Franssen (2011) [K]
      !fracture train
      LUTEP_U    =     (/   0.15,    0.25 /)
      LUTEP_U_TEMP=    (/ 873.15, 1073.15 /) ![K]
      !ultimate strain yield-point interpolation
	  EP_Y_INTERP_STRAIN = (/ EP_Y, EP_T /)
      EP_Y_INTERP_TEMP   = (/ 873.15, 1073.15 /)
	  F_Y_INTERP_TEMP = EP_Y_INTERP_TEMP
      
      
      
!     Adjust to Celsius Kelvin
      IF (.NOT.KELVIN) THEN
        lutKB_TEMP = lutKB_TEMP - kelvinOffset
        LUTE_TEMP = LUTE_TEMP - kelvinOffset
        LUTKP_TEMP = LUTKP_TEMP - kelvinOffset
        LUTEP_U_TEMP = LUTEP_U_TEMP - kelvinOffset
        EP_Y_INTERP_TEMP = EP_Y_INTERP_TEMP - kelvinOffset
        F_Y_INTERP_TEMP = F_Y_INTERP_TEMP - kelvinOffset
      END IF

!     Store outputs for ABAQUS
      DO 100 km = 1,nblock
	    YIELD_OUTPUT = YIELD_MODEL(eqps(km), tempOld(km), fieldOld(km,1))
	    yield(km) = YIELD_OUTPUT(1)
		dyieldDeqps(km,1) = YIELD_OUTPUT(2)
  100 CONTINUE
      dyieldDeqps(:,2) = 0.0 !Data based on constant strain rate
	  
      RETURN
      
	  CONTAINS
	  
	  FUNCTION LINTERP(x, x1, x2, y1, y2) RESULT(y)
	   !Linearly interpolates between two data points given the input value x
	   
	   REAL*8, INTENT(IN) :: x, y1, y2, x1, x2
	   REAL*8 :: y
	   
	   y = (x-x1)/(x2-x1)*(y2-y1) + y1
	   RETURN
	  END FUNCTION
	  
	  FUNCTION lookup(x, xarray, yarray, arraysize) RESULT(y)
!		Takes a value and linearly interpolates it to a look up table.
!       x values outside of the lookup table will be set to the end values of yarray
        IMPLICIT NONE
        REAL*8 :: y
		REAL*8, INTENT (IN) :: x
		REAL*8, DIMENSION(*), INTENT(IN) :: xarray, yarray
		INTEGER, INTENT (IN) :: arraysize
		INTEGER :: i
		
!		Find index of lower bound of LUT interval
!       Early termination: Clamp values if x is out of bounds
		IF (x .LT. xarray(1)) THEN ! x smaller than all values
			y = yarray(1)
			RETURN
		END IF
		IF (x .GT. xarray(arraysize)) THEN ! x larger than all values
			y = yarray(arraysize)
			RETURN
		END IF
		
		i = COUNT((xarray(1:arraysize)-x)<0)
!		Linterp formuia
		y = LINTERP(x,xarray(i),xarray(i+1),yarray(i), yarray(i+1))
		RETURN
	  END FUNCTION
	  
	  FUNCTION knr(Ts, Tu) RESULT(rknr)
!      Takes the current and maximum temperature of an integration point and returns the post-peak cooling factor
!      according to Hanus, F., G. Zilli, and J.M. Franssen, Behaviour of Grade 8.8 bolts under natural fire conditions—Tests and model.
!      Journal of constructional steel research, 2011. 67(8): p. 1292-1298.
		IMPLICIT NONE
		REAL*8 :: rknr
		REAL*8, INTENT (IN)  :: Ts, Tu
		REAL*8 			   :: Tuclamp, Tumax, Tumin
        
        Tumax = 1073.15
        Tumin = 773.15
        
        IF (.NOT.KELVIN) THEN
          Tumax = Tumax - kelvinOffset
          Tumin = Tumin - kelvinOffset
        END IF
		rknr = 1.0
		IF (Tu <= Tumin) RETURN ! knr = 1 when Maximum temperature>=500 C
		Tuclamp = MIN(Tumax, Tu) !data only for up to 800 °C
		rknr = 1.0 - 0.00133*(Tuclamp - MAX(Ts, Tumin))
		RETURN

	  END FUNCTION
	  
	  FUNCTION YIELD_MODEL(eqps,temp,temp_ult) RESULT(YIELD_OUTPUT)
!		Takes the current plastic strain 'eqps', the current temperature and the
!       maximum temperature reached to determine the yield strength of the
!       material at this time. Note that the input strain and output stress is
!       in terms of true stress/strain, but the yield surface is determined in engineering stress/strain
		REAL*8 :: EP_P, EP_U, E_a, F_P, F_Y, yield, a2, b2, c, ba, dyield, dyieldT, ep, EP_Y_TEMP
		REAL*8, INTENT(IN) :: eqps, temp, temp_ult
		REAL*8, DIMENSION(2) :: YIELD_OUTPUT, F_Y_INTERP_STRESS
		
		yield = 0 !value of stress-strain curve
		dyield = 0 !gradient of stress-strain curve

		F_Y = props(1)*lookup(temp,lutKB_TEMP,lutkb,SIZE(lutKB_TEMP))*knr(temp,temp_ult) !eng stress
		!Set up interpolation of the ultimate stress point
		EP_Y_TEMP = lookup(temp_ult, EP_Y_INTERP_TEMP, EP_Y_INTERP_STRAIN, SIZE(EP_Y_INTERP_TEMP))
		IF (F_Y > F_T_LIM) THEN ! and then interpolate it
	      F_Y_INTERP_STRESS = (/ F_Y, F_T_LIM /)
		  F_Y = lookup(temp_ult, F_Y_INTERP_TEMP, F_Y_INTERP_STRESS, SIZE(F_Y_INTERP_TEMP));
	    END IF
        F_P = lookup(temp_ult,LUTKP_TEMP,LUTKP,SIZE(LUTKP_TEMP))*F_Y !engineering stress
        E_a = props(2)*lookup(temp, LUTE_TEMP,LUTE,SIZE(lutKB_TEMP))
		EP_P = F_P/E_a !proportional limit strain, eng
		ep = (exp(log(EP_P+1)+eqps)-1) !total strain, eng
		

		IF ((ep) >= (EP_T)) THEN
          !post-limiting strain region
		  IF (POST_YIELD_MODEL .EQ. 0) THEN
			EP_U = lookup(MAX(temp_ult,temp), LUTEP_U_TEMP, LUTEP_U, SIZE(LUTEP_U))
            dyield = (0-MIN(F_T_LIM,F_Y))/(EP_U-EP_T)
            yield = MAX(LINTERP(ep, EP_T,EP_U,MIN(F_T_LIM,F_Y),1._8),1._8)
          ELSE
			IF (POST_YIELD_MODEL .EQ. 2) THEN
			  yield = F_Y
		    ELSE
			  yield = MIN(F_T_LIM,F_Y)
			END IF
		  END IF
        ELSE
          !post-ultimate region
		  IF ((ep).GE.EP_Y_TEMP .AND. POST_YIELD_MODEL.NE.2 .AND. F_Y > F_T_LIM) THEN
			yield = LINTERP((ep), EP_Y_TEMP, EP_T, F_Y, F_T_LIM)
            dyield = (F_Y - F_T_LIM)/(EP_T-EP_Y_TEMP)
          ELSE
            yield = F_Y
          END IF
          
          !non-propotional region
		  IF ((ep) < EP_Y_TEMP) THEN
            ep = MAX(ep, EP_P)
			c  = (F_Y-F_P)**(2)/((EP_Y_TEMP-EP_P)*E_a - 2*(F_Y-F_P))
			b2 = c*(EP_Y_TEMP-EP_P)*E_a+c**2
			a2 = (EP_Y_TEMP-EP_P)*(EP_Y_TEMP-EP_P+c/E_a)
			ba = (b2**(1./2.)/a2**(1./2.))
			yield = F_P - c + ba*(a2-(EP_Y_TEMP-(ep))**2)**(1./2.)
			dyield = ba*(EP_Y_TEMP-(ep))/((a2-(EP_Y_TEMP-ep)**2)**(1./2.))
		  END IF
        END IF
		
 		yield = yield*(1+ep)!convert back to true stress
	    YIELD_OUTPUT = (/ yield, dyield /) 
		
	  END FUNCTION
	  
	  END
