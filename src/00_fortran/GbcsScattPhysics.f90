!------------------------------------------------------------------------
!
!    Copyright 2008 Chris Merchant, Chris Old, and Owen Embury
!    The Institute of Atmospheric and Environmental Science
!    The University of Edinburgh, UK
!
!    This file is part of the GBCS software package.
!
!    The GBCS software package is free software: you can redistribute it 
!    and/or modify it under the terms of the GNU General Public License 
!    as published by the Free Software Foundation, either version 3 of 
!    the License, or (at your option) any later version.
!
!    The GBCS software package is distributed in the hope that it will be 
!    useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with the GBCS software package.  
!    If not, see <http://www.gnu.org/licenses/>.
!
!------------------------------------------------------------------------



!+ This module contains Scattering Physics functions

MODULE GbcsScattPhysics

!
! Description:
!
! Method:
!
! Owner: Manager of TRICS Project
!
! History:
! Version  Date       Comment
! -------  ----       -------
! 0.0   05/09/2006    Creation                                                   CPO
!
!
! Code Description:
!   Language:            Fortran 90
!
!
! Institute of Atmospheric and Environmental Sciences
! The University of Edinburgh, The Crew Building, The King's Buildings
! West Mains Road, Edinburgh, UK  EH9 3JN
!

! Declarations:

! Modules used:

  USE GbcsConstants
  USE GbcsKinds
  USE GbcsTypes

  IMPLICIT NONE

    CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsScattPhysics.mod'

  REAL(KIND=GbcsReal) , PARAMETER :: Sun_Solid_Angle = 4.0 * Pi * 5.398843E-06
  REAL(KIND=GbcsReal) , PARAMETER :: I_Sun = 230644.0                           ! mW / m^2 / ???

CONTAINS


  FUNCTION Angle_Omega( Sat_ZA , Sol_ZA , Rel_Az , cos_omega_2 )

  !
  ! Description:
  !
  !   Calculate the water reflectivity based on the angle of incidence.
  !
  ! Method:
  !
  !   The value is calculated by interpolating into a look-up-table of values.
  !
  !   The table values are at 10 degree intervals from 0.0 to 90.0 degrees. 
  !
  !   The input angles are assumed to be in radians.
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes
    USE GbcsTimeSpace

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Angle_Omega

    REAL(KIND=GbcsReal) :: Sat_ZA
    REAL(KIND=GbcsReal) :: Sol_ZA
    REAL(KIND=GbcsReal) :: Rel_Az
    REAL(KIND=GbcsReal) :: cos_omega_2

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Angle_Omega'

      cos_omega_2 = 1.0 + SIN( Sol_ZA ) * SIN( Sat_ZA ) * COS( Rel_Az ) + COS( Sol_ZA ) * COS( Sat_ZA )

      Angle_Omega = ACOS( SQRT( cos_omega_2 / 2.0 ) ) 

    RETURN

  END FUNCTION Angle_Omega


  FUNCTION Water_Reflectivity( Omega )

  !
  ! Description:
  !
  !   Calculate the water reflectivity based on the angle omega.
  !
  ! Method:
  !
  !   Based on theory of 
  !
  !   Hale and Querry, 1973
  !
  !   Optical constants of water in the 200nm to 200um wavelength region.
  !   Applied Optics. 1973 March
  !
  !   The value is calculated by interpolating into a look-up-table of values.
  !
  !   The table values are at 1 degree intervals from 0.0 to 90.0 degrees. 
  !   ( Angles stored as radians )
  !
  !   The input omega is assumed to be in radians.
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes
    USE GbcsTimeSpace
    USE GbcsInterpolators, ONLY : Linear_Interp

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Water_Reflectivity

    REAL(KIND=GbcsReal) :: Omega

    REAL(KIND=GbcsReal) , DIMENSION(1) :: Rho

    REAL(KIND=GbcsReal), PARAMETER, DIMENSION(91)  :: Angle = (/ &
      0.00000,  0.0174533,  0.0349066,  0.0523599,  0.0698132,  0.0872665,  0.104720,  0.122173,  0.139626, &
     0.157080,   0.174533,   0.191986,   0.209440,   0.226893,   0.244346,  0.261799,  0.279253,  0.296706, &
     0.314159,   0.331613,   0.349066,   0.366519,   0.383972,   0.401426,  0.418879,  0.436332,  0.453786, &
     0.471239,   0.488692,   0.506145,   0.523599,   0.541052,   0.558505,  0.575959,  0.593412,  0.610865, &
     0.628319,   0.645772,   0.663225,   0.680678,   0.698132,   0.715585,  0.733038,  0.750492,  0.767945, &
     0.785398,   0.802851,   0.820305,   0.837758,   0.855211,   0.872665,  0.890118,  0.907571,  0.925025, &
     0.942478,   0.959931,   0.977384,   0.994838,    1.01229,    1.02974,   1.04720,   1.06465,   1.08210, &
      1.09956,    1.11701,    1.13446,    1.15192,    1.16937,    1.18682,   1.20428,   1.22173,   1.23918, &
      1.25664,    1.27409,    1.29154,    1.30900,    1.32645,    1.34390,   1.36136,   1.37881,   1.39626, &
      1.41372,    1.43117,    1.44862,    1.46608,    1.48353,    1.50098,   1.51844,   1.53589,   1.55334, &
      1.57080 /)

    REAL(KIND=GbcsReal), PARAMETER, DIMENSION(91)  :: Reflectivity = (/ &
    0.0235992,  0.0235992,  0.0235992,  0.0235993,  0.0235995,  0.0235999,  0.0236006,  0.0236019,  0.0236038, &
    0.0236067,  0.0236107,  0.0236161,  0.0236233,  0.0236327,  0.0236446,  0.0236595,  0.0236779,  0.0237004, &
    0.0237276,  0.0237602,  0.0237990,  0.0238448,  0.0238985,  0.0239612,  0.0240340,  0.0241180,  0.0242147, &
    0.0243256,  0.0244523,  0.0245965,  0.0247603,  0.0249459,  0.0251555,  0.0253920,  0.0256580,  0.0259569, &
    0.0262920,  0.0266674,  0.0270871,  0.0275559,  0.0280789,  0.0286618,  0.0293108,  0.0300328,  0.0308355, &
    0.0317273,  0.0327174,  0.0338161,  0.0350348,  0.0363860,  0.0378835,  0.0395428,  0.0413808,  0.0434163, &
    0.0456704,  0.0481660,  0.0509290,  0.0539877,  0.0573738,  0.0611224,  0.0652727,  0.0698678,  0.0749561, &
    0.0805913,  0.0868330,  0.0937478,   0.101410,   0.109902,   0.119315,   0.129753,   0.141331,   0.154176, &
     0.168433,   0.184260,   0.201838,   0.221367,   0.243073,   0.267210,   0.294061,   0.323947,   0.357227, &
     0.394308,   0.435651,   0.481774,   0.533268,   0.590802,   0.655141,   0.727156,   0.807845,   0.898352, &
      1.00000 /)

    REAL(KIND=GbcsReal) , DIMENSION(1) :: X

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Water_Reflectivity'

      X = Omega

      CALL Linear_Interp( Angle , Reflectivity , X , Rho )

      Water_Reflectivity = Rho(1)

    RETURN

  END FUNCTION Water_Reflectivity


  FUNCTION Facet_Normal( Sat_ZA , Sol_ZA , Omega )
  !
  ! Description:
  !
  !  Calculates the of the facet slope normal
  !
  ! Method:
  !
  !   Zeisse, 1995
  !
  !   Radiance of the ocean horizon.
  !   Journal of the Optical Society of America, A. 1995, 12(9), pp 2022-2030
  !
  !   Assumes input zentih angles are in radians.
  !   U125 is the 12.5m wind speed in m/s.
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

  ! Declarations:

  ! Modules used:

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Facet_Normal

    REAL(KIND=GbcsReal) :: Sat_ZA
    REAL(KIND=GbcsReal) :: Sol_ZA
    REAL(KIND=GbcsReal) :: Omega

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Facet_Normal'

      Facet_Normal = ACOS( ( COS( Sol_ZA ) + COS( Sat_ZA ) ) / ( 2.0 * COS( Omega ) ) )

    RETURN

  END FUNCTION Facet_Normal


  FUNCTION Prob_Facet_Slope( Sat_ZA , Sol_ZA , Rel_Az , U125 )

  !
  ! Description:
  !
  !  Calculates the of the facet slope based on 12.5m wind speed
  !
  ! Method:
  !
  !   Cox and Munk, 1954  ( Probability )
  !
  !   Measurement of the roughness of the sea from photographs of the sun's glitter.
  !   Journal of the Optical Society of America. 1954 November, 44(11), pp 838-850
  !
  !   Zeisse, 1995        ( Facet slopes )
  !
  !   Radiance of the ocean horizon.
  !   Journal of the Optical Society of America, A. 1995, 12(9), pp 2022-2030
  !
  !   Zeisse et al., 1999  ( Variances )
  !
  !   Infrared radiance of wind-ruffled sea.
  !   Journal of the Optical Society of America, A. 1999, 16(6), pp 1439-1452
  !
  !
  !   Assumes all input angles are in radians
  !
  !   Sat_ZA     - Satellite Zenith Angle
  !   Sol_ZA     - Solar Zenith Angle
  !   Rel_Az     - Relative Azimith between sun and satellite
  !
  !   U125       - 12.5m wind speed in m/s
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

  ! Declarations:

  ! Modules used:

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Prob_Facet_Slope      

    REAL(KIND=GbcsReal) :: Sat_ZA      
    REAL(KIND=GbcsReal) :: Sol_ZA
    REAL(KIND=GbcsReal) :: Rel_Az
    REAL(KIND=GbcsReal) :: U125

    REAL(KIND=GbcsReal) :: dZdEta_2 , Sigma_2

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Prob_Facet_Slope'

      Sigma_2 = 0.0015 + 0.00256 * U125

      dZdEta_2 = ( SIN(Sol_ZA)**2.0 + SIN(Sat_ZA)**2.0 + 2.0 * SIN(Sol_ZA) * SIN(Sat_ZA) * COS(Rel_Az) ) / &
!                ---------------------------------------------------------------------------------------   
                              ( ( COS(Sol_ZA) + COS(Sat_ZA) ) ** 2.0 )

      Prob_Facet_Slope = EXP( -1.0 * dZdEta_2 / ( 2.0 * Sigma_2 ) ) / ( 2.0 * Pi * Sigma_2 )

    RETURN

  END FUNCTION Prob_Facet_Slope


  FUNCTION Cox_Munk_Correction( Sat_ZA , Sol_ZA , Rel_Az , U125 )
  !
  ! Description:
  !
  !   Calculate sun-glint radiance correction.
  !
  ! Method:
  !
  !   The value is calculated by interpolating into a look-up-table of coefficients 
  !   then calculating the correction using the interpolated coefficients.
  !
  !   The table values are for three visibility ranges. 
  !
  !   The input zenith angles are assumed to be in radians.
  !   The input visiblity is assumed to be in kilometres.
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Cox_Munk_Correction

    REAL(KIND=GbcsReal) :: Sat_ZA
    REAL(KIND=GbcsReal) :: Sol_ZA
    REAL(KIND=GbcsReal) :: Rel_Az
    REAL(KIND=GbcsReal) :: U125

    REAL(KIND=GbcsReal) :: Omega
    REAL(KIND=GbcsReal) :: cos_omega_2
    REAL(KIND=GbcsReal) :: Rho          ! water refectivity
    REAL(KIND=GbcsReal) :: Theta_n      ! facet normal 
    REAL(KIND=GbcsReal) :: P_dZdS       ! P( dZ/dx , dZ/dy)

    REAL(KIND=GbcsReal) , PARAMETER :: Sun_Solid_Ang = 4.0 * Pi * 5.398843E-06  ! Assumed constant
    REAL(KIND=GbcsReal) , PARAMETER :: I_Sun = 230.644                          ! Ignores seasonal effects; small c.f. other errors

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Cox_Munk_Correction'

   
      ! Calculate Omega Angle
      !----------------------- 

      Omega = Angle_Omega( Sat_ZA , Sol_ZA , Rel_Az , cos_omega_2 )

      ! Calculate Water Reflectivity
      !------------------------------

      Rho = Water_Reflectivity( Omega )

      ! Calculate Facet Normal
      !------------------------

      Theta_n = Facet_Normal( Sat_ZA , Sol_ZA , Omega )

      ! Calculate Probability of Facet Slope
      !--------------------------------------

      P_dZdS = Prob_Facet_Slope( Sat_ZA , Sol_ZA , Rel_Az , U125 )


      ! Calculate Sun-Glint Radiance
      !------------------------------

      Cox_Munk_Correction =       ( Rho * P_dZdS * cos_omega_2 ** 2.0 ) / &
!                           ------------------------------------------------------ 
                             ( COS(Sat_ZA) * ( COS(Sol_ZA) + COS(Sat_ZA) )**4.0 )

    RETURN

  END FUNCTION Cox_Munk_Correction


  FUNCTION Atm_Scatt_Rad( Sat_ZA , Sol_ZA , Visibility )

  !
  ! Description:
  !
  !   Calculate atmospheric scattering radiance correction based on visiblity.
  !
  ! Method:
  !
  !   The value is calculated by interpolating into a look-up-table of coefficients 
  !   then calculating the correction using the interpolated coefficients.
  !
  !   The table values are for three visibility ranges. 
  !
  !   The input zenith angles are assumed to be in radians.
  !   The input visiblity is assumed to be in kilometres.
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes
    USE GbcsInterpolators, ONLY : Linear_Interp

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Atm_Scatt_Rad

    REAL(KIND=GbcsReal) :: Sat_ZA
    REAL(KIND=GbcsReal) :: Sol_ZA
    REAL(KIND=GbcsReal) :: Visibility

    REAL(KIND=GbcsReal) , DIMENSION(1) :: A1 , A2 , A3

    REAL(KIND=GbcsReal) , PARAMETER , DIMENSION( 3 ) :: Vis_Range = (/ 10.0 , 23.0 , 50.0 /)
    REAL(KIND=GbcsReal) , PARAMETER , DIMENSION( 3 ) :: Coeff_A1 = (/ 0.12632655 , 0.054745604 , 0.024171335 /)
    REAL(KIND=GbcsReal) , PARAMETER , DIMENSION( 3 ) :: Coeff_A2 = (/ 0.86493534 , 0.88800292 , 0.89530608 /)
    REAL(KIND=GbcsReal) , PARAMETER , DIMENSION( 3 ) :: Coeff_A3 = (/ -0.032430752 , -0.0200061740 , -0.0096661910 /)

    REAL(KIND=GbcsReal) , DIMENSION(1) :: X

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Atm_Scatt_Rad'

      X = Visibility

      CALL Linear_Interp( Vis_Range , Coeff_A1 , X , A1 )
      CALL Linear_Interp( Vis_Range , Coeff_A2 , X , A2 )
      CALL Linear_Interp( Vis_Range , Coeff_A3 , X , A3 )

      Atm_Scatt_Rad = ( A1(1) / COS( Sat_ZA ) ) * ( A2(1) ** ( ( 1.0 / COS( Sat_ZA ) ) + ( 1.0 / COS( Sol_Za ) ) ) ) + A3(1)

    RETURN

  END FUNCTION Atm_Scatt_Rad


  FUNCTION Rad_To_T( Radiance , CWN , P_Offset , P_Slope )

  !
  ! Description:
  !
  !   Convert a Radiance to Brightness Temperature.
  !
  ! Method:
  !
  !   Use the Inverse Planck function to calculate the mono-chromatic BT at
  !   the central wavenumber of the IR channel.
  !   
  !   Apply the Planck weighting correction to allow for the width of the 
  !   IR channel (i.e. chromatic component for finite width channel)
  !
  !
  !   Input parameters :
  !
  !      Radiance     - observed radiance
  !      CWN          - central wavenumber for IR channel
  !      P_Offset     - Planck weighting offset
  !      P_Slope      - Planck weighting slope
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   07/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Rad_To_T

    REAL(KIND=GbcsReal) :: Radiance
    REAL(KIND=GbcsReal) :: CWN
    REAL(KIND=GbcsReal) :: P_Offset
    REAL(KIND=GbcsReal) :: P_Slope

    REAL(KIND=GbcsReal) :: T_cwn

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Rad_To_T'

      T_cwn = ( Planck_C2 * CWN ) / LOG( 1.0 + ( ( Planck_C1 * ( CWN ** 3.0 ) ) / Radiance ) )

      Rad_To_T = ( T_cwn - P_Offset ) / P_Slope

    RETURN

  END FUNCTION Rad_To_T


  FUNCTION T_To_Rad( T , CWN , P_Offset , P_Slope )

  !
  ! Description:
  !
  !   Convert a Brightness Temperature to Radiance.
  !
  ! Method:
  !
  !   Use the Planck function to calculate the BT from the observed
  !   radiance.
  !   
  !
  !   Input parameters :
  !
  !      T            - observed temperature
  !      P_Offset     - Planck weighting offset
  !      P_Slope      - Planck weighting slope
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   07/09/2006    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsKinds
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: T_To_Rad

    REAL(KIND=GbcsReal) :: T
    REAL(KIND=GbcsReal) :: CWN
    REAL(KIND=GbcsReal) :: P_Offset
    REAL(KIND=GbcsReal) :: P_Slope

    REAL(KIND=GbcsReal) :: T_eff

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'T_To_Rad'

      T_eff = P_Slope * T + P_Offset

      T_To_Rad = ( Planck_C1 * ( CWN ** 3.0 ) ) / ( EXP( ( Planck_C2 * CWN ) / T_eff) - 1.0 )

    RETURN

  END FUNCTION T_To_Rad


END MODULE GbcsScattPhysics
