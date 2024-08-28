!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsAtmPhysics
!
! PURPOSE:
!       This module contains Atmospheric Physics functions
!       
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsAtmPhysics
!
! PUBLIC DATA:
!
! MODULES:
!       GbcsKinds
!       GbcsConstants
!       GbcsErrorHandler
!       
! CONTAINS:
!       TCWV
!       SH_TO_PPMV
!       PPMV_TO_SH
!       SH_TO_LNQ
!       LNQ_TO_SH
!       PPMV_TO_LNQ
!       PP_TO_MR
!       ATM_SVP
!       RH_TO_MR
!       MolWeight_Air
!       SVP_Water
!       PPMV_to_RH
!       RH_to_PPMV
!       MR_to_PPMV
!       
! DERIVED TYPES:
!
! CREATION HISTORY:
!       Written by:   Chris Old      25/04/2005
!                     IAES, University of Edinburgh
!       Updated headers, added new elemental functions:
!                     Owen Embury    09/11/2008
!       Added MR_to_PPMV for other elements 
!                     Jonathan Mittaz 10/07/2014
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

MODULE GbcsAtmPhysics
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE

  INTERFACE PPMV_to_RH
    MODULE PROCEDURE PPMV_to_RH_Real, PPMV_to_RH_Dble
  END INTERFACE PPMV_to_RH

  INTERFACE RH_to_PPMV
    MODULE PROCEDURE RH_to_PPMV_Real, RH_to_PPMV_Dble
  END INTERFACE RH_to_PPMV
  
  INTERFACE SVP_Water
    MODULE PROCEDURE SVP_Water_Real, SVP_Water_Dble
  END INTERFACE SVP_Water

  INTERFACE MR_to_PPMV
    MODULE PROCEDURE MR_to_PPMV_Real, MR_to_PPMV_Dble
  END INTERFACE MR_to_PPMV
  
  ! -----------------
  ! Public data
  ! -----------------

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Gas Constants used in calculations
  REAL(KIND=GbcsDble), PARAMETER :: Dry_Air_Mol_Wgt = 28.9648   ! Md    Dry Air
  REAL(KIND=GbcsDble), PARAMETER :: H2O_Mol_Wgt     = 18.01528  ! Mw    Water
  REAL(KIND=GbcsDble), PARAMETER :: CO2_Mol_Wgt     = 44.00950  ! Mco2  Carbon Dioxide
  REAL(KIND=GbcsDble), PARAMETER :: O3_Mol_Wgt      = 47.99820  ! Mo3   Ozone
  REAL(KIND=GbcsDble), PARAMETER :: N2O_Mol_Wgt     = 44.01288  ! Mn2o  Nitrous Oxide
  REAL(KIND=GbcsDble), PARAMETER :: CO_Mol_Wgt      = 28.01010  ! Mco   Carbon Monoxide
  REAL(KIND=GbcsDble), PARAMETER :: CH4_Mol_Wgt     = 16.04246  ! Mch4  Methane
  REAL(KIND=GbcsDble), PARAMETER :: O2_Mol_Wgt      = 31.99880  ! Mo2   Oxygen
  REAL(KIND=GbcsDble), PARAMETER :: Molar_Gas_Const = 8.314472  ! R

  REAL(KIND=GbcsDble), PARAMETER :: RH_PPMV_CONV    = 1.60771704E+06     ! Taken from Met Office SPS code

  INTEGER, PARAMETER :: n_molecular_weight = 7
  REAL(KIND=GbcsDble), PARAMETER :: Molecular_Weight(n_molecular_weight) = &
       (/H2O_Mol_Wgt,CO2_Mol_Wgt,O3_Mol_Wgt,N2O_Mol_Wgt,CO_Mol_Wgt,&
       CH4_Mol_Wgt,O2_Mol_Wgt/)

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Physics/GbcsAtmPhysics.f90'

CONTAINS


  ! Calculate Total Column Water Vapour from Specific Humidity and Pressure

  FUNCTION TCWV(SH,P,Ps,npts)

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
  ! 0.0   25/04/2005    Creation                                                   CPO
  ! 0.1   10/10/2005    Corrected TCWV calculation                                 CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants
    USE GbcsErrorHandler, ONLY: Gbcs_Log_File_Unit

    IMPLICIT NONE

    REAL(KIND=GbcsReal)               :: TCWV      ! Total Column Water Vapour returned

    REAL(KIND=GbcsReal), DIMENSION(:) :: SH        ! Specific Humidity  (kg/kg)
    REAL(KIND=GbcsReal), DIMENSION(:) :: P         ! Pressure profile   (hPa)
    REAL(KIND=GbcsReal)               :: Ps        ! Surfce pressure    (hPa)
    INTEGER                           :: npts      ! Number of points in profile

    INTEGER :: ii
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE :: PPMV
    REAL(KIND=GbcsReal) :: WV
    REAL(KIND=GbcsReal) :: Pa
    REAL(KIND=GbcsReal) :: EPS
    REAL(KIND=GbcsReal) :: TOTAL
    INTEGER :: STATUS
    INTEGER :: Level_1

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'TCWV'

      !  Assumes profiles from TOA downwards - i.e.increasing P

      EPS = H2O_Mol_Wgt / Dry_Air_Mol_Wgt

      ALLOCATE( PPMV( SIZE(SH) ) , STAT=STATUS )
      IF (STATUS /= 0) THEN
        WRITE(Gbcs_Log_File_Unit,*)'ERROR : Unable to allocate space for ppmv'
        WRITE(Gbcs_Log_File_Unit,*)'        Halting process'
        STOP
      END IF

      CALL SH_TO_PPMV(SH,PPMV)

      DO ii = 1 , npts
        IF ( P(ii) < Ps ) Level_1 = ii
      END DO

      Pa = ( P(Level_1) + Ps ) / 2.0
      WV = PPMV(Level_1)

!      WRITE(Gbcs_Log_File_Unit,*) WV,Pa,EPS,Ps,P(1),Ps/P(1)  for debuging

      TOTAL = WV * Pa * EPS * LOG( P(Level_1) / Ps ) / ( Standard_Gravity * 10000.0 )

      DO ii = 1,Level_1-1                                         ! [sum over remaining N-1 layers]

        WV = ( PPMV(ii) + PPMV(ii+1) ) / 2.0                      ! [use average specific humidity between levels]

        Pa = ( P(ii) + P(ii+1) ) / 2.0                            ! [use average Pressure between levels]

        TOTAL = TOTAL + WV * Pa * EPS * LOG( P(ii+1) / P(ii) ) / ( Standard_Gravity * 10000.0 )

      END DO

      TCWV = TOTAL

      DEALLOCATE( PPMV )

    RETURN

  END FUNCTION TCWV


  !  Convert Specific Humidity to ppmv (relative to wet air - as required by RTTOV)

  SUBROUTINE SH_TO_PPMV(SH,PPMV)

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
  ! 0.0   05/05/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants

    IMPLICIT NONE

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: SH
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(  OUT) :: PPMV

    INTEGER :: NPTS
    INTEGER :: Level

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'SH_TO_PPMV'


      NPTS = SIZE(SH)

      DO Level = 1,NPTS

        IF ( SH(Level) /= NAN_R ) THEN
          PPMV(Level) = SH(Level) * RH_PPMV_CONV
        ELSE
          PPMV(Level) = SH(Level)
        END IF

      END DO

    RETURN

  END SUBROUTINE SH_TO_PPMV


  SUBROUTINE PPMV_TO_SH(PPMV,SH)

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
  ! 0.0   05/05/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants

    IMPLICIT NONE

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: PPMV
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(  OUT) :: SH

    INTEGER :: NPTS
    INTEGER :: Level

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'PPMV_TO_SH'


      NPTS = SIZE(PPMV)

      DO Level = 1,NPTS

        IF ( PPMV(Level) /= NAN_R ) THEN
          SH(Level) = PPMV(Level) / RH_PPMV_CONV
        ELSE
          SH(Level) = PPMV(Level)
        END IF

      END DO

    RETURN

  END SUBROUTINE PPMV_TO_SH


  SUBROUTINE SH_TO_LNQ(SH,LNQ)

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
  ! 0.0   05/05/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants

    IMPLICIT NONE

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: SH
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(  OUT) :: LNQ

    INTEGER :: NPTS
    INTEGER :: Level

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'SH_TO_LNQ'

      NPTS = SIZE(SH)

      DO Level = 1,NPTS

        IF ( SH(Level) > 0.0 ) THEN
          LNQ(Level) = LOG( SH(Level) * 1000.0 )
        ELSE IF ( SH(Level) < 0.0 ) THEN
          LNQ(Level) = -1.0 * LOG( ABS( SH(Level) ) * 1000.0 )
        ELSE
          LNQ(Level) = NAN_R !0.0
        END IF

      END DO

    RETURN

  END SUBROUTINE SH_TO_LNQ


  SUBROUTINE LNQ_TO_SH(SH,LNQ)

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
  ! 0.0   05/05/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants

    IMPLICIT NONE

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: LNQ
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(  OUT) :: SH

    INTEGER :: NPTS
    INTEGER :: Level

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'LNQ_TO_SH'

      NPTS = SIZE(LNQ)

      DO Level = 1,NPTS

        IF ( LNQ(Level) > 0.0 ) THEN

          SH(Level) = EXP( LNQ(Level) ) / 1000.0

        ELSE

          SH(Level) = -1.0 * EXP( ABS( LNQ(Level) ) ) / 1000.0

        END IF

      END DO

    RETURN

  END SUBROUTINE LNQ_TO_SH


  SUBROUTINE PPMV_TO_LNQ(PPMV,LNQ)

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
  ! 0.0   05/05/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsConstants

    IMPLICIT NONE

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: PPMV
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(  OUT) :: LNQ

    INTEGER :: NPTS
    INTEGER :: Level
    REAL(KIND=GbcsReal) :: PP

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'PPMV_TO_LNQ'


      NPTS = SIZE(PPMV)

      DO Level = 1,NPTS

        IF ( PPMV(Level) > NAN_R ) THEN

          PP = PPMV(Level) / RH_PPMV_CONV

          IF ( PP > 0.0 ) THEN

            LNQ(Level) = LOG( PP * 1000.0 )

          ELSE IF ( PP < 0.0 ) THEN

            LNQ(Level) = -1.0 * LOG( ABS( PP ) * 1000.0 )

          ELSE

            LNQ(Level) = NAN_R !0.0

          END IF

        ELSE

          LNQ(Level) = NAN_R

        END IF

      END DO

    RETURN

  END SUBROUTINE PPMV_TO_LNQ


  SUBROUTINE PP_TO_MR( PP , PRESSURE , MR )

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
  ! 0.0   29/11/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(IN   ) :: PP
    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(IN   ) :: PRESSURE
    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(  OUT) :: MR

    REAL(KIND=GbcsDble) :: EPS

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'PP_TO_MR'

      ! Currently only do water vapour 
      ! See /home/oembury/IDL/Procedures/AtmProfile/convert_pp_to_mr.pro for other absorbers

      EPS = H2O_Mol_Wgt / Dry_Air_Mol_Wgt

      MR = EPS * PP / ( PRESSURE - PP )

    RETURN

  END SUBROUTINE PP_TO_MR


  SUBROUTINE ATM_SVP( T , SVP )

  !
  ! Description:
  !
  ! Method:
  !
  !   Flatau   - Polynomial fit to Wexler. Valid -85C to +70C. Used by OPTRAN
  !              profile conversion utility. Recommended for speed.
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   29/11/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(IN   ) :: T
    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(  OUT) :: SVP

    REAL(KIND=GbcsDble), PARAMETER, DIMENSION(9) :: coeff = &
       (/ 6.11583699_GbcsDble, 4.44606896E-01_GbcsDble, 1.43177157E-02_GbcsDble, &
          2.64224321E-04_GbcsDble, 2.99291081E-06_GbcsDble, 2.03154182E-08_GbcsDble, &
          7.02620698E-11_GbcsDble, 3.79534310E-14_GbcsDble,-3.21582393E-16_GbcsDble /)

    REAL(KIND=GbcsReal), PARAMETER, DIMENSION(2) :: valid_range = (/ 188.15 , 343.15 /)

    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: Temp

    INTEGER :: n_coeff
    INTEGER :: n_elems
    INTEGER :: indx

    LOGICAL :: out_of_range

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'ATM_SVP'

      n_elems = SIZE(T)
      n_coeff = 9

      ALLOCATE( Temp(n_elems) )

      Temp = T

      out_of_range = .FALSE.

      DO indx = 1,n_elems
        IF ( ( Temp(indx) < valid_range(1) ) .OR. ( Temp(indx) > valid_range(2) ) ) out_of_range = .TRUE.
        IF ( Temp(indx) < valid_range(1) ) Temp(indx) = valid_range(1)
        IF ( Temp(indx) > valid_range(2) ) Temp(indx) = valid_range(2)
     END DO

!      IF ( out_of_range ) THEN
!        WRITE(Gbcs_Log_File_Unit,*)'  WARNING : Temperature values out of range'
!        WRITE(Gbcs_Log_File_Unit,*)'            Subroutine ATM_SVP'
!        WRITE(Gbcs_Log_File_Unit,*)'            Module GbcsAtmPhysics'
!        WRITE(Gbcs_Log_File_Unit,*)'            /GbcsMod_AtmPhysics/GbcsAtmPhysics.f90'
!      END IF

      SVP = coeff(n_coeff)

      Temp = Temp - REAL(273.15,KIND=GbcsDble)

      DO indx = n_coeff-1,1,-1

        SVP = ( SVP * Temp ) + coeff( indx )

      END DO

      DEALLOCATE( Temp )

    RETURN

  END SUBROUTINE ATM_SVP


  SUBROUTINE RH_TO_MR( RH , T , P , MR )

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
  ! 0.0   29/11/2005    Creation                                                   CPO
  !
  !
  ! Code Description:
  !   Language:            Fortran 90
  !
  !
  ! Institute of Atmospheric and Environmental Sciences
  ! The University of Edinburgh, The Crew Building, The Kings Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: RH
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: T
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: P
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(  OUT) :: MR

    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: SVP
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: SVP_MR

    INTEGER :: NElems

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'RH_TO_MR'

      NElems = SIZE( RH )

      ALLOCATE( SVP( NElems ) )
      ALLOCATE( SVP_MR( NElems ) )

      CALL ATM_SVP( REAL(T,KIND=GbcsDble) , SVP )
      CALL PP_TO_MR( SVP , REAL(P,KIND=GbcsDble) , SVP_MR )
      MR = REAL(RH,KIND=GbcsReal) * SVP_MR / 100.0

      DEALLOCATE( SVP )
      DEALLOCATE( SVP_MR )

    RETURN

  END SUBROUTINE RH_TO_MR

  SUBROUTINE MR_TO_PPMV_Real( MR , PPMV, Molecular_ID )

    USE GbcsErrorHandler

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: MR
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(OUT  ) :: PPMV
    INTEGER, OPTIONAL :: Molecular_ID

    INTEGER :: MolID
    REAL(KIND=GbcsReal), PARAMETER :: PPV_to_PPMV = 1.0e+06_GbcsReal
    REAL(KIND=GbcsReal), PARAMETER :: SCALE_FACTOR = PPV_TO_PPMV

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'MR_TO_PPMV'

    IF( PRESENT(Molecular_ID) )THEN
       IF( 1 .gt. Molecular_ID .or. n_molecular_weight .lt. Molecular_ID )THEN
          CALL Gbcs_Critical(.TRUE.,'Molecular_ID out of range',&
               'MR_to_PPMV','GbcsAtmPhysics.f90')
       ENDIF
       MolID = Molecular_ID
    ELSE
       MolID = 1
    ENDIF

    PPMV = SCALE_FACTOR * MR * Dry_Air_Mol_Wgt / Molecular_Weight(MolID)

    RETURN

  END SUBROUTINE MR_TO_PPMV_REAL

  SUBROUTINE MR_TO_PPMV_DBLE( MR , PPMV, Molecular_ID )

    USE GbcsErrorHandler

    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(IN   ) :: MR
    REAL(KIND=GbcsDble), DIMENSION(:), INTENT(OUT  ) :: PPMV
    INTEGER, OPTIONAL :: Molecular_ID

    INTEGER :: MolID
    REAL(KIND=GbcsDble), PARAMETER :: PPV_to_PPMV = 1.0e+06_GbcsDble
    REAL(KIND=GbcsDble), PARAMETER :: SCALE_FACTOR = PPV_TO_PPMV

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'MR_TO_PPMV'

    IF( PRESENT(Molecular_ID) )THEN
       IF( 1 .gt. Molecular_ID .or. n_molecular_weight .lt. Molecular_ID )THEN
          CALL Gbcs_Critical(.TRUE.,'Molecular_ID out of range',&
               'MR_to_PPMV','GbcsAtmPhysics.f90')
       ENDIF
       MolID = Molecular_ID
    ELSE
       MolID = 1
    ENDIF

    PPMV = SCALE_FACTOR * MR * Dry_Air_Mol_Wgt / Molecular_Weight(MolID)

    RETURN

  END SUBROUTINE MR_TO_PPMV_DBLE


!------------------------------------------------------------------------------
!F+
! NAME:
!       MolWeight_Air
!
! PURPOSE:
!       Calculate the molecular weight of air
!
! CATEGORY:
!       
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       mw = MolWeight_Air( watervapour )
!
! INPUT ARGUMENTS:
!       watervapour - watervapour in ppmv
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!       mw
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     08/11/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION MolWeight_Air( watervapour ) &
                            RESULT( mw )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT(IN) :: watervapour

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble)             :: mw

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),      PARAMETER :: ROUTINE_NAME = 'MolWeight_Air'

    ! ---------------
    ! Local variables
    ! ---------------

    mw = (1.0 - 1e-6 * watervapour) * Dry_Air_Mol_Wgt + &
               (1e-6 * watervapour) * H2O_Mol_Wgt

  END FUNCTION MolWeight_Air


!------------------------------------------------------------------------------
!F+
! NAME:
!       SVP_Water
!
! PURPOSE:
!       Calculate saturation vapour pressure
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       svp = SVP_Water( temperature )
!
! INPUT ARGUMENTS:
!       temperature (Kelvin)
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       saturation vapour pressure (hPa)
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!
! PROCEDURE:
!       Flatau   - Polynomial fit to Wexler. Valid -85C to +70C. Used by OPTRAN
!                  profile conversion utility. Recommended for speed.
!
! CREATION HISTORY:
!       0.0   29/11/2005    Creation                                       CPO
!       0.1   05/11/2008    Converted to function
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION SVP_Water_Dble( temperature ) &
                        RESULT( svp )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT(IN) :: temperature

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble)             :: svp

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'SVP_Water_Dble'

    INTEGER, PARAMETER         :: n_coeff = 9
    REAL(KIND=GbcsDble), PARAMETER, DIMENSION(n_coeff) :: coeff = &
       (/ 6.11583699_GbcsDble,     4.44606896E-01_GbcsDble, 1.43177157E-02_GbcsDble, &
          2.64224321E-04_GbcsDble, 2.99291081E-06_GbcsDble, 2.03154182E-08_GbcsDble, &
          7.02620698E-11_GbcsDble, 3.79534310E-14_GbcsDble,-3.21582393E-16_GbcsDble /)
    REAL(KIND=GbcsDble), PARAMETER, DIMENSION(2) :: valid_range = (/ 188.15 , 343.15 /)

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble) :: temp
    INTEGER             :: i

    temp = temperature
    
    IF (temp < valid_range(1)) temp = valid_range(1)
    IF (temp > valid_range(2)) temp = valid_range(2)
    
    temp = temp - 273.15_GbcsDble

    svp = coeff(n_coeff)

    DO i = n_coeff-1,1,-1
      svp = (svp * temp) + coeff(i)
    END DO

  END FUNCTION SVP_Water_Dble

  ELEMENTAL FUNCTION SVP_Water_Real( temperature ) &
                        RESULT( svp )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), INTENT(IN) :: temperature

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal)             :: svp

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'SVP_Water_Real'

    INTEGER, PARAMETER         :: n_coeff = 9
    REAL(KIND=GbcsReal), PARAMETER, DIMENSION(n_coeff) :: coeff = &
       (/ 6.11583699_GbcsDble,     4.44606896E-01_GbcsDble, 1.43177157E-02_GbcsDble, &
          2.64224321E-04_GbcsDble, 2.99291081E-06_GbcsDble, 2.03154182E-08_GbcsDble, &
          7.02620698E-11_GbcsDble, 3.79534310E-14_GbcsDble,-3.21582393E-16_GbcsDble /)
    REAL(KIND=GbcsReal), PARAMETER, DIMENSION(2) :: valid_range = (/ 188.15 , 343.15 /)

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsReal) :: temp
    INTEGER             :: i

    temp = temperature
    
    IF (temp < valid_range(1)) temp = valid_range(1)
    IF (temp > valid_range(2)) temp = valid_range(2)
    
    temp = temp - 273.15_GbcsReal

    svp = coeff(n_coeff)

    DO i = n_coeff-1,1,-1
      svp = (svp * temp) + coeff(i)
    END DO

  END FUNCTION SVP_Water_Real


!------------------------------------------------------------------------------
!F+
! NAME:
!       PPMV_to_RH
!
! PURPOSE:
!       Convert ppmv to relative humidity
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       rh = PPMV_to_RH( ppmv, pressure, temperature )
!
! INPUT ARGUMENTS:
!       ppmv
!       pressure    (hPa)
!       temperature (Kelvin)
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       relative humidity (hPa)
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 05/11/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION PPMV_to_RH_Dble( ppmv, pressure, temperature ) &
                         RESULT( rh )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT(IN) :: ppmv
    REAL(KIND=GbcsDble), INTENT(IN) :: pressure
    REAL(KIND=GbcsDble), INTENT(IN) :: temperature

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble)             :: rh

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'PPMV_to_RH_Dble'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble)             :: svp

    svp = SVP_Water_Dble( temperature )
    
    rh = 100.0_GbcsDble * ppmv * (pressure - svp) / &
             ((1.0E6_GbcsDble - ppmv) * svp)

  END FUNCTION PPMV_to_RH_Dble

  ELEMENTAL FUNCTION PPMV_to_RH_Real( ppmv, pressure, temperature ) &
                         RESULT( rh )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), INTENT(IN) :: ppmv
    REAL(KIND=GbcsReal), INTENT(IN) :: pressure
    REAL(KIND=GbcsReal), INTENT(IN) :: temperature

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal)             :: rh

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'PPMV_to_RH_Real'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsReal)             :: svp

    svp = SVP_Water_Real( temperature )
    
    rh = 100.0_GbcsReal * ppmv * (pressure - svp) / &
             ((1.0E6_GbcsReal - ppmv) * svp)

  END FUNCTION PPMV_to_RH_Real


!------------------------------------------------------------------------------
!F+
! NAME:
!       RH_to_PPMV
!
! PURPOSE:
!       Convert relative humidity to ppmv
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       ppmv = RH_to_PPMV( rh, pressure, temperature )
!
! INPUT ARGUMENTS:
!       rh
!       pressure    (hPa)
!       temperature (Kelvin)
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       PPMV
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 05/11/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION RH_to_PPMV_Dble( rh, pressure, temperature ) &
                         RESULT( ppmv )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT(IN) :: rh
    REAL(KIND=GbcsDble), INTENT(IN) :: pressure
    REAL(KIND=GbcsDble), INTENT(IN) :: temperature

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble)             :: ppmv

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'RH_to_PPMV_Dble'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble)             :: svp

    svp = SVP_Water_Dble( temperature )
    
    ppmv=         1.0E6_GbcsDble * rh / &
          (100.0_GbcsDble*(pressure-svp)/svp + rh)

  END FUNCTION RH_to_PPMV_Dble

  ELEMENTAL FUNCTION RH_to_PPMV_Real( rh, pressure, temperature ) &
                         RESULT( ppmv )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), INTENT(IN) :: rh
    REAL(KIND=GbcsReal), INTENT(IN) :: pressure
    REAL(KIND=GbcsReal), INTENT(IN) :: temperature

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal)             :: ppmv

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'RH_to_PPMV_Real'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsReal)             :: svp

    svp = SVP_Water_Real( temperature )
    
    ppmv=         1.0E6_GbcsReal * rh / &
          (100.0_GbcsReal*(pressure-svp)/svp + rh)

  END FUNCTION RH_to_PPMV_Real
END MODULE GbcsAtmPhysics
