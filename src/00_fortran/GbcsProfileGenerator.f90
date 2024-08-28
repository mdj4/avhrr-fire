!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsProfileGenerator
!
! PURPOSE:
!       This module contains profile conversion subroutines and functions
!
!   Load_Profile is now obsolete - replaced by code in GbcsForecastModel and
!   should be removed eventually. Then only dependence on GbcsTypes will be
!   for Atm_Profile, which could be moved to this module
!
!       
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsProfileGenerator
!
! PUBLIC DATA:
!
! MODULES:
!       GbcsKinds
!       GbcsTypes
!       GbcsErrorHandler
!
! CONTAINS:
!       Create_Profile
!       Destroy_Profile
!       Load_Profile
!       Gen_Interpol_Weights
!       Apply_Interpol_Weights
!
! DERIVED TYPES:
!
! CREATION HISTORY:
!       0.0   12/04/2005 CPO: Creation                                                   CPO
!       0.1   08/11/2008 OE:  Added module header
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

MODULE GbcsProfileGenerator
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds
  USE GbcsTypes, ONLY: Atm_Profile

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE

  ! -----------------
  ! Public data
  ! -----------------

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Profiles/GbcsProfileGenerator.f90'

CONTAINS

!------------------------------------------------------------------------------
!F+
! NAME:
!       Create_Profile
!
! PURPOSE:
!       Allocate memory for an Atm_Profile structure
!
! CATEGORY:
!       Memory allocation
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       STAT = Create_Profile( NLevels , NAbsorb , Profile )
!
! INPUT ARGUMENTS:
!       NLecels
!       NAbsorb
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!       Profile
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!       Error_Status
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
!       0.0   02/05/2005    Creation                                                   CPO
!F-
!------------------------------------------------------------------------------
  FUNCTION Create_Profile( NLevels , NAbsorb , Profile ) &
                   RESULT( status )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    INTEGER,           INTENT(IN)    :: NLevels
    INTEGER,           INTENT(IN)    :: NAbsorb
    Type(Atm_Profile), INTENT(INOUT) :: Profile

    ! ---------
    ! Function result
    ! ---------
    INTEGER                           :: status

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Create_Profile'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: Absorber
    INTEGER :: STAT


    status = 0

    Profile%NLevels   = NLevels
    Profile%NAbsorb   = NAbsorb
    Profile%Allocated = .FALSE.

    ALLOCATE(Profile%z(NLevels),       &
             Profile%P(NLevels),       &
             Profile%T(NLevels),       &
             Profile%aerosol(NLevels), &
             STAT=STAT)
    IF (STAT /= 0) status = STAT

    ! -- Allocate absorber profiles
    DO Absorber = 1,Profile%NAbsorb
      ALLOCATE(Profile%A(Absorber)%d(NLevels), STAT=STAT)
      IF (STAT /= 0) status = STAT
    END DO        

    IF (status == 0) Profile%Allocated = .TRUE.

  END FUNCTION Create_Profile


  FUNCTION Destroy_Profile( Profile )

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
  ! 0.0   02/05/2005    Creation                                                   CPO
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

    USE GbcsErrorHandler, ONLY: Gbcs_Log_File_Unit

    IMPLICIT NONE

    INTEGER :: Destroy_Profile

    Type(Atm_Profile), INTENT (INOUT) :: Profile

    INTEGER :: Absorber
    INTEGER :: STAT, Destroy_Status
    
    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Destroy_Profile'

      Destroy_Status = 0
      STAT = 0

      IF ( ALLOCATED(Profile%z) ) THEN
        DEALLOCATE(Profile%z, STAT=STAT)
      END IF

      IF (STAT /= 0) THEN
        Destroy_Status = STAT
        WRITE(Gbcs_Log_File_Unit,*)'warning problem deallocating space for profile z values',STAT
      END IF

      IF ( ALLOCATED(Profile%P) ) THEN
        DEALLOCATE(Profile%P, STAT=STAT)
      END IF

      IF (STAT /= 0) THEN
        Destroy_Status = STAT
        WRITE(Gbcs_Log_File_Unit,*)'warning problem deallocating space for profile P values',STAT
      END IF

      IF ( ALLOCATED(Profile%T) ) THEN
        DEALLOCATE(Profile%T, STAT=STAT)
      END IF

      IF (STAT /= 0) THEN
        Destroy_Status = STAT
        WRITE(Gbcs_Log_File_Unit,*)'warning problem deallocating space for profile T values',STAT
      END IF

      IF ( ALLOCATED(Profile%Aerosol) ) THEN
        DEALLOCATE(Profile%Aerosol, STAT=STAT)
      END IF

      DO Absorber = 1,MIN(Profile%NAbsorb, 30)

        IF ( ALLOCATED(Profile%A(Absorber)%d) ) THEN
          DEALLOCATE(Profile%A(Absorber)%d, STAT=STAT)
        END IF

        IF (STAT /= 0) THEN
          Destroy_Status = STAT
          WRITE(Gbcs_Log_File_Unit,*)'warning problem deallocating space for profile A(',Absorber,') values for absorber ',STAT
        END IF

      END DO        

      Profile%Allocated = .FALSE.

      Destroy_Profile = Destroy_Status

    RETURN

  END FUNCTION Destroy_Profile


  SUBROUTINE Load_Profile( AUX , Elem , Line , Profile )

  !
  ! Description:
  !
  !  Loads profiles already in RTM format
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   23/07/2007    Creation                                                   CPO
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
    USE GbcsTypes

    IMPLICIT NONE

    Type(Aux_Data),    INTENT (IN   ) :: AUX
    INTEGER,           INTENT (IN   ) :: Elem
    INTEGER,           INTENT (IN   ) :: Line
    Type(Atm_Profile), INTENT (INOUT) :: Profile

    INTEGER :: Absorber
    INTEGER :: Level
    INTEGER :: NLevels
    INTEGER :: Level_2m
    INTEGER :: WVIndx

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Load_Profile'


      NLevels = AUX%Model%Grid%NLevels

      Profile%I_Lon = Elem
      Profile%I_Lat = Line
      
      IF ( ASSOCIATED(AUX%Model%SatZA) ) THEN
         Profile%SatZA = AUX%Model%SatZA(Elem,Line)
      ELSE
         Profile%SatZA = 0.0
      END IF
      
      IF( ALLOCATED(AUX%Model%Atm(Gbcs_Map_P_3D)%d) )THEN
        Profile%P( 1:NLevels ) = AUX%Model%Atm(Gbcs_Map_P_3D)%d(:,Elem,Line)
      ELSE
        Profile%P(:) = 0.0
      END IF

      IF( ALLOCATED(AUX%Model%Atm(Gbcs_Map_T_3D)%d) )THEN
        Profile%T( 1:NLevels ) = AUX%Model%Atm(Gbcs_Map_T_3D)%d(:,Elem,Line)
      ELSE
        Profile%T(:) = 0.0
      END IF

      IF( ALLOCATED(AUX%Model%Atm(Gbcs_Map_GH_3D)%d) )THEN
         Profile%z( 1:NLevels ) = AUX%Model%Atm(Gbcs_Map_GH_3D)%d(:,Elem,Line)
      ELSE
        Profile%z(:) = 0.0
      END IF

      Profile%Absorber_ID = AUX%Model%Absorber_ID
      Profile%Absorber_Units = AUX%Model%Absorber_Units
      DO Absorber = 1,Profile%NAbsorb
         SELECT CASE ( Profile%Absorber_ID( Absorber ) )
         CASE ( Gbcs_Absorber_WV )
           Profile%A(Absorber)%d(1:NLevels) = AUX%Model%Atm(Gbcs_Map_WV_3D)%d(:,Elem,Line)
           WVIndx = Absorber
         CASE ( Gbcs_Absorber_O3 )
           Profile%A(Absorber)%d(1:NLevels) = AUX%Model%Atm(Gbcs_Map_O3_3D)%d(:,Elem,Line)
         CASE ( Gbcs_Absorber_CO2 )
           Profile%A(Absorber)%d(1:NLevels) = AUX%Model%Atm(Gbcs_Map_CO2_3D)%d(:,Elem,Line)
         CASE DEFAULT
           Profile%A(Absorber)%d(:) = 0.0
         END SELECT
      END DO

      IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_OROG)%d ) ) THEN
        Profile%z0 = AUX%Model%Srf(Gbcs_Map_OROG)%d(Elem,Line)
      ELSE
        Profile%z0 = 0.0
      END IF

      IF ( ALLOCATED( AUX%Model%Surface_Type ) ) THEN
         Profile%Surface_Type = AUX%Model%Surface_Type(Elem,Line)
      ELSE
         IF (Profile%z0 /= 0.0) THEN
            Profile%Surface_Type = Gbcs_Surface_Land
         ELSE
            Profile%Surface_Type = Gbcs_Surface_Sea
         END IF
      END IF

      IF ( Profile%Surface_Type == Gbcs_Surface_Sea ) THEN

        IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_SST)%d ) ) THEN
          Profile%T_Skin = AUX%Model%Srf(Gbcs_Map_SST)%d(Elem,Line) ! use climatologically-corrected model ST if available
        ELSE IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_T_Skin)%d ) ) THEN
          Profile%T_Skin = AUX%Model%Srf(Gbcs_Map_T_Skin)%d(Elem,Line) ! otherwise use profile skin T if available
        ELSE IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_T_Surf)%d ) ) THEN
          Profile%T_Skin = AUX%Model%Srf(Gbcs_Map_T_Surf)%d(Elem,Line)! otherwise use profile ST if available
        ELSE
          WRITE(*,*)'ERROR: No background sea surface temperature for RTM'
          STOP 1
        END IF

      ELSE

        IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_LST)%d ) ) THEN
          Profile%T_Skin = AUX%Model%Srf(Gbcs_Map_LST)%d(Elem,Line)  ! use profile ST with elevation correction if available
        ELSE IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_T_Skin)%d ) ) THEN
          Profile%T_Skin = AUX%Model%Srf(Gbcs_Map_T_Skin)%d(Elem,Line)! otherwise use profile skin T if available
        ELSE IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_T_Surf)%d ) ) THEN
          Profile%T_Skin = AUX%Model%Srf(Gbcs_Map_T_Surf)%d(Elem,Line)! otherwise use profile ST if available
        ELSE
          WRITE(*,*)'ERROR: No background land surface temperature for RTM'
          STOP 1
        END IF

      END IF

      Profile%P_2m = AUX%Model%Srf(Gbcs_Map_P_SURF)%d(Elem,Line)

      Level_2m = 0
      DO Level = 1, NLevels
        IF ( Profile%P(Level) .LE. Profile%P_2m ) THEN
          Level_2m = Level
        END IF
      END DO

      IF ( Level_2m .EQ. 0 ) THEN
        Level_2m = NLevels
      ELSE
        Level_2m = Level_2m - 1
      END IF

      IF ( ALLOCATED( AUX%Model%Srf(Gbcs_Map_T_2M)%d ) ) THEN
        Profile%T_2m = AUX%Model%Srf(Gbcs_Map_T_2M)%d(Elem,Line)
      ELSE
        Profile%T_2m = Profile%T(Level_2m)
      END IF

      DO Absorber = 1, Profile%NAbsorb
        SELECT CASE ( Profile%Absorber_ID( Absorber ) )
          CASE ( Gbcs_Absorber_WV )
            IF( ALLOCATED(AUX%Model%Srf(Gbcs_Map_WV_2M)%d) )THEN
                Profile%A_2m(Absorber) = AUX%Model%Srf(Gbcs_Map_WV_2M)%d(Elem,Line)
                Profile%Surface_Absorber_Units( Absorber ) = AUX%Model%Field_2D_Units( AUX%Model%Srf_Index(Gbcs_Map_WV_2M) ) 
            ELSE
                Profile%A_2m(Absorber) = Profile%A(Absorber)%d(Level_2m)
                Profile%Surface_Absorber_Units( Absorber ) = Profile%Absorber_Units( WVIndx )
            END IF
          CASE ( Gbcs_Absorber_O3 )
            Profile%A_2m(Absorber) = Profile%A(Absorber)%d(Level_2m)
            Profile%Surface_Absorber_Units( Absorber ) = Gbcs_Units_ppmv 
          CASE ( Gbcs_Absorber_CO2 )
            Profile%A_2m(Absorber) = Profile%A(Absorber)%d(Level_2m)
            Profile%Surface_Absorber_Units( Absorber ) = Gbcs_Units_ppmv 
          CASE DEFAULT
            Profile%A_2m(Absorber) = 0.0
            Profile%Surface_Absorber_Units( Absorber ) = Gbcs_Units_ppmv 
        END SELECT
      END DO

      IF ( ALLOCATED(AUX%Model%Srf(Gbcs_Map_U_SURF)%d) ) THEN
        Profile%U_2m = AUX%Model%Srf(Gbcs_Map_U_SURF)%d(Elem,Line)
      ELSE
        Profile%U_2m = 0.0
      END IF

      IF ( ALLOCATED(AUX%Model%Srf(Gbcs_Map_V_SURF)%d) ) THEN
        Profile%V_2m = AUX%Model%Srf(Gbcs_Map_V_SURF)%d(Elem,Line)
      ELSE
        Profile%V_2m = 0.0
      END IF

      IF ( ALLOCATED(AUX%Model%Srf(Gbcs_Map_Wind_10M)%d) ) THEN
        Profile%Wind_10m = AUX%Model%Srf(Gbcs_Map_Wind_10M)%d(Elem,Line)
      ELSE
        IF ( Profile%Surface_Type == Gbcs_Surface_Sea ) THEN
          Profile%Wind_10m = 8.63
        ELSE
           Profile%Wind_10m = 0.0
         END IF
      END IF

    ! Initialise salinity to 350 dg/l
    Profile%Salinity=350.0
    
    RETURN    

  END SUBROUTINE Load_Profile


!------------------------------------------------------------------------------
!S+
! NAME:
!       Gen_Interpol_Weights
!
! PURPOSE:
!       Generate interpolation weights required to interpolate the profile
!       to the supplied pressure levels
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Gen_Interpol_Weights( profile, nlevels, plevs, 
!                                  int_let, int_fac, int_srf )
!
! INPUT ARGUMENTS:
!       Profile   - Atm_Profile structure to interpolate
!       nlevels   - number of output levels
!       plevs     - output pressure levels
!
! OUTPUT ARGUMENTS:
!       int_lev   - input layer corresponding to each output level
!       int_fac   - output weighting in layer
!       int_srf   - input level corresponding to surface
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       Input profile must be ordered ToA -> surface
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     07/07/2008
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Gen_Interpol_Weights( profile, &
                                   nlevels, &
                                   plevels, &
                                   int_lev, &
                                   int_fac, &
                                   int_srf )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Atm_Profile),                       INTENT(IN)  :: profile
    INTEGER,                                 INTENT(IN)  :: nlevels
    REAL(KIND=GbcsReal), DIMENSION(nlevels), INTENT(IN)  :: plevels
    INTEGER,             DIMENSION(nlevels), INTENT(OUT) :: int_lev
    REAL(KIND=GbcsReal), DIMENSION(nlevels), INTENT(OUT) :: int_fac
    INTEGER,                                 INTENT(OUT) :: int_srf

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Gen_Interpol_Weights'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsReal), DIMENSION(nlevels)         :: lpress_out
    REAL(KIND=GbcsReal), DIMENSION(profile%NLevels) :: lpress_in
    
    INTEGER  ::  i_lev, o_lev
    
    IF (profile%P(1) > profile%P(2)) THEN
      WRITE(*,*) 'Surface-up profiles not supported'
      WRITE(*,*) 'Either invert when loading or add support to Profile interpolator'
      STOP 1
    END IF
    
    lpress_out = LOG(plevels)
    lpress_in  = LOG(profile%P)
    
    ! -- Check for any 'below surface' input levels
    DO int_srf = 1, profile%NLevels
      IF (profile%P(int_srf) > profile%P_2m) EXIT
    END DO

    DO o_lev = 1, nlevels
      DO i_lev = 1, MIN(int_srf-2, profile%NLevels-1)
        IF (profile%P(i_lev+1) > plevels(o_lev)) EXIT
      END DO
      int_lev(o_lev) = i_lev
      
      IF (i_lev == (int_srf - 1)) THEN
        int_fac(o_lev) = (lpress_in(i_lev) - lpress_out(o_lev)) / &
                         (lpress_in(i_lev) - LOG(profile%P_2m))
      ELSE
        int_fac(o_lev) = (lpress_in(i_lev) - lpress_out(o_lev)) / &
                         (lpress_in(i_lev) - lpress_in(i_lev+1))
      END IF
      
      ! -- Prevent extrapolation outside input profile
      IF (int_fac(o_lev) < 0.0) int_fac(o_lev) = 0.0
      IF (int_fac(o_lev) > 1.0) int_fac(o_lev) = 1.0
    END DO
    
  END SUBROUTINE Gen_Interpol_Weights


!------------------------------------------------------------------------------
!F+
! NAME:
!       Apply_Interpol_Weights
!
! PURPOSE:
!       Apply interpolation coefficients from Gen_Interpol_Weights to
!       supplied data to produce interpolated profile
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Apply_Interpol_Weights( profdat, surfdat, nlevels,
!                                    int_let, int_fac, int_srf )
!
! INPUT ARGUMENTS:
!       profdat   - profile data from Atm_Profile structure
!       surfdat   - surface data from Atm_Profile structure
!       nlevels   - number of output levels
!       int_lev   - input layer corresponding to each output level
!       int_fac   - output weighting in layer
!       int_srf   - input level corresponding to surface
!
! RESULT:
!       profout(nlevels)
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       Input profile must be ordered ToA -> surface
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     07/07/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Apply_Interpol_Weights(profdat, &
                                  surfdat, &
                                  nlevels, &
                                  int_lev, &
                                  int_fac, &
                                  int_srf ) &
                          RESULT( profout )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(:),       INTENT(IN)  :: profdat
    REAL(KIND=GbcsReal),                     INTENT(IN)  :: surfdat
    INTEGER,                                 INTENT(IN)  :: nlevels
    INTEGER,             DIMENSION(nlevels), INTENT(IN)  :: int_lev
    REAL(KIND=GbcsReal), DIMENSION(nlevels), INTENT(IN)  :: int_fac
    INTEGER,                                 INTENT(IN)  :: int_srf

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(nlevels)              :: profout
    
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Apply_Interpol_Weights'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER             ::  o_lev
    REAL(KIND=GbcsReal) :: val


    DO o_lev = 1, nlevels
      IF (int_lev(o_lev)+1 < int_srf) THEN
        val = profdat(int_lev(o_lev)+1)
      ELSE
        val = surfdat
      END IF
      profout(o_lev) = profdat(int_lev(o_lev)) * (1.0-int_fac(o_lev)) + &
                        val * int_fac(o_lev)
    END DO
  END FUNCTION Apply_Interpol_Weights


!------------------------------------------------------------------------------
!S+
! NAME:
!       Calculate_Profile_Altitude
!
! PURPOSE:
!       Calculate level heights for profile
!       NOTE - also generate additional aerosol info (profile + near-surf RH)
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Calculate_Profile_Altitude( profile )
!
! INPUT ARGUMENTS:
!       Profile   - Atm_Profile structure to interpolate
!
! OUTPUT ARGUMENTS:
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     11/11/2008
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Calculate_Profile_Altitude( profile )
    USE GbcsConstants
    USE GbcsGeopotential
    USE GbcsAtmPhysics
    USE GbcsErrorHandler
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Atm_Profile), INTENT(INOUT)    :: Profile

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Calculate_Profile_Altitude'
    ! -- Marine aerosol parameters:
    REAL(KIND=GbcsReal), PARAMETER :: hs = 1.0
    REAL(KIND=GbcsReal), PARAMETER :: z1 = 2.0
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER   :: i_abs, i_lev, WVIndx, n_lev
    REAL(KIND=GbcsReal), DIMENSION(Profile%NLevels) :: relhum
!    REAL(KIND=GbcsReal), DIMENSION(Profile%NLevels) :: cleared, svp

    ! -- Check for invalid profile input
    IF (Profile%P_2m < 1.0) THEN
      Profile%relhum = 90.0
      RETURN
    END IF

    n_lev = Profile%NLevels
    !--------------------------------------------------------------------------
    ! Determine which absorbers are available
    !--------------------------------------------------------------------------
    WVIndx = 0
    DO i_abs = 1,Profile%NAbsorb
      IF (Profile%Absorber_ID(i_abs) == Gbcs_Absorber_WV) WVIndx = i_abs
    END DO

    CALL Gbcs_Critical(WVIndx==0, "Profile watervapour unavailable", ROUTINE_NAME, MODULE_NAME)
    
    CALL Geopotential_Height(Profile%p,           &
                             Profile%t,           &
                             Profile%A(WVIndx)%d, &
                             Profile%P_2m,        &
                             Profile%z0,          &
                             Profile%z,           &
                             Profile%lat          )

    ! -- Calculate aerosol column density for each layer
    DO i_lev = 1,n_lev-1
      Profile%aerosol(i_lev) = hs * (EXP(-MIN(z1,Profile%z(i_lev+1))/hs) - &
                                     EXP(-MIN(z1,Profile%z(i_lev))/hs)   )
    END DO
    IF (Profile%z(i_lev) > Profile%z0 .AND. Profile%z0 > 0.0) THEN
      Profile%aerosol(i_lev) = hs * (EXP(-MIN(z1,Profile%z0)/hs)         - &
                                     EXP(-MIN(z1,Profile%z(i_lev))/hs)   )
    ELSE
      Profile%aerosol(i_lev) = 0.0
    END IF
    ! -- Normalize aerosol profile to sum to one
    !    Workaround for VisRTM as the coefficients are
    !    pre-calculated for a standard aerosol profile
    !    Also makes it easy to calculate average relhum...
    Profile%aerosol = Profile%aerosol / (hs * (1.0 - EXP(-z1/hs)))

    
    relhum = MIN(MAX(PPMV_to_RH(Profile%A(WVIndx)%d, Profile%p, Profile%t), 0.0), 99.0)
    
!    ! -- Cloud clearing (EXPERIMENTAL)
!    
!    DO i_lev=1, n_lev
!      cleared(i_lev) = MIN(relhum(i_lev), 95.0)
!    END DO
!    cleared = RH_to_PPMV(cleared, Profile%p, Profile%t)
!    
!    ! - ppmv corresponding to 90% RH
!    svp = 1.0E6_GbcsReal * 0.90 * SVP_Water(Profile%t) / Profile%p
!
!    DO i_lev=1, n_lev
!!      IF (relhum(i_lev) > 95.0) Profile%A(WVIndx)%d(i_lev) = cleared(i_lev)
!      Profile%A(WVIndx)%d(i_lev) = MIN(Profile%A(WVIndx)%d(i_lev), svp(i_lev))
!    END DO

    ! -- Just average level values to get layer for now...
    relhum(1:n_lev-1) = (relhum(1:n_lev-1) + relhum(2:n_lev)) / 2.0

    Profile%relhum = SUM(Profile%aerosol * relhum)

  END SUBROUTINE Calculate_Profile_Altitude

END MODULE GbcsProfileGenerator

