!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsForecastModel
!
! PURPOSE:
!       
!       
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsForecastModel
!
! PUBLIC DATA:
!
! MODULES:
!       GbcsErrorHandler
!       GbcsTypes
!
! CONTAINS:
!       Register_2D_Field
!       Register_3D_Field
!       Field2D_Registered
!
! DERIVED TYPES:
!
! CREATION HISTORY:
!       Written by:   Owen Embury
!                     IAES, University of Edinburgh
!
!M-
!----------------------------------------------------------------------------------

MODULE GbcsForecastModel
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsErrorHandler
  USE GbcsTypes

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE

  ! -----------------
  ! Public data
  ! -----------------

  ! -----------------
  ! Private data
  ! -----------------
  REAL(KIND=GbcsReal), PRIVATE, ALLOCATABLE, DIMENSION(:) :: default_pressure
  REAL(KIND=GbcsReal), PRIVATE, PARAMETER                 :: default_wind =  8.63
  
  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_RTM/GbcsForecastModel.f90'

CONTAINS


!------------------------------------------------------------------------------
!F+
! NAME:
!       Free_Forecast_Model
!
! PURPOSE:
!       Deallocate all Forecast_Model arrays
!
! CATEGORY:
!       Memory deallocation
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       status = Free_Forecast_Model( Model )
!
! INPUT ARGUMENTS:
!       Model
!
! FUNCTION RESULT:
!       0 - Success
!       1 - Failure
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 05/03/2009
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Free_Forecast_Model(Model)       &
                        RESULT(status)
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model),     INTENT(OUT) :: Model

    ! ---------
    ! Function result
    ! ---------
    INTEGER :: status
   
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Free_Forecast_Model'

    status = 0

  END FUNCTION Free_Forecast_Model


!------------------------------------------------------------------------------
!F+
! NAME:
!       Register_2D_Field
!
! PURPOSE:
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!
! INPUT ARGUMENTS:
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!F-
!------------------------------------------------------------------------------
  FUNCTION Register_2D_Field(Model,       &
                             Gbcs_Map,    &
                             Gbcs_Units,  &
                             Field_Code,  &
                             Field_Name)  &
                     RESULT( i_field )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model),     INTENT(INOUT) :: Model
    INTEGER,                  INTENT(IN)    :: Gbcs_Map
    INTEGER,                  INTENT(IN)    :: Gbcs_Units
    INTEGER,        OPTIONAL, INTENT(IN)    :: Field_Code
    CHARACTER( * ), OPTIONAL, INTENT(IN)    :: Field_Name

    ! ---------
    ! Function result
    ! ---------
    INTEGER :: i_field
   
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Register_2D_Field'

    ! ---------------
    ! Local variables
    ! ---------------

    i_field = Model%N_2D_Model_Fields + 1
    
    IF (i_field > Gbcs_Max_Fields) THEN
      CALL Gbcs_Error(.TRUE., "Too many forecast fields", ROUTINE_NAME, MODULE_NAME)
      i_field = -1
      RETURN
    END IF
    
    Model%N_2D_Model_Fields       = i_field
    Model%Field_2D_Map(i_field)   = Gbcs_Map
    Model%Field_2D_Units(i_field) = Gbcs_Units
    Model%Srf_Index( Gbcs_Map )   = i_field

    IF (PRESENT(Field_Code)) Model%Field_2D_ID(I_Field)   = Field_Code
    IF (PRESENT(Field_Name)) Model%Field_2D_Name(I_Field) = Field_Name

  END FUNCTION Register_2D_Field


!------------------------------------------------------------------------------
!F+
! NAME:

  FUNCTION Register_3D_Field(Model,       &
                             Gbcs_Map,    &
                             Gbcs_Units,  &
                             Field_Code,  &
                             Field_Name)  &
                     RESULT( i_field )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model),     INTENT(INOUT) :: Model
    INTEGER,                  INTENT(IN)    :: Gbcs_Map
    INTEGER,                  INTENT(IN)    :: Gbcs_Units
    INTEGER,        OPTIONAL, INTENT(IN)    :: Field_Code
    CHARACTER( * ), OPTIONAL, INTENT(IN)    :: Field_Name

    ! ---------
    ! Function result
    ! ---------
    INTEGER :: i_field
   
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Register_3D_Field'

    ! ---------------
    ! Local variables
    ! ---------------

    i_field = Model%N_3D_Model_Fields + 1
    
    IF (i_field > Gbcs_Max_Fields) THEN
      CALL Gbcs_Error(.TRUE., "Too many forecast fields", ROUTINE_NAME, MODULE_NAME)
      i_field = -1
      RETURN
    END IF
    
    Model%N_3D_Model_Fields       = i_field
    Model%Field_3D_Map(i_field)   = Gbcs_Map
    Model%Field_3D_Units(i_field) = Gbcs_Units
    Model%Atm_Index( Gbcs_Map )   = i_field
    
    IF (PRESENT(Field_Code)) Model%Field_3D_ID(I_Field)   = Field_Code
    IF (PRESENT(Field_Name)) Model%Field_3D_Name(I_Field) = Field_Name

  END FUNCTION Register_3D_Field


!------------------------------------------------------------------------------
!F+
! NAME:

  FUNCTION Field2D_Registered( Model, Gbcs_Map ) &
                       RESULT( status )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model),     INTENT(INOUT) :: Model
    INTEGER,                  INTENT(IN)    :: Gbcs_Map

    ! ---------
    ! Function result
    ! ---------
    LOGICAL :: status
   
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Field2D_Registered'

    status = .FALSE.
    IF (Model%Srf_Index( Gbcs_Map ) > 0) status = .TRUE.

  END FUNCTION Field2D_Registered


!------------------------------------------------------------------------------
!S+
! NAME:
!       Set_Default_Profile_Pressure
!
! PURPOSE:
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Set_Default_Profile_Pressure( pressure )
!
! INPUT ARGUMENTS:
!       pressure
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     09/11/2008
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Set_Default_Profile_Pressure( pressure )
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN) :: pressure

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Set_Default_Profile_Pressure'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: n_lev, STAT
    
    n_lev = SIZE(pressure)

    ALLOCATE(default_pressure(n_lev), STAT=STAT)
    
    default_pressure = pressure

  END SUBROUTINE Set_Default_Profile_Pressure


!------------------------------------------------------------------------------
!S+
! NAME:
!       Extract_Profile
!
! PURPOSE:
!       Extract data from the model arrays and save into an Atm_Profile
!       structure. Replaces the Load_Profile subroutine in GbcsProfileGenerators
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Extract_Profile( Model, elem, line, profile )
!
! INPUT ARGUMENTS:
!       Model
!       elem
!       line
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!       profile
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!       Extract_Srf - private helper routines used
!       Extract_Atm   to simplify code
!       Solar_Angles
!       Relative_Azimuth
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       Does not fill in 'missing data' - assumes profile was initialized
!       to sensible values
!       Assumes profile is 'top-down' when determining 2m level
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     04/07/2008
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Extract_Profile( Model, elem, line, profile )
    USE GbcsTimeSpace
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model), INTENT(IN)  :: Model
    INTEGER,              INTENT(IN)  :: elem
    INTEGER,              INTENT(IN)  :: line
    TYPE(Atm_Profile),    INTENT(INOUT) :: profile

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Extract_Profile'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER       :: nlev, ilev, lev2m
    INTEGER       :: Index_Atm, Index_Srf
    INTEGER       :: iabs
    LOGICAL       :: STAT, st_u, st_v
    LOGICAL       :: satza_avail, sataz_avail, solza_avail, solaz_avail, relaz_avail

    nlev = Model%Grid%NLevels

    profile%I_Lon = elem
    profile%I_Lat = line

    STAT = Extract_Srf(Model, Gbcs_Map_P_SURF,   elem, line, profile%P_2m)
    STAT = Extract_Srf(Model, Gbcs_Map_OROG,     elem, line, profile%z0)
    st_u = Extract_Srf(Model, Gbcs_Map_U_SURF,   elem, line, profile%U_2m)
    st_v = Extract_Srf(Model, Gbcs_Map_V_SURF,   elem, line, profile%V_2m)
    stat = Extract_Srf(Model, Gbcs_Map_Wind_10M, elem, line, profile%Wind_10m)
    IF (.NOT. stat) THEN
      IF (st_u .AND. st_v) THEN
        Profile%Wind_10m = SQRT(profile%U_2m**2 + profile%V_2m**2)
      ELSE IF (st_u) THEN
        Profile%Wind_10m = ABS(profile%U_2m)
      ELSE IF (st_v) THEN
        Profile%Wind_10m = ABS(profile%V_2m)
      ELSE
        Profile%Wind_10m = default_wind
      END IF
    END IF
    IF( profile%U_2m .eq. 0. )then
       profile%U_2m = 0.01
    ENDIF
    IF( profile%V_2m .eq. 0. )then
       profile%V_2m = 0.01
    ENDIF
    STAT = Extract_Srf(Model, Gbcs_Map_TCWV,     elem, line, profile%TCWV)

    STAT = Extract_Atm(Model, Gbcs_Map_P_3D,   elem, line, profile%P)
    IF (.NOT. STAT) profile%P = default_pressure
    
    STAT = Extract_Atm(Model, Gbcs_Map_T_3D,   elem, line, profile%T)

    STAT = Extract_Atm(Model, Gbcs_Map_GH_3D,  elem, line, profile%z)
    
    ! -- Fine the model level closest to the surface (will be used for
    !    surface(2m) values if actual values are absent)
    lev2m = 0
    DO ilev = 1, nlev
      IF( profile%P(ilev) <= profile%P_2m ) THEN
        lev2m = ilev
      END IF
    END DO

    ! -- Not sure the following is correct...
    IF ( lev2m == 0 ) THEN
       lev2m = nlev
    ELSE
       lev2m = lev2m - 1
    END IF

    ! -- Fill in profile absorber fields specified in JOB script. In
    !    general this will always be water vapour

    profile%Absorber_ID = Model%Absorber_ID
    DO iabs = 1, Model%NAbsorb
      SELECT CASE (Model%Absorber_ID(iabs))
      CASE (Gbcs_Absorber_WV)
        Index_Atm = Gbcs_Map_WV_3D
        Index_Srf = Gbcs_Map_WV_2M
      CASE (Gbcs_Absorber_O3)
        Index_Atm = Gbcs_Map_O3_3D
        Index_Srf = -1
      CASE (Gbcs_Absorber_CO2)
        Index_Atm = Gbcs_Map_CO2_3D
        Index_Srf = -1
      CASE DEFAULT
        Index_Atm = -1
        Index_Srf = -1
      END SELECT

      STAT = Extract_Atm(Model, Index_Atm, elem, line, profile%A(iabs)%d)
      IF (STAT) THEN
        profile%Absorber_Units(iabs) = Model%Field_3D_Units( Model%Atm_Index(Index_Atm) ) 
      END IF
      
      STAT = Extract_Srf(Model, Index_Srf, elem, line, profile%A_2m(iabs))
      IF (STAT) THEN
        profile%Surface_Absorber_Units(iabs) = Model%Field_2D_Units( Model%Srf_Index(Index_Srf) ) 
      ELSE
        ! Could not extract surface field, so copy from 3D field
        ! Does not check if extract_atm failed - so will never use
        ! the 'default' 2m value as profile value will override it
        profile%A_2m(iabs) = Profile%A(iabs)%d(lev2m)
        profile%Surface_Absorber_Units(iabs) = profile%Absorber_Units(iabs)
      END IF
    END DO


    ! -- Remaining variables not stored in 2D/3D 'model' fields...
    STAT = Extract_Srf(Model, Gbcs_Map_T_2M, elem, line, profile%T_2m)
    IF (.NOT. STAT) THEN
      profile%T_2m = profile%T(lev2m)
    END IF

    IF (ASSOCIATED(Model%lat))   profile%lat   = Model%lat(elem,line)
    IF (ASSOCIATED(Model%lon))   profile%lon   = Model%lon(elem,line)
    IF (ALLOCATED(Model%elems))  profile%elem  = Model%elems(elem,line)
    IF (ALLOCATED(Model%lines))  profile%line  = Model%lines(elem,line)
    
    satza_avail = ASSOCIATED(Model%SatZA)
    sataz_avail = ASSOCIATED(Model%SatAz)
    solza_avail = ASSOCIATED(Model%SolZA)
    solaz_avail = ASSOCIATED(Model%SolAz)
    relaz_avail = ASSOCIATED(Model%RelAz)

    IF (satza_avail) profile%SatZA = Model%SatZA(elem,line)
    IF (sataz_avail) profile%SatAz = Model%SatAz(elem,line)

    IF (solza_avail .AND. solaz_avail) THEN
      profile%SolZA = Model%SolZA(elem,line)
      profile%SolAz = Model%SolAz(elem,line)
    ELSE
      CALL Solar_Angles(Profile%Time, Profile%lon, Profile%lat, Profile%SolZA, Profile%SolAz)
      IF (solza_avail) profile%SolZA = Model%SolZA(elem,line)
      IF (solaz_avail) profile%SolAz = Model%SolAz(elem,line)
    END IF
    
    IF (relaz_avail) THEN
      profile%RelAz = Model%RelAz(elem,line)
      IF (.NOT. sataz_avail) profile%SatAz = profile%SolAz - profile%RelAz
    ELSE
      Profile%RelAz = Relative_Azimuth(profile%SolAz , profile%SatAz)
    END IF

    IF (ALLOCATED(Model%Surface_Type)) THEN
      profile%Surface_Type = Model%Surface_Type(elem,line)
    ELSE
      IF (profile%z0 /= 0.0) THEN
        profile%Surface_Type = Gbcs_Surface_Land
      ELSE
        profile%Surface_Type = Gbcs_Surface_Sea
      END IF
    END IF

    ! -- Check if the Land Class is available
    IF (ALLOCATED(Model%Surface_Class)) THEN
      profile%Land_Class = Model%Surface_Class(elem,line)
    ELSE
      profile%Land_Class = 0
    END IF

    ! -- Decide which 'surface temperature' field to use
    Index_Srf = Get_SurfTemp_Index(Model, Profile%Surface_Type)
    STAT = Extract_Srf(Model, Index_Srf, elem, line, Profile%T_Skin)


  END SUBROUTINE Extract_Profile
  
  

!------------------------------------------------------------------------------
!S+
! NAME:
!        Construct_Profile_At_Pixel
!
! Purpose:
!        Sets up atmospheric profile from pixel data.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Construct_Profile_At_Pixel(PX, AUX, Profile)
!
! INPUT ARGUMENTS: SAT     - Satellite Structure
!                  PX      - Pixel Structure
!                  Profile - Atmospheric Profile Structure
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!       Generates a profile structure using data at the pixel location.
!       NWP data are interpolated from the 4 surrounding profiles to the
!       pixel location.
!
! CREATION HISTORY:
!       14/02/2013: Creation - Claire Bulgin
!                   Refactored from the GbcsRunRTM module. 
!                   (GbcsRunRTM remains for compatibility with legacy code).
!
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Interpolate_Profile(LocMP, Model, Profile)

    USE GbcsInterpolators

    IMPLICIT NONE

    !----------
    !Arguments
    !----------
    TYPE(WeightMP),        INTENT(IN)    :: LocMP(4) 
    TYPE(Forecast_Model),  INTENT(IN)    :: Model
    TYPE(Atm_Profile),     INTENT(INOUT) :: Profile

    !----------------
    !Local Variables
    !----------------
    INTEGER :: iabs, Index_Atm, Index_Srf

    !-----------------
    !Local Parameters
    !-----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Construct_Profile_At_Pixel'

    !Interpolate remaining model fields to PX location.

    Profile%P_2m = InterpolateField(Model%Srf(Gbcs_Map_P_SURF)%d,LocMP)
    Profile%U_2m = InterpolateField(Model%Srf(Gbcs_Map_U_SURF)%d,LocMP)
    Profile%V_2m = InterpolateField(Model%Srf(Gbcs_Map_V_SURF)%d,LocMP)
    Profile%Tmin = InterpolateField(Model%Srf(Gbcs_Map_Tmin)%d,LocMP)
    IF( ALLOCATED(Model%Srf(Gbcs_Map_OROG)%d) )THEN
       Profile%z0 = InterpolateField(Model%Srf(Gbcs_Map_OROG)%d,LocMP)
    ELSE
       Profile%z0 = -1e30
    ENDIF

    IF( ALLOCATED(Model%Atm(Gbcs_Map_GH_3D)%d) )THEN
       Profile%z = InterpolateField(Model%Atm(Gbcs_Map_GH_3D)%d,LocMP)
    ELSE
       Profile%z = -1e30
    ENDIF
    Profile%P = InterpolateField(Model%Atm(Gbcs_Map_P_3D)%d,LocMP)
    Profile%T = InterpolateField(Model%Atm(Gbcs_Map_T_3D)%d,LocMP)

    Profile%Absorber_ID = Model%Absorber_ID
    DO iabs = 1, Model%NAbsorb
      SELECT CASE (Model%Absorber_ID(iabs))
      CASE (Gbcs_Absorber_WV)
        Index_Atm = Gbcs_Map_WV_3D
        Index_Srf = Gbcs_Map_WV_2M
      CASE (Gbcs_Absorber_O3)
        Index_Atm = Gbcs_Map_O3_3D
        Index_Srf = -1
      CASE (Gbcs_Absorber_CO2)
        Index_Atm = Gbcs_Map_CO2_3D
        Index_Srf = -1
      CASE DEFAULT
        Index_Atm = -1
        Index_Srf = -1
      END SELECT
       
      IF (Index_Atm /= -1) THEN
        Profile%A(iabs)%d = &
             InterpolateField(Model%Atm(Index_Atm)%d,LocMP)
        Profile%Absorber_Units(iabs) = &
             Model%Field_3D_Units(Model%Atm_Index(Index_Atm)) 
      END IF
      IF (Index_Srf /= -1) THEN
        Profile%A_2m(iabs) = &
             InterpolateField(Model%Srf(Index_Srf)%d,LocMP)
        Profile%Surface_Absorber_Units(iabs) = &
             Model%Field_2D_Units(Model%Srf_Index(Index_Srf)) 
      END IF
    END DO

  END SUBROUTINE Interpolate_Profile


!------------------------------------------------------------------------------
!F+
! NAME:
!       Extract_Srf
!
  FUNCTION Extract_Srf( Model, Index, elem, line, val ) &
                RESULT( status )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model), INTENT(IN)  :: Model
    INTEGER,              INTENT(IN)  :: Index
    INTEGER,              INTENT(IN)  :: elem
    INTEGER,              INTENT(IN)  :: line
    REAL(KIND=GbcsReal),  INTENT(INOUT) :: val

    ! ---------
    ! Function result
    ! ---------
    LOGICAL :: status
    
    status = .FALSE.
    IF (Index < 1) RETURN
    
    val = 0
    IF (ALLOCATED(Model%Srf(Index)%d)) THEN
      val = Model%Srf(Index)%d(elem,line)
      status = .TRUE.
    END IF
    
  END FUNCTION Extract_Srf

  FUNCTION Extract_Atm( Model, Index, elem, line, val ) &
                RESULT( status )
   IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model),              INTENT(IN)  :: Model
    INTEGER,                           INTENT(IN)  :: Index
    INTEGER,                           INTENT(IN)  :: elem
    INTEGER,                           INTENT(IN)  :: line
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(INOUT) :: val

    ! ---------
    ! Function result
    ! ---------
    LOGICAL :: status
    
    status = .FALSE.
    IF (Index < 1) RETURN
    
    val(1:Model%Grid%NLevels) = 0
    IF (ALLOCATED(Model%Atm(Index)%d)) THEN
      val(1:Model%Grid%NLevels) = Model%Atm(Index)%d(:,elem,line)
      status = .TRUE.
    END IF
    
  END FUNCTION Extract_Atm


!------------------------------------------------------------------------------
!F+
! NAME:
!       Get_SurfTemp_Index
!
! PURPOSE:
!       Decide which surface temperature field (SST / LST / Skin / Surface)
!       to use.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       index = Get_SurfTemp_Index( Model, landsea )
!
! INPUT ARGUMENTS:
!       Model
!       landsea
!
! OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!       index of the surface temperature field
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 16/07/2007
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Get_SurfTemp_Index(Model, SurfaceType) &
                       RESULT(index)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model), INTENT(IN)  :: Model
    INTEGER,              INTENT(IN)  :: SurfaceType

    ! ---------
    ! Function result
    ! ---------
    INTEGER :: index

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Get_SurfTemp_Index'

    index = -1
    
    IF ( SurfaceType == Gbcs_Surface_Sea ) THEN

      IF ( ALLOCATED( Model%Srf(Gbcs_Map_SST)%d ) ) THEN
        index = Gbcs_Map_SST
      ELSE IF ( ALLOCATED( Model%Srf(Gbcs_Map_T_Skin)%d ) ) THEN
        index = Gbcs_Map_T_Skin
      ELSE IF ( ALLOCATED( Model%Srf(Gbcs_Map_T_Surf)%d ) ) THEN
        index = Gbcs_Map_T_Surf
      END IF

    ELSE
      IF ( ALLOCATED( Model%Srf(Gbcs_Map_LST)%d ) ) THEN
        index = Gbcs_Map_LST
      ELSE IF ( ALLOCATED( Model%Srf(Gbcs_Map_T_Skin)%d ) ) THEN
        index = Gbcs_Map_T_Skin
      ELSE IF ( ALLOCATED( Model%Srf(Gbcs_Map_T_Surf)%d ) ) THEN
        index = Gbcs_Map_T_Surf
      END IF
    END IF

  END FUNCTION Get_SurfTemp_Index

END MODULE GbcsForecastModel
