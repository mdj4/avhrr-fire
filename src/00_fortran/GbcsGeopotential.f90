!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsGeopotential
!
! PURPOSE:
!       Various routines for calculating geopotential height etc.
!       
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsGeopotential
!
! PUBLIC DATA:
!
! MODULES:
!       GbcsKinds
!       GbcsAtmPhysics
!       
! CONTAINS:
!       Earth_Radius
!       Normal_Gravity
!       Geopotential_Height
!       
!
! DERIVED TYPES:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     08/11/2008
!                     IAES, University of Edinburgh
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
!M-
!----------------------------------------------------------------------------------

MODULE GbcsGeopotential
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds

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
  REAL(KIND=GbcsDble), PARAMETER :: EarthRadius_Equator = 6378.137 
  REAL(KIND=GbcsDble), PARAMETER :: EarthRadius_Polar   = 6356.752
  REAL(KIND=GbcsDble), PARAMETER :: EarthRadius_Mean    = 6371.000


  REAL(KIND=GbcsDble), PARAMETER :: D_Pi  = 3.14159265358979323846_GbcsDble  ! Pi                  ( double precision )
  REAL(KIND=GbcsDble), PARAMETER :: D_Deg2Rad = D_Pi/180.0            ! degrees to radians  ( double precision )
  REAL(KIND=GbcsDble), PARAMETER :: D_Rad2Deg = 180.0/D_Pi            ! radians to degrees  ( double precision )

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Physics/GbcsGeopotential.f90'

CONTAINS

!------------------------------------------------------------------------------
!F+
! NAME:
!       Earth_Radius
!
! PURPOSE:
!       Calculate the Earth's radius at the given latitude
!
! CATEGORY:
!       
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       radius = Earth_Radius( latitude )
!
! INPUT ARGUMENTS:
!       latitude
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!       radius
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
  ELEMENTAL FUNCTION Earth_Radius( latitude ) &
                 RESULT( radius )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT(IN) :: latitude

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble)             :: radius

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),      PARAMETER :: ROUTINE_NAME = 'Earth_Radius'
    REAL(KIND=GbcsDble), PARAMETER :: eccen2 = 1 -  EarthRadius_Polar**2 / &
                                                  EarthRadius_Equator**2

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble) :: cos_lat, scale_factor
    
    cos_lat      = COS(latitude * D_Deg2Rad)
    scale_factor = SQRT(1.0 - eccen2 * (cos_lat ** 2.0))
    
    radius = EarthRadius_Polar / scale_factor

  END FUNCTION Earth_Radius


!------------------------------------------------------------------------------
!F+
! NAME:
!       Normal_Gravity
!
! PURPOSE:
!       Calculate the surface gravity (including centripetal acceleration) at
!       the specified latitude.
!       Gives standard gravity at lat ~45
!
! CATEGORY:
!       
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       gravity = Normal_Gravity( latitude )       
!
! INPUT ARGUMENTS:
!       latitude
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!       radius
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
!       Geodetic Reference System 1980 http://www.gfy.ku.dk/~iag/handbook/geodeti.htm
!
! CREATION HISTORY:
!       Written by:   Owen Embury     08/11/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Normal_Gravity( latitude ) &
                             RESULT( gravity )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT(IN) :: latitude

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble)             :: gravity

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),      PARAMETER :: ROUTINE_NAME = 'Normal_Gravity'

    REAL(KIND=GbcsDble),               PARAMETER :: g_equator = 9.7803267715
    REAL(KIND=GbcsDble), DIMENSION(4), PARAMETER :: g_coeff = &
          (/5.2790414e-03, 2.32718e-05, 1.262e-07, 7.0e-10 /)

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble) :: sin_lat
    
    sin_lat = SIN(latitude * D_Deg2Rad)

    gravity = g_equator * ( 1.0         + &
                g_coeff(1) * sin_lat ** 2 + &
                g_coeff(2) * sin_lat ** 4 + &
                g_coeff(3) * sin_lat ** 6 + &
                g_coeff(4) * sin_lat ** 8)

  END FUNCTION Normal_Gravity


!------------------------------------------------------------------------------
!S+
! NAME:
!       Geopotential_Height
!
! PURPOSE:
!       Pixel-by-pixel processing of imagery - calculates the probability of
!       each pixel being in a clear scene
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Geopotential_Height(pressure, temperature, watervapour, psurf, &
!                                zsurf,    height,      latitude)
!
! INPUT ARGUMENTS:
!       pressure
!       temperature
!       watervapour
!       psurf
!       zsurf
!
! OPTIONAL INPUT ARGUMENTS:
!       latitude
!
! OUTPUT ARGUMENTS:
!       height
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!       Normal_Gravity
!       MolWeight_Air
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       Does not check for valid input profile
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury   09/11/2008
!                     IAES, University of Edinburgh
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Geopotential_Height(pressure,    &
                                 temperature, &
                                 watervapour, &
                                 psurf,       &
                                 zsurf,       &
                                 height,      &
                                 latitude)
    USE GbcsAtmPhysics, ONLY: MolWeight_Air
    USE GbcsConstants,  ONLY: Standard_Gravity
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN)  :: pressure
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN)  :: temperature
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN)  :: watervapour
    REAL(KIND=GbcsReal),               INTENT(IN)  :: psurf
    REAL(KIND=GbcsReal),               INTENT(IN)  :: zsurf
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(OUT) :: height
    REAL(KIND=GbcsReal), OPTIONAL,     INTENT(IN)  :: latitude
  
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),      PARAMETER :: ROUTINE_NAME = 'Geopotential_Height'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: isurf, i_lev
    INTEGER :: n_lev
    REAL(KIND=GbcsDble), DIMENSION(SIZE(pressure)) :: logpress, mwair
    REAL(KIND=GbcsDble) :: lay_t, dz, gravity, lay_wt
    
    height = -1.0
    n_lev  = SIZE(pressure)
    
    IF (PRESENT(latitude)) THEN
      gravity = Normal_Gravity(REAL(latitude, KIND=GbcsDble))
    ELSE
      gravity = Standard_Gravity
    END IF
    
    logpress = LOG(pressure)
    mwair    = MolWeight_Air(REAL(watervapour, KIND=GbcsDble))

    ! -- Find where surface level is
    DO isurf=1,n_lev
      IF (pressure(isurf) > psurf) EXIT
    END DO
    
    ! -- Below surface levels
    DO i_lev=isurf,n_lev
      height(i_lev) = 0.0
    END DO

    lay_t  = temperature(isurf-1)
    lay_wt = mwair(isurf-1)
    
    dz = 8.314472 * lay_t * (LOG(psurf) - logpress(isurf-1)) / lay_wt / gravity
    height(isurf-1) = dz
    
    DO i_lev=isurf-2,1,-1
      lay_t  = (temperature(i_lev) + temperature(i_lev+1)) / 2.0
      lay_wt = (mwair(i_lev) + mwair(i_lev+1)) / 2.0
      dz = 8.314472 * lay_t * (logpress(i_lev+1) - logpress(i_lev)) / lay_wt / gravity
      height(i_lev) = height(i_lev+1) + dz
    END DO
    
  END SUBROUTINE Geopotential_Height


!------------------------------------------------------------------------------
!S+
! NAME:
!       Geometric_Height
!
! PURPOSE:
!       Converts geopotential to geometric height - used to calculate
!       surface elevation from surface level geopotential.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Geometric_Height
!
! INPUT ARGUMENTS: PX%elev
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS: PX%elev
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
!       02/09/13 OE:  Tidy, add intent statements
!       Written by:   Claire Bulgin   10/2011
!                     IAES, University of Edinburgh
!
!      Conversion formula taken from the Federal Meteorological Handbook No. 3 : 
!      Rawinsonde and Pibal Observatoins. OFCM, May 1997.
!      Unit Conversion Calculators (http://hurri.kean.edu/~yoh/calculations/height/
!      height.html).
!S-
!------------------------------------------------------------------------------
  FUNCTION Geometric_Height (geopotential, latitude) RESULT (geometric_elev)
    USE GbcsConstants
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    REAL(Kind=GbcsReal), INTENT(IN) :: geopotential
    REAL(Kind=GbcsReal), INTENT(IN) :: latitude

    ! ---------
    ! Function result
    ! ---------
    REAL(Kind=GbcsDble) :: geometric_elev

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),      PARAMETER :: ROUTINE_NAME = 'Geometric_Height'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(Kind=GbcsReal) :: radius
    REAL(Kind=GbcsDble) :: lat
    REAL(Kind=GbcsDble) :: gravity_ratio
    REAL(Kind=GbcsDble) :: geopotential_height

    lat = REAL(latitude, Kind=GbcsDble)
 
    !ECMWF geopotential is in m2s2 - divide by gravitational constant to get
    !geopotential height.
    geopotential_height = geopotential / Standard_Gravity 

    radius = Earth_Radius(lat) * 1000 !Convert into metres.

    gravity_ratio = Normal_Gravity(lat) / Standard_Gravity

    geometric_elev =      (geopotential_height*radius) / &
                    ((gravity_ratio*radius) - geopotential_height)
   
  END FUNCTION  Geometric_Height


END MODULE GbcsGeopotential
