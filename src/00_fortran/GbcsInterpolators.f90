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



!+ This module contains interpolations subroutines and functions

MODULE GbcsInterpolators

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
! 0.0   04/03/2005    Creation                                                   CPO
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


  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE


  INTERFACE InterpolateField
    MODULE PROCEDURE InterpolateField2, InterpolateField3, InterpolateField4
  END INTERFACE InterpolateField


  SAVE

  ! For linear_interp - stop allocating/deallocating every time
  INTEGER :: linear_dx_size = 0
  INTEGER :: linear_dx_step = 0
  REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE :: linear_dx
  PRIVATE :: linear_dx_size
  PRIVATE :: linear_dx_step
  PRIVATE :: linear_dx

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Maths/GbcsInterpolators.f90'

CONTAINS


  SUBROUTINE ModelIndices(PX,Grid,SurfType)

  !
  ! Description:
  !
  !  Subroutine for finding the indices of the nearest model grid points to the current
  !  image pixel.
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   07/03/2005    Creation                                                   CPO
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

  ! Modules used:

    USE GbcsKinds
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

  ! Define Input Variabiles

    TYPE(ImagePixel),       INTENT (INOUT) :: PX
    TYPE(Grid_Descriptor),  INTENT (IN   ) :: Grid
    INTEGER,DIMENSION(:,:), INTENT (IN   ) :: SurfType

  ! Define Local Variabiles

    REAL(Kind=GbcsReal) :: offlat, offlon
    REAL(Kind=GbcsReal) :: offcyclic
    REAL(Kind=GbcsReal) :: ModelLat, ModelLon
    INTEGER :: NCnt(4,4)
    INTEGER :: MCnt(4) , SCnt(9)
    INTEGER :: ii , jj , i_lat , i_lon
    INTEGER :: Lat_Scale , Lon_Scale
    
    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'ModelIndices'


      SELECT CASE (Grid%DataLoc)

        CASE (Gbcs_At_Corner)

          offlat = Grid%LLCrnr%lat
          offlon = Grid%LLCrnr%lon

        CASE (Gbcs_At_Centre)

          offlat = Grid%LLCrnr%lat + ( Grid%Incr%lat / 2.0 )
          offlon = Grid%LLCrnr%lon + ( Grid%Incr%lon / 2.0 )

      END SELECT

      offcyclic = 0.0

      IF (PX%EarthCoord%lon < Grid%LLCrnr%lon) THEN
        offcyclic = 360.0
      END IF

      PX%FMMap%LonLo = FLOOR((PX%EarthCoord%lon + offcyclic - offlon)/Grid%Incr%lon) + 1
      IF ( PX%FMMap%LonLo < 1 ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonLo = Grid%NElems
        ELSE
          PX%FMMap%LonLo = 1
        END IF
      ELSE IF ( PX%FMMap%LonLo > Grid%NElems ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonLo = 1
        ELSE
          PX%FMMap%LonLo = Grid%NElems
        END IF
      END IF
      PX%FMMap%LonHi = PX%FMMap%LonLo + 1
      IF ( PX%FMMap%LonHi > Grid%NElems ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonHi = 1
        ELSE
          PX%FMMap%LonHi = Grid%NElems
        END IF
      END IF

      PX%FMMap%LatLo = FLOOR((PX%EarthCoord%lat - offlat)/Grid%Incr%lat) + 1
      IF ( PX%FMMap%LatLo < 1 ) THEN 
        PX%FMMap%LatLo = 1
      ELSE IF ( PX%FMMap%LatLo > Grid%NLines ) THEN 
        PX%FMMap%LatLo = Grid%NLines
      END IF
      PX%FMMap%LatHi = PX%FMMap%LatLo + 1
      IF ( PX%FMMap%LatHi > Grid%NLines ) PX%FMMap%LatHi = Grid%NLines

      Lon_Scale = 1
      Lat_Scale = 1

      IF ( SurfType(PX%FMMap%LonLo,PX%FMMap%LatLo) /= PX%Surface_Type ) THEN

        MCnt = 0

        IF ( SurfType(PX%FMMap%LonLo,PX%FMMap%LatLo) == PX%Surface_Type ) MCnt(1) = 1
        IF ( SurfType(PX%FMMap%LonHi,PX%FMMap%LatLo) == PX%Surface_Type ) MCnt(2) = 1
        IF ( SurfType(PX%FMMap%LonHi,PX%FMMap%LatHi) == PX%Surface_Type ) MCnt(3) = 1
        IF ( SurfType(PX%FMMap%LonLo,PX%FMMap%LatHi) == PX%Surface_Type ) MCnt(4) = 1

        IF ( SUM( MCnt ) == 0 ) THEN

          NCnt = 0

          DO jj = 1,4
            i_lat = PX%FMMap%LatLo - 2 + jj
            IF ( i_lat < 1 ) i_lat = 1
            IF ( i_lat > Grid%NLines ) i_lat = Grid%NLines
            DO ii = 1,4
              i_lon = PX%FMMap%LonLo - 2 + ii
              IF ( i_lon < 1 ) THEN
                IF ( Grid%Full_Globe ) THEN
                  i_lon = Grid%NElems + i_lon
                ELSE
                  i_lon = 1
                END IF
              ELSE IF ( i_lon > Grid%NElems ) THEN
                IF ( Grid%Full_Globe ) THEN
                  i_lon = i_lon - Grid%NElems
                ELSE
                  i_lon = Grid%NElems
                END IF
              END IF
              IF ( SurfType( i_lon , i_lat ) == PX%Surface_Type ) NCnt( ii , jj ) = 1
            END DO
          END DO

          SCnt(1) = SUM( NCnt(1:2,1:2) )
          SCnt(2) = SUM( NCnt(2:3,1:2) )
          SCnt(3) = SUM( NCnt(3:4,1:2) )
          SCnt(4) = SUM( NCnt(1:2,2:3) )
          SCnt(5) = SUM( NCnt(2:3,2:3) )
          SCnt(6) = SUM( NCnt(3:4,2:3) )
          SCnt(7) = SUM( NCnt(1:2,3:4) )
          SCnt(8) = SUM( NCnt(2:3,3:4) )
          SCnt(9) = SUM( NCnt(3:4,3:4) )

          jj = 5
          DO ii = 1 , 6
            IF ( SCnt(ii) > SCnt(jj) ) THEN
              jj = ii
            END IF
          END DO

          SELECT CASE ( jj ) 

            CASE ( 1 )

              PX%FMMap%LonLo = PX%FMMap%LonLo - 1
              PX%FMMap%LatLo = PX%FMMap%LatLo - 1

              Lon_Scale = 2
              Lat_Scale = 2

            CASE ( 2 )

              PX%FMMap%LatLo = PX%FMMap%LatLo - 1

              Lat_Scale = 2

            CASE ( 3 )

              PX%FMMap%LonHi = PX%FMMap%LonHi + 1
              PX%FMMap%LatLo = PX%FMMap%LatLo - 1

              Lon_Scale = 2
              Lat_Scale = 2

            CASE ( 4 )

              PX%FMMap%LonLo = PX%FMMap%LonLo - 1

              Lon_Scale = 2

            CASE ( 5 )

              ! No change 

            CASE ( 6 )

              PX%FMMap%LonHi = PX%FMMap%LonHi + 1

              Lon_Scale = 2

            CASE ( 7 )

              PX%FMMap%LonLo = PX%FMMap%LonLo - 1
              PX%FMMap%LatHi = PX%FMMap%LatHi + 1

              Lon_Scale = 2
              Lat_Scale = 2

            CASE ( 8 )

              PX%FMMap%LatHi = PX%FMMap%LatHi + 1

              Lat_Scale = 2

            CASE ( 9 )

              PX%FMMap%LonHi = PX%FMMap%LonHi + 1
              PX%FMMap%LatHi = PX%FMMap%LatHi + 1

              Lon_Scale = 2
              Lat_Scale = 2

          END SELECT

        END IF

      END IF

      ModelLon = offlon + ( PX%FMMap%LonLo - 1 ) * Grid%Incr%lon 
      ModelLat = offlat + ( PX%FMMap%LatLo - 1 ) * Grid%Incr%lat 

      PX%FMMap%LonWgt = ( PX%EarthCoord%lon + offcyclic - ModelLon ) / ( Grid%Incr%lon * Lon_Scale )
      PX%FMMap%LatWgt = ( PX%EarthCoord%lat - ModelLat ) / ( Grid%Incr%lat * Lat_Scale )

      IF ( PX%FMMap%LonLo < 1 ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonLo = Grid%NElems
        ELSE
          PX%FMMap%LonLo = 1
        END IF
      ELSE IF ( PX%FMMap%LonLo > Grid%NELems ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonLo = 1
        ELSE
          PX%FMMap%LonLo = Grid%NELems
        END IF
      END IF

      IF ( PX%FMMap%LonHi < 1 ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonHi = Grid%NElems
        ELSE
          PX%FMMap%LonHi = 1
        END IF
      ELSE IF ( PX%FMMap%LonHi > Grid%NELems ) THEN
        IF ( Grid%Full_Globe ) THEN
          PX%FMMap%LonHi = 1
        ELSE
          PX%FMMap%LonHi = Grid%NElems
        END IF
      END IF

      IF ( PX%FMMap%LatLo < 1 ) THEN
        PX%FMMap%LatLo = 1
      ELSE IF ( PX%FMMap%LatLo > Grid%NLines ) THEN
        PX%FMMap%LatLo = Grid%NLines
      END IF

      IF ( PX%FMMap%LatHi < 1 ) THEN
        PX%FMMap%LatHi = 1
      ELSE IF ( PX%FMMap%LatHi > Grid%NLines ) THEN
        PX%FMMap%LatHi = Grid%NLines
      END IF

    ! -- Only use the surrounding profile which are the same
    !    surface type as the current pixel.
    MCnt(:) = 0
    IF (SurfType(PX%FMMap%LonLo,PX%FMMap%LatLo) == PX%Surface_Type) Mcnt(1) = 1
    IF (SurfType(PX%FMMap%LonHi,PX%FMMap%LatLo) == PX%Surface_Type) Mcnt(2) = 1
    IF (SurfType(PX%FMMap%LonHi,PX%FMMap%LatHi) == PX%Surface_Type) Mcnt(3) = 1
    IF (SurfType(PX%FMMap%LonLo,PX%FMMap%LatHi) == PX%Surface_Type) Mcnt(4) = 1

    CALL Calc_Interpol_Weights( PX, MCnt )

  END SUBROUTINE ModelIndices


  SUBROUTINE ModelIndices_ImageCoords(PX,Grid,SurfType)

  !
  ! Description:
  !
  !  Subroutine for finding the indices of the nearest model grid points to the current
  !  image pixel.
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   07/03/2005    Creation                                                   CPO
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

  ! Modules used:

    USE GbcsKinds
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

  ! Define Input Variabiles

    TYPE(ImagePixel),                  INTENT(INOUT) :: PX
    TYPE(Grid_Descriptor),             INTENT(IN)    :: Grid
    INTEGER,DIMENSION(:,:), OPTIONAL,  INTENT(IN)    :: SurfType

  ! Define Local Variabiles

    INTEGER :: offline, offelem
    INTEGER :: ModelLine, ModelElem
    INTEGER :: ii
    INTEGER, DIMENSION(4) :: MCnt

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'ModelIndices_ImageCoords'

    IF (Grid%NElems==1 .AND. Grid%NLines==1) THEN
      ! -- Only one grid point available (MDB reprocessing)
      PX%FMMap%LatLo = 1
      PX%FMMap%LatHi = 1
      PX%FMMap%LonLo = 1
      PX%FMMap%LonHi = 1
      PX%FMMap%LatWgt = 1.0
      PX%FMMap%LonWgt = 1.0
      DO ii=1, 4
        PX%LocMP(ii)%line   = 1
        PX%LocMP(ii)%elem   = 1
        PX%LocMP(ii)%weight = 0.0
      END DO
      PX%LocMP(1)%weight = 1.0
      PX%profiles_do_not_match = .FALSE.
      RETURN
    END IF
    
      offline = Grid%I_LLCrnr%line
      offelem = Grid%I_LLCrnr%elem

      PX%FMMap%LatLo = FLOOR( REAL( PX%ImageCoord%line + PX%ImageCoordOffset%line - offline , KIND=GbcsReal ) / &
                              REAL( Grid%I_Incr%line , KIND=GbcsReal) ) + 1
      PX%FMMap%LatHi = PX%FMMap%LatLo + 1
      IF ( PX%FMMap%LatHi > Grid%NLines ) PX%FMMap%LatHi = Grid%Nlines
      PX%FMMap%LonLo = FLOOR( REAL( PX%ImageCoord%elem + PX%ImageCoordOffset%elem - offelem , KIND=GbcsReal ) / &
                              REAL( Grid%I_Incr%elem , KIND=GbcsReal ) ) + 1
      PX%FMMap%LonHi = PX%FMMap%LonLo + 1
      IF ( PX%FMMap%LonHi > Grid%NElems ) PX%FMMap%LonHi = Grid%NElems
      IF ( PX%FMMap%LonLo < 1 ) PX%FMMap%LonLo = 1

      ModelLine = offline + ( (PX%FMMap%LatLo - 1) * Grid%I_Incr%line )
      ModelElem = offelem + ( (PX%FMMap%LonLo - 1) * Grid%I_Incr%elem )

      PX%FMMap%LatWgt = REAL( PX%ImageCoord%line + PX%ImageCoordOffset%line - ModelLine , KIND=GbcsReal ) / & 
                        REAL( Grid%I_Incr%line , KIND=GbcsReal )
      PX%FMMap%LonWgt = REAL( PX%ImageCoord%elem + PX%ImageCoordOffset%elem - ModelElem , KIND=GbcsReal ) / & 
                        REAL( Grid%I_Incr%elem , KIND=GbcsReal )

      IF ( PX%FMMap%LatLo < 1 ) THEN
        PX%FMMap%LatLo = 1
      END IF

      IF ( PX%FMMap%LatLo > Grid%NLines ) THEN
        PX%FMMap%LatLo = Grid%NLines
      END IF

      IF ( PX%FMMap%LatHi < 1 ) THEN
        PX%FMMap%LatHi = 1
      END IF

      IF ( PX%FMMap%LatHi > Grid%NLines ) THEN
        PX%FMMap%LatHi = Grid%NLines
      END IF

      IF ( PX%FMMap%LonLo < 1 ) THEN
        PX%FMMap%LonLo = 1
      END IF

      IF ( PX%FMMap%LonLo > Grid%NELems ) THEN
        PX%FMMap%LonLo = Grid%NELems
      END IF

      IF ( PX%FMMap%LonHi < 1 ) THEN
        PX%FMMap%LonHi = 1
      END IF

      IF ( PX%FMMap%LonHi > Grid%NELems ) THEN
        PX%FMMap%LonHi = Grid%NElems
      END IF

    ! -- Only use the surrounding profile which are the same
    !    surface type as the current pixel.
    IF (PRESENT(SurfType)) THEN
      MCnt(:) = 0
      IF (SurfType(PX%FMMap%LonLo,PX%FMMap%LatLo) == PX%Surface_Type) Mcnt(1) = 1
      IF (SurfType(PX%FMMap%LonHi,PX%FMMap%LatLo) == PX%Surface_Type) Mcnt(2) = 1
      IF (SurfType(PX%FMMap%LonHi,PX%FMMap%LatHi) == PX%Surface_Type) Mcnt(3) = 1
      IF (SurfType(PX%FMMap%LonLo,PX%FMMap%LatHi) == PX%Surface_Type) Mcnt(4) = 1
    ELSE
      MCnt(:) = 1
    END IF

    CALL Calc_Interpol_Weights( PX, MCnt )

  END SUBROUTINE ModelIndices_ImageCoords


!------------------------------------------------------------------------------
!S+
! NAME:
!       ModelIndices_GGrid(PX,Grid,SurfType)
!
! PURPOSE:
!  Subroutine for finding the indices of the nearest model grid points to the current
!  image pixel for Gaussian Grid data.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL ModelIndices_GGrid( PX, Grid, SurfType )
!
! INPUT ARGUMENTS:
!       PX
!       Grid
!       SurfType
!
! OUTPUT ARGUMENTS:
!       Modifies PX structure.
!       Nearest profile locations and interpolation weights
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
!       Written by:   Chris Old 06/11/2012
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE ModelIndices_GGRID(PX,Grid,Lat,SurfType)

  ! Modules used:

    USE GbcsKinds
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

  ! Define Input Variabiles

    TYPE(ImagePixel),                  INTENT(INOUT) :: PX
    TYPE(Grid_Descriptor),             INTENT(IN   ) :: Grid
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT(IN   ) :: Lat
    INTEGER,DIMENSION(:,:), OPTIONAL,  INTENT(IN   ) :: SurfType

  ! Define Local Variabiles

    REAL(Kind=GbcsReal) :: offlat, offlon
    REAL(Kind=GbcsReal) :: offcyclic
    REAL(Kind=GbcsReal) :: ModelLat, ModelLon
    INTEGER :: iLat
    INTEGER, DIMENSION(4) :: MCnt

    INTEGER :: n
    INTEGER :: Lat_Scale , Lon_Scale
    REAL(KIND=GbcsReal) :: Lat_Inc

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'ModelIndices_GGrid'

    N = SIZE(Lat)
    IF ( Lat(1) .GT. 0 ) THEN
      Lat_Inc = Lat(1) - 90.0
    ELSE
      Lat_Inc = 90.0 + Lat(1)
    END IF

    ! Set lower lefthand corner offset
    SELECT CASE (Grid%DataLoc)

      CASE (Gbcs_At_Corner)

        offlat = Grid%LLCrnr%lat
        offlon = Grid%LLCrnr%lon

      CASE (Gbcs_At_Centre)

        offlat = Grid%LLCrnr%lat + Lat_Inc
        offlon = Grid%LLCrnr%lon + ( Grid%Incr%lon / 2.0 )

    END SELECT

    ! Shift to 0 to 360 longitude range
    offcyclic = 0.0
    IF ( PX%EarthCoord%lon < Grid%LLCrnr%lon ) THEN
      offcyclic = 360.0
    END IF

    ! Identify the four model points surrounding the current pixel location
    PX%FMMap%LonLo = FLOOR((PX%EarthCoord%lon + offcyclic - offlon)/Grid%Incr%lon) + 1
    IF ( PX%FMMap%LonLo < 1 ) THEN
      IF ( Grid%Full_Globe ) THEN
        PX%FMMap%LonLo = Grid%NElems
      ELSE
        PX%FMMap%LonLo = 1
      END IF
    ELSE IF ( PX%FMMap%LonLo > Grid%NElems ) THEN
      IF ( Grid%Full_Globe ) THEN
        PX%FMMap%LonLo = 1
      ELSE
        PX%FMMap%LonLo = Grid%NElems
      END IF
    END IF
    PX%FMMap%LonHi = PX%FMMap%LonLo + 1
    IF ( PX%FMMap%LonHi > Grid%NElems ) THEN
      IF ( Grid%Full_Globe ) THEN
        PX%FMMap%LonHi = 1
      ELSE
        PX%FMMap%LonHi = Grid%NElems
      END IF
    END IF

    ! Find latitude bin using linear search
    IF ( Lat(N) - Lat(1) > 0 ) THEN
      DO iLat = 1,N
        IF ( PX%EarthCoord%lat < Lat(iLat) ) EXIT
      ENDDO
    ELSE
      DO iLat = 1,N
        IF ( PX%EarthCoord%lat > Lat(iLat) ) EXIT
      ENDDO
    END IF

    PX%FMMap%LatLo = iLat - 1
    IF ( PX%FMMap%LatLo < 1 ) THEN 
      PX%FMMap%LatLo = 1
    ELSE IF ( PX%FMMap%LatLo > Grid%NLines ) THEN 
      PX%FMMap%LatLo = Grid%NLines
    END IF
    PX%FMMap%LatHi = PX%FMMap%LatLo + 1
    IF ( PX%FMMap%LatHi > Grid%NLines ) PX%FMMap%LatHi = Grid%NLines

    Lon_Scale = 1
    Lat_Scale = 1

    !********************************************
    ! Skip surface type check for initial testing
    !********************************************

    ! Calculate lat and lon weighting factors
    ModelLon = offlon + ( PX%FMMap%LonLo - 1 ) * Grid%Incr%lon 
    ModelLat = Lat( PX%FMMap%LatLo )

!    PX%FMMap%LonWgt = ( PX%EarthCoord%lon + offcyclic - ModelLon ) / ( Grid%Incr%lon * Lon_Scale )
    PX%FMMap%LonWgt = modulo( ( PX%EarthCoord%lon + offcyclic - ModelLon ) , 360.0 ) / ( Grid%Incr%lon * Lon_Scale )
    IF ( PX%FMMap%LatHi .NE. PX%FMMap%LatLo ) THEN
      PX%FMMap%LatWgt = ( PX%EarthCoord%lat - ModelLat ) / ( Lat( PX%FMMap%LatHi ) - Lat( PX%FMMap%LatLo ) )
    ELSE
      IF ( PX%FMMap%LatHi .EQ. 1 ) THEN
        PX%FMMap%LatWgt = ( PX%EarthCoord%lat - ModelLat ) / ( Lat( PX%FMMap%LatHi + 1 ) - Lat( PX%FMMap%LatLo ) )
      ELSE
        PX%FMMap%LatWgt = ( PX%EarthCoord%lat - ModelLat ) / ( Lat( PX%FMMap%LatHi ) - Lat( PX%FMMap%LatLo - 1 ) )
      END IF
    END IF

    ! Set profiles to use.
    ! Only use the surrounding profile which are the same surface type as the current pixel.
    IF (PRESENT(SurfType)) THEN
      MCnt(:) = 0
      IF (SurfType(PX%FMMap%LonLo,PX%FMMap%LatLo) == PX%Surface_Type) Mcnt(1) = 1
      IF (SurfType(PX%FMMap%LonHi,PX%FMMap%LatLo) == PX%Surface_Type) Mcnt(2) = 1
      IF (SurfType(PX%FMMap%LonHi,PX%FMMap%LatHi) == PX%Surface_Type) Mcnt(3) = 1
      IF (SurfType(PX%FMMap%LonLo,PX%FMMap%LatHi) == PX%Surface_Type) Mcnt(4) = 1
    ELSE
      MCnt(:) = 1
    END IF

    ! Calculate interpolation weighting factors based on selection of profiles
    CALL Calc_Interpol_Weights( PX, MCnt )
        
  END SUBROUTINE ModelIndices_GGrid


!------------------------------------------------------------------------------
!S+
! NAME:
!       Calc_Interpol_Weights
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
!       CALL Calc_Interpol_Weights( PX, use_map )
!
! INPUT ARGUMENTS:
!       SAT
!
! OUTPUT ARGUMENTS:
!       File_Name
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
!       Written by:   Owen Embury 23/07/2008
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Calc_Interpol_Weights( PX, use_map )
    USE GbcsTypes, ONLY: ImagePixel
    USE GbcsErrorHandler
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel),               INTENT(INOUT) :: PX
    INTEGER,          DIMENSION(4), INTENT(IN)    :: use_map

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Calc_Interpol_Weights'



    PX%LocMP(1)%line = PX%FMMap%LatLo
    PX%LocMP(1)%elem = PX%FMMap%LonLo
    PX%LocMP(1)%weight = 0.0

    PX%LocMP(2)%line = PX%FMMap%LatLo
    PX%LocMP(2)%elem = PX%FMMap%LonHi
    PX%LocMP(2)%weight = 0.0

    PX%LocMP(3)%line = PX%FMMap%LatHi
    PX%LocMP(3)%elem = PX%FMMap%LonHi
    PX%LocMP(3)%weight = 0.0

    PX%LocMP(4)%line = PX%FMMap%LatHi
    PX%LocMP(4)%elem = PX%FMMap%LonLo
    PX%LocMP(4)%weight = 0.0

    PX%profiles_do_not_match = .FALSE.

    ! -- Calculate interpolation weights
    SELECT CASE( SUM((/1, 2, 4, 8/) * use_map) )
    ! -- Use all neighbours
    CASE(15)
      PX%LocMP(1)%weight = (1.0 - PX%FMMap%LonWgt) * (1.0 - PX%FMMap%LatWgt)
      PX%LocMP(2)%weight =        PX%FMMap%LonWgt  * (1.0 - PX%FMMap%LatWgt)
      PX%LocMP(3)%weight =        PX%FMMap%LonWgt  *        PX%FMMap%LatWgt 
      PX%LocMP(4)%weight = (1.0 - PX%FMMap%LonWgt) *        PX%FMMap%LatWgt 

    ! -- Use all neighbours (but none of them match)
    CASE(0)
      PX%LocMP(1)%weight = (1.0 - PX%FMMap%LonWgt) * (1.0 - PX%FMMap%LatWgt)
      PX%LocMP(2)%weight =        PX%FMMap%LonWgt  * (1.0 - PX%FMMap%LatWgt)
      PX%LocMP(3)%weight =        PX%FMMap%LonWgt  *        PX%FMMap%LatWgt 
      PX%LocMP(4)%weight = (1.0 - PX%FMMap%LonWgt) *        PX%FMMap%LatWgt 
      PX%profiles_do_not_match = .TRUE.

    ! -- Use one point
    CASE(1)
      PX%LocMP(1)%weight = 1.0
    CASE(2)
      PX%LocMP(2)%weight = 1.0
    CASE(4)
      PX%LocMP(3)%weight = 1.0
    CASE(8)
      PX%LocMP(4)%weight = 1.0

    ! -- Two point
    CASE(3)
      PX%LocMP(1)%weight = (1.0 - PX%FMMap%LonWgt)
      PX%LocMP(2)%weight =        PX%FMMap%LonWgt 
    CASE(5)
      PX%LocMP(1)%weight = 1.0 - (PX%FMMap%LonWgt + PX%FMMap%LatWgt) / 2.0
      PX%LocMP(3)%weight =       (PX%FMMap%LonWgt + PX%FMMap%LatWgt) / 2.0
    CASE(9)
      PX%LocMP(1)%weight =                           (1.0 - PX%FMMap%LatWgt)
      PX%LocMP(4)%weight =                                  PX%FMMap%LatWgt 
    CASE(6)
      PX%LocMP(2)%weight =                           (1.0 - PX%FMMap%LatWgt)
      PX%LocMP(3)%weight =                                  PX%FMMap%LatWgt 
    CASE(10)
      PX%LocMP(2)%weight =       (PX%FMMap%LonWgt + (1-PX%FMMap%LatWgt)) / 2.0
      PX%LocMP(4)%weight = 1.0 - (PX%FMMap%LonWgt + (1-PX%FMMap%LatWgt)) / 2.0
    CASE(12)
      PX%LocMP(3)%weight =        PX%FMMap%LonWgt
      PX%LocMP(4)%weight = (1.0 - PX%FMMap%LonWgt)

    ! -- Three point
    CASE(7)
      PX%LocMP(1)%weight = 1.0 - PX%FMMap%LonWgt
      PX%LocMP(2)%weight = PX%FMMap%LonWgt - PX%FMMap%LatWgt
      PX%LocMP(3)%weight = PX%FMMap%LatWgt 
    CASE(11)
      PX%LocMP(1)%weight = 1.0 - PX%FMMap%LonWgt - PX%FMMap%LatWgt
      PX%LocMP(2)%weight = PX%FMMap%LonWgt
      PX%LocMP(4)%weight = PX%FMMap%LatWgt 
    CASE(13)
      PX%LocMP(1)%weight = 1.0 - PX%FMMap%LatWgt
      PX%LocMP(3)%weight = PX%FMMap%LonWgt
      PX%LocMP(4)%weight = PX%FMMap%LatWgt - PX%FMMap%LonWgt
    CASE(14)
      PX%LocMP(2)%weight = 1.0 - PX%FMMap%LatWgt
      PX%LocMP(3)%weight = PX%FMMap%LonWgt + PX%FMMap%LatWgt - 1.0
      PX%LocMP(4)%weight = 1.0 - PX%FMMap%LonWgt
    CASE DEFAULT
      CALL Gbcs_Critical(.TRUE.,'should not be here', ROUTINE_NAME, MODULE_NAME)
    END SELECT

  END SUBROUTINE Calc_Interpol_Weights
  
  
  SUBROUTINE Linear_Interp( X1 , Y1 , X2 , Y2 )

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
  ! 0.0   09/11/2005    Creation                                                   CPO
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

    USE GbcsKinds
    USE GbcsErrorHandler, ONLY: Gbcs_Log_File_Unit

    IMPLICIT NONE

    REAL(KIND=GbcsReal), DIMENSION(:), INTENT (IN   ) :: X1
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT (IN   ) :: Y1
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT (IN   ) :: X2
    REAL(KIND=GbcsReal), DIMENSION(:), INTENT (  OUT) :: Y2

    INTEGER      :: npts1
    INTEGER      :: npts2
    INTEGER      :: ii , jj
    REAL(KIND=GbcsReal) :: dh 
    INTEGER :: STAT
    INTEGER :: nsteps 

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Linear_Interp'

      npts1 = SIZE(X1)
      npts2 = SIZE(X2)
      
      ! Only allocate dx if necessary - stops multiple allocations
      ! Allocate with 100 in first and go from there
      if( linear_dx_size .lt. npts1 )then
         nsteps = npts1/100 + 1
         if( linear_dx_step .lt. nsteps )then
            if( ALLOCATED(linear_dx) )then
               DEALLOCATE(linear_dx)
            endif
            linear_dx_step = nsteps
            linear_dx_size = linear_dx_step * 100
            ALLOCATE( linear_dx( linear_dx_size ) , STAT=STAT)
            if( 0 /= STAT )then
               WRITE(Gbcs_Log_File_Unit,*)' ERROR : Cannot allocate linear_dx (Linear_Interp)'
               stop
            endif
         endif
      endif

      linear_dx(1:npts1-1) = X1(2:npts1) - X1(1:npts1-1)

      DO ii = 1,npts1-1
        IF ( ABS( linear_dx(ii) ) < 1.0e-30 ) THEN
          WRITE(Gbcs_Log_File_Unit,*)' ERROR : dx too small'
          WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Linear_Interp'
          WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsInterpolators'
          WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_Maths/GbcsInterpolators.f90'
          STOP
        END IF
      END DO

      DO jj = 1,npts2

        IF ( linear_dx(1) > 0 ) THEN         ! Monotonically increasing

          ii = 1
          DO WHILE( ( X2( jj ) > X1( ii ) ) .AND. ( ii < npts1 ) )
            ii = ii + 1
          END DO
          ii = ii - 1

          IF ( ii == 0 )     ii = 1              ! Outside lower bound
          IF ( ii == npts1 ) ii = npts1 - 1      ! Outside upper bound
          
        ELSE IF ( linear_dx(1) < 0 ) THEN    ! Monotonically decreasing

          ii = 1
          DO WHILE( ( X2( jj ) < X1( ii ) ) .AND. ( ii < npts1 ) )
            ii = ii + 1
          END DO
          ii = ii - 1

          IF ( ii == 0 )     ii = 1              ! Outside upper bound
          IF ( ii == npts1 ) ii = npts1 - 1      ! Outside lower bound
          
        END IF

        dh = ( X2(jj) - X1(ii) ) / linear_dx(ii)

        Y2(jj) = ( 1.0 - dh ) * Y1(ii) + dh * Y1(ii+1)

      END DO

  END SUBROUTINE Linear_Interp


  REAL(KIND=GbcsReal) FUNCTION Interpolate(mnm,dlon,dlat,v4)

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
  ! 0.0   19/07/2004    Creation                                                   CPO
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

  ! Modules used:

    USE GbcsKinds

    IMPLICIT NONE

    INTEGER             :: mnm(4)
    REAL(KIND=GbcsReal) :: dlon,dlat
    REAL(KIND=GbcsReal) :: v4(4)
    REAL(KIND=GbcsReal) :: v2(2),v3(3)
    REAL(KIND=GbcsReal) :: dh
    INTEGER             :: nm,nc
    INTEGER             :: cnum(4)

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Interpolate'

    DATA cnum /1,2,3,4/

      nm = sum(mnm)

      IF (nm == 0) THEN           ! 4 Point Interpolation

        Interpolate = Interp4Pt(dlon,dlat,v4)

      ELSE IF (nm == 1) THEN       ! 3 Point Interpolation

        nc = sum(cnum*(1-mnm))    ! Sum corner values not over land

        SELECT CASE (nc)

          CASE(6)

            v3(1) = v4(1)                                        ! O----X
            v3(2) = v4(2)                                        ! |    |
            v3(3) = v4(3)                                        ! |    |
            Interpolate = Interp3Pt(dlon,dlat,v3)                ! X----X    1+2+3+0=6

          CASE(7)

            v3(1) = v4(2)                                        ! X----O
            v3(2) = v4(1)                                        ! |    |
            v3(3) = v4(4)                                        ! |    |
            Interpolate = Interp3Pt((1.0-dlon),dlat,v3)          ! X----X    1+2+0+4=7

          CASE(8)

            v3(1) = v4(3)                                        ! X----X
            v3(2) = v4(4)                                        ! |    |
            v3(3) = v4(1)                                        ! |    |
            Interpolate = Interp3Pt((1.0-dlon),(1.0-dlat),v3)    ! X----O    1+0+3+4=8

          CASE(9)

            v3(1) = v4(4)                                        ! X----X
            v3(2) = v4(3)                                        ! |    | 
            v3(3) = v4(2)                                        ! |    |
            Interpolate = Interp3Pt(dlon,(1.0-dlat),v3)          ! O----X    0+2+3+4=9

        END SELECT

      ELSE IF (nm == 2) THEN       ! 2 Point Interpolation

        nc = sum(cnum*(1-mnm))    ! Sum corner values not over land

        SELECT CASE (nc)

          CASE (3)

            v2(1) = v4(1)                                        ! O----O
            v2(2) = v4(2)                                        ! |    |
            Interpolate = Interp2Pt(dlon,v2)                     ! |    |
                                                                 ! X----X    1+2+0+0=3
          CASE (4)

            v2(1) = v4(1)                                        ! O----X
            v2(2) = v4(3)                                        ! |    |
            dh = (dlon+dlat)/2.0_GbcsReal                        ! |    |
            Interpolate = Interp2Pt(dh,v2)                       ! X----O    1+0+3+0=4

          CASE (5)
                                                                 ! X----O
            IF (mnm(1) == 0) THEN                                ! |    |
              v2(1) = v4(1)                                      ! |    |
              v2(2) = v4(4)                                      ! X----O    1+0+0+4=5
            ELSE
              v2(1) = v4(2)                                      ! O----X
              v2(2) = v4(3)                                      ! |    |
            END IF                                               ! |    |
            Interpolate = Interp2Pt(dlat,v2)                     ! O----X    0+2+3+0=5

          CASE (6)

            v2(1) = v4(2)                                        ! X----O
            v2(2) = v4(4)                                        ! |    | 
            dh = (dlon+dlat)/2.0_GbcsReal                        ! |    |
            Interpolate = Interp2Pt(dh,v2)                       ! O----X    0+2+0+4=6

          CASE (7)
                                                                 ! X----X
            v2(1) = v4(4)                                        ! |    |
            v2(2) = v4(3)                                        ! |    |
            Interpolate = Interp2Pt(dlon,v2)                     ! O----O    0+0+3+4=7

        END SELECT

      ELSE IF (nm == 3) THEN       ! 1 Point = value in active grid

        nc = sum(cnum*(1-mnm))    ! Sum corner values not over land

        Interpolate = v4(nc)

      ELSE                        ! 0 Points = Land cell

        Interpolate = 0.0_GbcsReal

      END IF

    RETURN

  END FUNCTION Interpolate



  REAL(KIND=GbcsReal) FUNCTION Interp2Pt(dh,v)

  !
  ! Description:
  !
  !  Two Point Linear interpolation
  !
  ! Method:
  !
  !  dh is fractional distance of p from point 1 along the line joining
  !  points 1 and 2.
  !
  !    1            2
  !     x----p----x
  !     -----
  !      dh
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   19/07/2004    Creation                                                   CPO
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

  ! Modules used:

    USE GbcsKinds

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: dh,v(2)

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Interp2Pt'

      Interp2Pt = (1.0-dh)*v(1)+dh*v(2)

    RETURN

  END FUNCTION Interp2Pt



  REAL(KIND=GbcsReal) FUNCTION Interp3Pt(dx,dy,v)

  !
  ! Description:
  !
  !  Three Point Interpolation
  !
  ! Method:
  !
  !  dh is fractional distance of p from point 1 along the line joining
  !  points 1 and 2.
  !
  !  Assumes data at locations X [1,2,3]
  !
  !     4        3
  !      O------X
  !      |      |
  !      |--p   |
  !   dy |  |   |
  !      X------X
  !     1 dx     2
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   19/07/2004    Creation                                                   CPO
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

  ! Modules used:

    USE GbcsKinds

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: dx,dy,v(3)

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Interp3Pt'

      Interp3Pt = (1.0-dx)*v(1)+(dx-dy)*v(2)+dy*v(3)

    RETURN

  END FUNCTION Interp3Pt



  REAL(KIND=GbcsReal) FUNCTION Interp4Pt(dx,dy,v)

  !
  ! Description:
  !
  !  Four Point Interpolation
  !
  ! Method:
  !
  !  dh is fractional distance of p from point 1 along the line joining
  !  points 1 and 2.
  !
  !  Assumes data at locations X [1,2,3]
  !
  !     4        3
  !      X------X
  !      |      |
  !      |--p   |
  !   dy |  |   |
  !      X------X
  !     1 dx     2
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   19/07/2004    Creation                                                   CPO
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

  ! Modules used:

    USE GbcsKinds

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: dx,dy,v(4)

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Interp4Pt'

      Interp4Pt = (1.0-dx)*(1.0-dy)*v(1)+(1.0-dy)*dx*v(2)+dx*dy*v(3)+(1.0-dx)*dy*v(4)

    RETURN


  END FUNCTION Interp4Pt


!------------------------------------------------------------------------------
!F+
! NAME:
!       InterpolateField2
!
! PURPOSE:
!       Interpolates a 2-D field using the supplied weights and model points.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       value = InterpolateField2( field, WgtMP )
!
! INPUT ARGUMENTS:
!       field   REAL 2-D array to interpolate
!       WgtMP   Model points and weights for interpolation
!
! FUNCTION RESULT:
!       Interpolated value
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
!       Written by:   Owen Embury 11/07/2007
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION InterpolateField2( field, WgtMP ) &
                     RESULT( value )
    USE GbcsTypes
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(:,:), INTENT(IN) :: field
    TYPE(WeightMP),      DIMENSION(4),   INTENT(IN) :: WgtMP
    
    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal)                             :: value


    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME        = 'InterpolateField2'

    ! ---------------
    ! Local variables
    ! ---------------
    
    value = WgtMP(1)%weight * field(WgtMP(1)%Elem,WgtMP(1)%Line) + &
            WgtMP(2)%weight * field(WgtMP(2)%Elem,WgtMP(2)%Line) + &
            WgtMP(3)%weight * field(WgtMP(3)%Elem,WgtMP(3)%Line) + &
            WgtMP(4)%weight * field(WgtMP(4)%Elem,WgtMP(4)%Line)

  END FUNCTION InterpolateField2

!------------------------------------------------------------------------------
!F+
! NAME:
!       InterpolateField3
!
! PURPOSE:
!       Interpolates a 1-D field using the supplied weights and model points.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       value = InterpolateField( field, WgtMP )
!
! INPUT ARGUMENTS:
!       field   REAL 3-D array to interpolate
!       WgtMP   Model points and weights for interpolation
!
! FUNCTION RESULT:
!       Array of interpolated values
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
!       Written by:   Owen Embury 11/07/2007
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION InterpolateField3( field, WgtMP ) &
                     RESULT( value )
    USE GbcsTypes
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(:,:,:), INTENT(IN)         :: field
    TYPE(WeightMP),      DIMENSION(4),   INTENT(IN)           :: WgtMP
    
    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(SIZE(field,1)) :: value


    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME        = 'InterpolateField4'

    ! ---------------
    ! Local variables
    ! ---------------
    
    value = WgtMP(1)%weight * field(:,WgtMP(1)%Elem,WgtMP(1)%Line) + &
            WgtMP(2)%weight * field(:,WgtMP(2)%Elem,WgtMP(2)%Line) + &
            WgtMP(3)%weight * field(:,WgtMP(3)%Elem,WgtMP(3)%Line) + &
            WgtMP(4)%weight * field(:,WgtMP(4)%Elem,WgtMP(4)%Line)

  END FUNCTION InterpolateField3

!------------------------------------------------------------------------------
!F+
! NAME:
!       InterpolateField4
!
! PURPOSE:
!       Interpolates a 2-D field using the supplied weights and model points.
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       value = InterpolateField( field, WgtMP )
!
! INPUT ARGUMENTS:
!       field   REAL 4-D array to interpolate
!       WgtMP   Model points and weights for interpolation
!
! FUNCTION RESULT:
!       Array of interpolated values
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
!       Written by:   Owen Embury 11/07/2007
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION InterpolateField4( field, WgtMP ) &
                     RESULT( value )
    USE GbcsTypes
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(:,:,:,:), INTENT(IN)         :: field
    TYPE(WeightMP),      DIMENSION(4),   INTENT(IN)             :: WgtMP
    
    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal), DIMENSION(SIZE(field,1),SIZE(field,2)) :: value


    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME        = 'InterpolateField4'

    ! ---------------
    ! Local variables
    ! ---------------
    
    value = WgtMP(1)%weight * field(:,:,WgtMP(1)%Elem,WgtMP(1)%Line) + &
            WgtMP(2)%weight * field(:,:,WgtMP(2)%Elem,WgtMP(2)%Line) + &
            WgtMP(3)%weight * field(:,:,WgtMP(3)%Elem,WgtMP(3)%Line) + &
            WgtMP(4)%weight * field(:,:,WgtMP(4)%Elem,WgtMP(4)%Line)

  END FUNCTION InterpolateField4


!------------------------------------------------------------------------------
!F+
! NAME:
!       InterpolateFlags
!
! PURPOSE:
!       Checks the qc flags for the given model points
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       value = InterpolateFlags( Model, WgtMP )
!
! INPUT ARGUMENTS:
!       model   Model structure to check
!       WgtMP   Model points and weights for interpolation
!
! FUNCTION RESULT:
!       Bitwise OR of model flags
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
!       Written by:   Owen Embury 08/04/2010
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION InterpolateFlags( Model, WgtMP ) &
                     RESULT( value )
    USE GbcsTypes
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model),         INTENT(IN) :: Model
    TYPE(WeightMP), DIMENSION(4), INTENT(IN) :: WgtMP
    
    ! ---------
    ! Function result
    ! ---------
    INTEGER :: value

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME        = 'InterpolateFlags'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i
    
    value = 0
    
    IF (ALLOCATED(Model%Flags)) THEN
      DO i=1, 4
        !IF (WgtMP(i)%weight == 0.0) CYCLE
        value = IOR(value, Model%Flags(WgtMP(i)%Elem,WgtMP(i)%Line))
      END DO
    END IF
    
  END FUNCTION InterpolateFlags
  
  
END MODULE GbcsInterpolators

