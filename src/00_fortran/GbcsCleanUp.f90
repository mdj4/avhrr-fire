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



!+ This module contains clean up procedures and functions

MODULE GbcsCleanUp

!
! Description:
!
!  Contains the functions used to deallocate all allocated data structures
!
! Method:
!
! Owner: Manager of TRICS Project
!
! History:
! Version  Date       Comment
! -------  ----       -------
! 0.0   22/02/2005    Creation                                                   CPO
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
  USE GbcsErrorHandler

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: Clean_Up,Release_Auxillary,Release_Imagery
  
  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_CleanUp/GbcsCleanUp.f90'
  

CONTAINS


  ! This function releases all space allocated by process

  SUBROUTINE Clean_Up( SAT , IMG , AUX , RTM )

  !
  ! Description:
  !
  !  Releases all allocated data structures
  !
  ! Method:
  !
  !  Each structre is cleaned by calling 
  !
  !    FUNCTION Release_Satellite( SAT )
  !    FUNCTION Release_Imagery( IMG )
  !    FUNCTION Release_Auxillary( AUX )
  !    FUNCTION Release_RTM( RTM )
  !
  !  in this file
  !
  !  Each function returns a status value 
  !  A non-zero value means there was an error releasing one of the structures
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   24/02/2005    Creation                                                   CPO
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

    USE GbcsTypes

    IMPLICIT NONE

    TYPE(Satellite),     INTENT (INOUT) :: SAT
    TYPE(Imagery),       INTENT (INOUT) :: IMG
    TYPE(Aux_Data),      INTENT (INOUT) :: AUX
    TYPE(RTM_Interface), INTENT (INOUT) :: RTM

    INTEGER :: STAT

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Clean_Up.mod'

      IF ( Gbcs_Verbosity > 1 ) &
      WRITE(Gbcs_Log_File_Unit,*)'  Clean Up'

      STAT = Release_Satellite( SAT )
      IF ( Gbcs_Verbosity > 3 ) &
      WRITE(Gbcs_Log_File_Unit,*)'    Release Satellite Status = ',STAT
          
      STAT = Release_Imagery( IMG )
      IF ( Gbcs_Verbosity > 3 ) &
      WRITE(Gbcs_Log_File_Unit,*)'    Release Imagery Status   = ',STAT
          
      STAT = Release_Auxillary( AUX )
      IF ( Gbcs_Verbosity > 3 ) &
      WRITE(Gbcs_Log_File_Unit,*)'    Release Auxillary Status = ',STAT
          
      STAT = Release_RTM( RTM )
      IF ( Gbcs_Verbosity > 3 ) &
      WRITE(Gbcs_Log_File_Unit,*)'    Release RTM Status       = ',STAT
                    
    RETURN

  END SUBROUTINE Clean_Up  


  ! This function releases the space allocated for the satellite descriptor

  FUNCTION Release_Satellite( SAT )

  !
  ! Description:
  !
  !  Relases the satellite allocated structures
  !
  ! Method:
  !
  !  Tests each allocatable variable to see if it has been allocated
  !  If a variable is allocated it is dealloacted
  !
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
  ! The University of Edinburgh, The Crew Building, The King's Buildings
  ! West Mains Road, Edinburgh, UK  EH9 3JN
  !

    USE GbcsTypes

    IMPLICIT NONE

    INTEGER :: Release_Satellite
    TYPE(Satellite), INTENT (INOUT) :: SAT

    INTEGER :: STAT, ClrStatus

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Release_Satellite'

      ClrStatus = 0  

      IF ( ALLOCATED( SAT%Chan_ID ) ) THEN

        DEALLOCATE( SAT%Chan_ID , STAT=STAT )

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF ( ALLOCATED( SAT%Chan_Map ) ) THEN

        DEALLOCATE( SAT%Chan_Map , STAT=STAT )

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF ( ALLOCATED( SAT%Nedt ) ) THEN

        DEALLOCATE( SAT%NedT , STAT=STAT )

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF ( ALLOCATED( SAT%CWN ) ) THEN

        DEALLOCATE( SAT%CWN , STAT=STAT )

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF ( ALLOCATED( SAT%P_Offset ) ) THEN

        DEALLOCATE( SAT%P_Offset , STAT=STAT )

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF ( ALLOCATED( SAT%P_Slope ) ) THEN

        DEALLOCATE( SAT%P_Slope , STAT=STAT )

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      Release_Satellite = ClrStatus

    RETURN

  END FUNCTION Release_Satellite


  ! This function releases the space allocated for the satellite imagery

  FUNCTION Release_Imagery( IMG )

  !
  ! Description:
  !
  !  Relases the imagery allocated structures
  !
  ! Method:
  !
  !  Tests each allocatable variable to see if it has been allocated
  !  If a variable is allocated it is dealloacted
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   21/02/2005    Creation                                                   CPO
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

    USE GbcsTypes

    IMPLICIT NONE

    INTEGER :: Release_Imagery
    TYPE(Imagery), INTENT (INOUT) :: IMG

    INTEGER :: ii,STAT, ClrStatus
    INTEGER :: GbcsIndx

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Release_Imagery'

      ClrStatus = 0  

      DO ii = 1,IMG%N_Chans
      
        GbcsIndx = IMG%Gbcs_Map(ii)

        IF (ALLOCATED(IMG%Chan_Data(GbcsIndx)%d)) THEN

          DEALLOCATE(IMG%Chan_Data(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

        IF (ALLOCATED(IMG%Chan_LSD(GbcsIndx)%d)) THEN

          DEALLOCATE(IMG%Chan_LSD(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

        IF (ALLOCATED(IMG%Albedo(GbcsIndx)%d)) THEN

          DEALLOCATE(IMG%Albedo(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

      END DO        

      IF (ALLOCATED(IMG%Lat)) THEN

        DEALLOCATE(IMG%Lat, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%Lon)) THEN

        DEALLOCATE(IMG%Lon, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%SatZA)) THEN

        DEALLOCATE(IMG%SatZA, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%SolZA)) THEN

        DEALLOCATE(IMG%SolZA, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%LandSeaMask)) THEN

        DEALLOCATE(IMG%LandSeaMask, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%CloudMask)) THEN

        DEALLOCATE(IMG%CloudMask, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%ClearingFlags)) THEN

        DEALLOCATE(IMG%ClearingFlags, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%QualityFlags)) THEN

        DEALLOCATE(IMG%QualityFlags, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%Bad_Data)) THEN

        DEALLOCATE(IMG%Bad_Data, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%dBT_Atm)) THEN

        DEALLOCATE(IMG%dBT_Atm, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%edBT_Atm)) THEN

        DEALLOCATE(IMG%edBT_Atm, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%dBT_Glint)) THEN

        DEALLOCATE(IMG%dBT_Glint, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%edBT_Glint)) THEN

        DEALLOCATE(IMG%edBT_Glint, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%BT_MOD)) THEN

        DEALLOCATE(IMG%BT_MOD, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(IMG%eBT_MOD)) THEN

        DEALLOCATE(IMG%eBT_MOD, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      Release_Imagery = ClrStatus

    RETURN

  END FUNCTION Release_Imagery  


  ! This function releases the space allocated for the auxillary data

  FUNCTION Release_Auxillary( AUX )

  !
  ! Description:
  !
  !  Relases the auxillary allocated structures
  !
  ! Method:
  !
  !  Tests each allocatable variable to see if it has been allocated
  !  If a variable is allocated it is dealloacted
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   23/02/2005    Creation                                                   CPO
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

    USE GbcsTypes
    USE GbcsProfileGenerator
    USE GbcsForecastModel
#ifdef USE_GRIBNOAA
    USE NOAA_ReadGrib, ONLY:Deallocate_Grib_Arrays
#endif

    IMPLICIT NONE

    INTEGER :: Release_Auxillary
    TYPE(Aux_Data), INTENT (INOUT) :: AUX

    INTEGER :: ii, STAT, ClrStatus

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Release_Auxillary'

    ClrStatus = 0  

    STAT = Free_Forecast_Model(AUX%Model)


      IF (ALLOCATED(AUX%CovMat%Data)) THEN

        DEALLOCATE(AUX%CovMat%Data, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT
          WRITE(Gbcs_Log_File_Unit,*)'Error releasing Model covariance matrix data'

        END IF

      END IF

      IF (ALLOCATED(AUX%PDF_SN%LUT)) THEN

        DEALLOCATE(AUX%PDF_SN%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(AUX%PDF_ST%LUT)) THEN

        DEALLOCATE(AUX%PDF_ST%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(AUX%PDF_SD%LUT)) THEN

        DEALLOCATE(AUX%PDF_SD%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(AUX%PDF_TN%LUT)) THEN

        DEALLOCATE(AUX%PDF_TN%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(AUX%PDF_TT%LUT)) THEN

        DEALLOCATE(AUX%PDF_TT%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      IF (ALLOCATED(AUX%PDF_TD%LUT)) THEN

        DEALLOCATE(AUX%PDF_TD%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF
 
      IF (ALLOCATED(AUX%PDF_TD_CLR%LUT)) THEN

        DEALLOCATE(AUX%PDF_TD_CLR%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF
 
      IF (ALLOCATED(AUX%PDF_TT_CLR%LUT)) THEN

        DEALLOCATE(AUX%PDF_TT_CLR%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF
 
      IF (ALLOCATED(AUX%PDF_TN_CLR%LUT)) THEN

        DEALLOCATE(AUX%PDF_TN_CLR%LUT, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF
 
      DO ii = 1,4

        STAT = Destroy_Profile(AUX%Profiles(ii))

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END DO

      IF (ALLOCATED(AUX%SST_Clim%SST)) THEN

        DEALLOCATE(AUX%SST_Clim%SST, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF
 
      IF (ALLOCATED(AUX%VIS_CLD_PDF)) THEN

        DEALLOCATE(AUX%VIS_CLD_PDF, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF
 
      AUX%Aux_Data_Loaded = .FALSE.

#ifdef USE_GRIBNOAA
      ! If GRIB read clear up arrays
      CALL Deallocate_Grib_Arrays()
#endif

      Release_Auxillary = ClrStatus

    RETURN

  END FUNCTION Release_Auxillary  


  ! This function releases the space allocated for the RTM data

  FUNCTION Release_RTM( RTM )

  !
  ! Description:
  !
  !  Relases the radiative transfer model allocated structures
  !
  ! Method:
  !
  !  Tests each allocatable variable to see if it has been allocated
  !  If a variable is allocated it is dealloacted
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   09/03/2005    Creation                                                   CPO
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
    USE GbcsProfileGenerator
#ifdef USE_CRTM
    USE GbcsRunCRTM, ONLY:Deallocate_CRTM
#endif
    IMPLICIT NONE

    INTEGER :: Release_RTM
    TYPE(RTM_Interface), INTENT (INOUT) :: RTM

    INTEGER :: STAT, ClrStatus, RTM_Num, I_Prof, I_Chan
    INTEGER :: GbcsIndx

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Release_RTM'

    INTEGER :: RTM_Id

      ClrStatus = 0  

#ifdef USE_CRTM
      ! If used CRTM then cleanup CRTM arrays

      RTMLoop: DO RTM_Num = 1,RTM%N_RTMs     ! Process location using all RTMs

        RTM_id = RTM%ID_Map( RTM_Num )

        SELECT CASE ( RTM_id )
        CASE ( Gbcs_CRTM_IR_ID, Gbcs_CRTM_VIS_ID, Gbcs_CRTM_VISIR_ID )
           CALL Deallocate_CRTM(RTM%RTM_Set(RTM_id))
           EXIT RTMLoop
        END SELECT

     END DO RTMLoop
#endif
     ! Then do the rest
      DO RTM_Num = 1,Gbcs_Max_RTMs

        DO I_Prof = 1,4

          STAT = Destroy_Profile( RTM%RTM_Set( RTM_Num )%Profiles(I_Prof) )

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END DO

        IF ( ALLOCATED( RTM%RTM_Set( RTM_Num )%TLN ) ) THEN

          DEALLOCATE( RTM%RTM_Set( RTM_Num )%TLN , STAT=STAT )

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

      END DO

      IF (ALLOCATED(RTM%Run_Map)) THEN

        DEALLOCATE(RTM%Run_Map, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      DO I_Chan = 1,RTM%N_Chans
      
        GbcsIndx = RTM%Gbcs_Map(I_Chan)

        IF (ALLOCATED(RTM%BTR(GbcsIndx)%d)) THEN

          DEALLOCATE(RTM%BTR(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

        IF (ALLOCATED(RTM%RAD(GbcsIndx)%d)) THEN

          DEALLOCATE(RTM%RAD(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

        IF (ALLOCATED(RTM%TAU(GbcsIndx)%d)) THEN

          DEALLOCATE(RTM%TAU(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

        IF (ALLOCATED(RTM%Opt_Depth(GbcsIndx)%d)) THEN

          DEALLOCATE(RTM%Opt_Depth(GbcsIndx)%d, STAT=STAT)

          IF (STAT /= 0) THEN

            ClrStatus = STAT

          END IF

        END IF

      END DO        

      IF (ALLOCATED(RTM%H_Matrix)) THEN

        DEALLOCATE(RTM%H_Matrix, STAT=STAT)

        IF (STAT /= 0) THEN

          ClrStatus = STAT

        END IF

      END IF

      Release_RTM = ClrStatus

    RETURN

  END FUNCTION Release_RTM 


END MODULE GbcsCleanUp
