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




!+ This module contains data readers for Cloudy Sky PDF Look Up Tables

MODULE GbcsPDFLoaders

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
! 0.0   14/03/2005    Creation                                                   CPO
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
  USE GbcsTypes
  USE GbcsErrorHandler

  IMPLICIT NONE

  CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsPDFLoaders.mod'

CONTAINS

  ! Read Cloudy Sky PDF Look Up Tables (LUTs)

  INTEGER FUNCTION Load_PDF_LUT( SAT , AUX , SpcTxt , NgtDay , PDF )

  !
  ! Description:
  !
  !  PDF LUT Loader : load PDF LUT from file dependent on
  !                           satellite
  !                           spectral or textural
  !                           day or night
  !
  !                   Implemented to generalize loading of cloudy sky PDF LUTs
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   21/11/2004    Creation                                                   CPO
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
  
    USE GbcsConstants
    USE GbcsTypes
    USE GbcsSystemTools, ONLY: Get_New_File_Unit
    USE GbcsErrorHandler

    IMPLICIT NONE

  ! Define Input Variabiles

    TYPE(Satellite),   INTENT (IN   ) :: SAT
    TYPE(Aux_Data),    INTENT (INOUT) :: AUX
    INTEGER,           INTENT (IN   ) :: SpcTxt      ! 0 = spectral; 1 = textural
    INTEGER,           INTENT (IN   ) :: NgtDay      ! 0 = night; 1 = day

  ! Define Output Variabiles

    TYPE(PDF_LUT_Struct), INTENT(INOUT) :: PDF  ! PDF Structure

  ! Define Local Variabiles

    INTEGER :: I_Chan, Chan_ID
    INTEGER :: ndims
    INTEGER :: nrecs
    INTEGER :: STAT
    CHARACTER(LEN=256) :: fname ! character string for data filenames
    CHARACTER(LEN=256) :: LUT_File_Name
    CHARACTER(LEN=3)  :: File_Type

    INTEGER :: File_Unit
    LOGICAL :: File_Exists

    INTEGER :: Record_Num

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Load_PDF_LUT'


      STAT = 0
      Record_Num = 1 ! Default Record Number

      !-----------------------------------
      !     DEFAULT PDF LUTs
      !-----------------------------------
  
      IF ( ( SpcTxt == 0 ).AND.( NgtDay == Gbcs_Nighttime ) ) THEN                ! Spectural, Nighttime

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_SPEC_PDF_'//AUX%SN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_SPEC_PDF_'//AUX%SN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'AATSR_SPEC_PDF_NADIR_'//AUX%SN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 1

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_SPEC_PDF_'//AUX%SN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_SPEC_PDF_'//AUX%SN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          fname=TRIM(AUX%GbcsDataPath)//'DEFAULT_PDF_LUT_SN.ASC'
          File_Type = 'ASC'

        END IF

      ELSE IF ( ( SpcTxt == 0 ).AND.( NgtDay == Gbcs_Daytime ) ) THEN           ! Spectural, daytime

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_SPEC_PDF_'//AUX%SD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_SPEC_PDF_'//AUX%SD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'AATSR_SPEC_PDF_NADIR_'//AUX%SD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 2

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_SPEC_PDF_'//AUX%SD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_SPEC_PDF_'//AUX%SD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          fname=TRIM(AUX%GbcsDataPath)//'DEFAULT_PDF_LUT_SD.ASC'
          File_Type = 'ASC'

        END IF

      ELSE IF ( ( SpcTxt == 0 ).AND.( NgtDay == Gbcs_Twilight ) ) THEN           ! Spectural, twilight

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_SPEC_PDF_'//AUX%ST_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_SPEC_PDF_'//AUX%ST_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'AATSR_SPEC_PDF_NADIR_'//AUX%ST_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 1
 
        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_SPEC_PDF_'//AUX%ST_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_SPEC_PDF_'//AUX%ST_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          fname=TRIM(AUX%GbcsDataPath)//'DEFAULT_PDF_LUT_SD.ASC'
          File_Type = 'ASC'

        END IF

      ELSE IF ( ( SpcTxt == 1 ).AND.( NgtDay == Gbcs_Nighttime ) ) THEN           ! Textural, nighttime

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'ATSR2_CLD_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 1

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          fname=TRIM(AUX%GbcsDataPath)//'DEFAULT_PDF_LUT_TN.ASC'
          File_Type = 'ASC'

        END IF

      ELSE IF ( ( SpcTxt == 1 ).AND.( NgtDay == Gbcs_Daytime ) ) THEN           ! Textural, daytime

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'ATSR2_CLD_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 3

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          fname=TRIM(AUX%GbcsDataPath)//'DEFAULT_PDF_LUT_TD.ASC'
          File_Type = 'ASC'

        END IF

      ELSE IF ( ( SpcTxt == 1 ).AND.( NgtDay == Gbcs_Twilight ) ) THEN           ! Textural, twilight

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'ATSR2_CLD_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 1

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          fname=TRIM(AUX%GbcsDataPath)//'DEFAULT_PDF_LUT_TD.ASC'
          File_Type = 'ASC'

        END IF

      ELSE

        STAT = 2
        WRITE(Gbcs_Log_File_Unit,*)' ERROR : Invalid flag combination'
        WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_PDF_LUT'
        WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
        WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

      END IF

      IF (STAT == 0) THEN

        IF (File_Type == 'ASC') THEN

          INQUIRE( FILE=TRIM( fname ) , EXIST=File_Exists )

          IF ( File_Exists ) THEN
            
            File_Unit = Get_New_File_Unit()

            OPEN( UNIT=File_Unit , FILE=TRIM(fname) )
    
            IF ( SpcTxt == 0 ) THEN
              PDF%Spectral = .TRUE.
            ELSE
              PDF%Spectral = .FALSE.
            END IF

            READ(File_Unit,*) PDF%NChans
            DO I_Chan = 1,PDF%NChans
              READ(File_Unit,*) Chan_ID
              PDF%Chan_ID( I_Chan ) = SAT%Chan_Map( Chan_ID )
            END DO
            READ(File_Unit,*) PDF%Dims(1:PDF%NChans)
            READ(File_Unit,*) PDF%Orig(1:PDF%NChans)
            READ(File_Unit,*) PDF%Incr(1:PDF%NChans)

            ndims = PDF%NChans
            nrecs = PRODUCT(PDF%Dims(1:PDF%NChans))

            IF ( Gbcs_Verbosity > 3 ) THEN
              WRITE(Gbcs_Log_File_Unit,*)'      PDF LUT File Name : ',TRIM(fname)
              WRITE(Gbcs_Log_File_Unit,*)'              Num. Dims = ',ndims
              WRITE(Gbcs_Log_File_Unit,*)'              Num. Recs = ',nrecs
            END IF

            IF ( ALLOCATED( PDF%LUT ) ) THEN
              DEALLOCATE( PDF%LUT , STAT=STAT )
            END IF

            ALLOCATE(PDF%LUT(nrecs),STAT=STAT)

            IF (STAT == 0) THEN

              READ(File_Unit,*) PDF%LUT(1:nrecs)

            ELSE

              STAT = 4
              WRITE(Gbcs_Log_File_Unit,*)' ERROR : Unable to allocate data space for LUT'
              WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_PDF_LUT'
              WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
              WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

            END IF

            CLOSE(File_Unit)

          ELSE

            STAT = 3
            WRITE(Gbcs_Log_File_Unit,*)' WARNING : File does not exist'
            WRITE(Gbcs_Log_File_Unit,*)'         ',TRIM(fname)
            WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_PDF_LUT'
            WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
            WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

          END IF

        ELSE

          INQUIRE( FILE=TRIM( fname ) , EXIST=File_Exists )

          IF ( File_Exists ) THEN
            
            File_Unit = Get_New_File_Unit()

            OPEN( UNIT=File_Unit , FILE=TRIM(fname) )
    
            IF ( SpcTxt == 0 ) THEN
              PDF%Spectral = .TRUE.
            ELSE
              PDF%Spectral = .FALSE.
            END IF

            READ(File_Unit,*) LUT_File_Name
            READ(File_Unit,*) PDF%NChans
            DO I_Chan = 1,PDF%NChans
              READ(File_Unit,*) Chan_ID
              PDF%Chan_ID( I_Chan ) = SAT%Chan_Map( Chan_ID )
            END DO
            READ(File_Unit,*) PDF%Dims(1:PDF%NChans)
            READ(File_Unit,*) PDF%Orig(1:PDF%NChans)
            READ(File_Unit,*) PDF%Incr(1:PDF%NChans)

            CLOSE(File_Unit)

            ndims = PDF%NChans
            nrecs = PRODUCT(PDF%Dims(1:PDF%NChans))

            fname = TRIM(AUX%GbcsDataPath)//TRIM(LUT_File_Name)

            INQUIRE( FILE=TRIM( fname ) , EXIST=File_Exists )

            IF ( File_Exists ) THEN
              
              IF ( Gbcs_Verbosity > 3 ) THEN
                WRITE(Gbcs_Log_File_Unit,*)'      PDF LUT File Name : ',TRIM(LUT_File_Name)
                WRITE(Gbcs_Log_File_Unit,*)'              Num. Dims = ',ndims
                WRITE(Gbcs_Log_File_Unit,*)'              Num. Recs = ',nrecs
              END IF
              
              IF ( ALLOCATED( PDF%LUT ) ) THEN
                DEALLOCATE( PDF%LUT , STAT=STAT )
              END IF

              ALLOCATE(PDF%LUT(nrecs),STAT=STAT)

              IF (STAT == 0) THEN

                File_Unit = Get_New_File_Unit()

                OPEN( UNIT   = File_Unit  , &
                      FILE   = TRIM(fname), &
                      ACCESS = 'DIRECT'   , &
                      RECL   = nrecs*4      )

                READ(File_Unit,REC=Record_Num) PDF%LUT

                CLOSE(File_Unit)

              ELSE

                STAT = 4
                WRITE(Gbcs_Log_File_Unit,*)' ERROR : Unable to allocate data space for LUT'
                WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_PDF_LUT'
                WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
                WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

              END IF

            ELSE

              STAT = 3
              WRITE(Gbcs_Log_File_Unit,*)' ERROR : File does not exist'
              WRITE(Gbcs_Log_File_Unit,*)'         ',TRIM(fname)
              WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_PDF_LUT'
              WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
              WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

            END IF

          ELSE

            STAT = 3
            WRITE(Gbcs_Log_File_Unit,*)' WARNING : File does not exist'
            WRITE(Gbcs_Log_File_Unit,*)'         ',TRIM(fname)
            WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_PDF_LUT'
            WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
            WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

          END IF

        END IF

      END IF
  
      Load_PDF_LUT = STAT                    ! Return load status

    RETURN

  END FUNCTION Load_PDF_LUT


  ! Read Clear Sky Texture PDF Look Up Tables (LUTs)

  INTEGER FUNCTION Load_CLR_PDF_LUT( SAT , AUX , SpcTxt , NgtDay , PDF )

  !
  ! Description:
  !
  !  PDF LUT Loader : load PDF LUT from file dependent on
  !                           satellite
  !                           spectral or textural
  !                           day or night
  !
  !                   Implemented to generalize loading of clear sky PDF LUTs
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   08/03/2006    Creation                                                   CPO
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
  
    USE GbcsSystemTools, ONLY: Get_New_File_Unit
    
    IMPLICIT NONE

  ! Define Input Variabiles

    TYPE(Satellite),   INTENT (IN   ) :: SAT
    TYPE(Aux_Data),    INTENT (INOUT) :: AUX
    INTEGER,           INTENT (IN   ) :: SpcTxt      ! 0 = spectral; 1 = textural
    INTEGER,           INTENT (IN   ) :: NgtDay      ! 0 = night; 1 = day

  ! Define Output Variabiles

    TYPE(PDF_LUT_Struct), INTENT(INOUT) :: PDF  ! PDF Structure

  ! Define Local Variabiles

    INTEGER :: I_Chan, Chan_ID
    INTEGER :: ndims
    INTEGER :: nrecs
    INTEGER :: STAT
    CHARACTER(LEN=256) :: fname ! character string for data filenames
    CHARACTER(LEN=256) :: LUT_File_Name
    CHARACTER(LEN=3)  :: File_Type

    INTEGER :: File_Unit
    LOGICAL :: File_Exists

    INTEGER :: Record_Num

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Load_CLR_PDF_LUT'


      STAT = 0
      Record_Num = 1  ! Default record number

      !-----------------------------------
      !     DEFAULT PDF LUTs
      !-----------------------------------

      IF ( ( SpcTxt == 1 ).AND.( NgtDay == Gbcs_Nighttime ) ) THEN                ! Textural, nighttime

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_CLR_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_CLR_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'ATSR2_CLR_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 1

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_CLR_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_CLR_TEXT_PDF_'//AUX%TN_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          STAT = 1
          WRITE(Gbcs_Log_File_Unit,*)' ERROR : Invalid satellite ID: ',SAT%Platform_ID
          WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
          WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
          WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

        END IF

      ELSE IF ( ( SpcTxt == 1 ).AND.( NgtDay == Gbcs_Daytime ) ) THEN           ! Textural, daytime

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_CLR_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_CLR_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'ATSR2_CLR_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 3

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_CLR_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_CLR_TEXT_PDF_'//AUX%TD_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          STAT = 1
          WRITE(Gbcs_Log_File_Unit,*)' ERROR : Invalid satellite ID : ',SAT%Platform_ID
          WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
          WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
          WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

        END IF

      ELSE IF ( ( SpcTxt == 1 ).AND.( NgtDay == Gbcs_Twilight ) ) THEN           ! Textural, twilight

        IF ( SAT%Platform_ID == Gbcs_MSG ) THEN                      ! MSG1

          fname = TRIM(AUX%GbcsDataPath)//'MSG_CLR_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ERS ) THEN                 ! ERS

          fname = TRIM(AUX%GbcsDataPath)//'ATSR_CLR_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_ENVISAT ) THEN             ! ENVISAT

          fname = TRIM(AUX%GbcsDataPath)//'ATSR2_CLR_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'
          Record_Num = 1

        ELSE IF ( SAT%Platform_ID == Gbcs_GOES ) THEN                ! GOES

          fname = TRIM(AUX%GbcsDataPath)//'GOES_CLR_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE IF ( SAT%Platform_ID == Gbcs_MTSAT ) THEN                ! MTSAT

          fname = TRIM(AUX%GbcsDataPath)//'MTSAT_CLR_TEXT_PDF_'//AUX%TT_PDF_Num_Str//'.HDR'
          File_Type = 'BIN'

        ELSE

          STAT = 1
          WRITE(Gbcs_Log_File_Unit,*)' ERROR : Invalid satellite ID : ',SAT%Platform_ID
          WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
          WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
          WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

        END IF

      ELSE

        STAT = 2
        WRITE(Gbcs_Log_File_Unit,*)' ERROR : Invalid flag combination'
        WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
        WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
        WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

      END IF

      IF (STAT == 0) THEN

        INQUIRE( FILE=TRIM( fname ) , EXIST=File_Exists )

        IF ( File_Exists ) THEN
          
          File_Unit = Get_New_File_Unit()

          OPEN( UNIT=File_Unit , FILE=TRIM(fname) )
    
          READ(File_Unit,*) LUT_File_Name
          READ(File_Unit,*) PDF%NChans
          DO I_Chan = 1,PDF%NChans
            READ(File_Unit,*) Chan_ID
            PDF%Chan_ID( I_Chan ) = SAT%Chan_Map( Chan_ID )
          END DO
          READ(File_Unit,*) PDF%Dims(1:PDF%NChans)
          READ(File_Unit,*) PDF%Orig(1:PDF%NChans)
          READ(File_Unit,*) PDF%Incr(1:PDF%NChans)
          READ(File_Unit,*) PDF%Data_Flag

          CLOSE(File_Unit)

          ndims = PDF%NChans
          nrecs = PRODUCT(PDF%Dims(1:PDF%NChans))

          fname = TRIM(AUX%GbcsDataPath)//TRIM(LUT_File_Name)

          INQUIRE( FILE=TRIM( fname ) , EXIST=File_Exists )

          IF ( File_Exists ) THEN
            
            IF ( Gbcs_Verbosity > 3 ) THEN
              WRITE(Gbcs_Log_File_Unit,*)'      PDF LUT File Name : ',TRIM(LUT_File_Name)
              WRITE(Gbcs_Log_File_Unit,*)'              Num. Dims = ',ndims
              WRITE(Gbcs_Log_File_Unit,*)'              Num. Recs = ',nrecs
            END IF
            
            IF ( ALLOCATED( PDF%LUT ) ) THEN
              DEALLOCATE( PDF%LUT , STAT=STAT )
            END IF

            ALLOCATE(PDF%LUT(nrecs),STAT=STAT)

            IF (STAT == 0) THEN

              File_Unit = Get_New_File_Unit()

              OPEN( UNIT   = File_Unit  , &
                    FILE   = TRIM(fname), &
                    ACCESS = 'DIRECT'   , &
                    RECL   = nrecs*4     )

              READ(File_Unit,REC=Record_Num) PDF%LUT

              CLOSE(File_Unit)

            ELSE

              STAT = 4
              WRITE(Gbcs_Log_File_Unit,*)' ERROR : Unable to allocate data space for LUT'
              WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
              WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
              WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

            END IF

          ELSE

            STAT = 3
            WRITE(Gbcs_Log_File_Unit,*)' ERROR : File does not exist'
            WRITE(Gbcs_Log_File_Unit,*)'         ',TRIM(fname)
            WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
            WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
            WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

          END IF

        ELSE

          STAT = 3
          WRITE(Gbcs_Log_File_Unit,*)' ERROR : File does not exist'
          WRITE(Gbcs_Log_File_Unit,*)'         ',TRIM(fname)
          WRITE(Gbcs_Log_File_Unit,*)'         SUBROUTINE Load_CLR_PDF_LUT'
          WRITE(Gbcs_Log_File_Unit,*)'         MODULE GbcsPDFLoaders'
          WRITE(Gbcs_Log_File_Unit,*)'         CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

        END IF

      END IF

      Load_CLR_PDF_LUT = STAT                    ! Return load status

    RETURN

  END FUNCTION Load_CLR_PDF_LUT


  REAL(KIND=GbcsReal) FUNCTION Look_Up_Clear_Txt_PDF( PDF , PX )

  !
  ! Description:
  !
  !  Retrieve Clear sky texture probability based on LSD of BTs
  !
  ! Method:
  !
  !  The PDFs are loaded as a vector array of binned proability densisties.
  !
  !  The index along each dimension of the N-dimensional array is given by
  !
  !     INDEX(i) =  CEILING( ( LSD(i) - ORIGIN(i) ) / BinWidth(i) ) - CEILING = round up, and i = 1 to N
  !     
  !     If INDEX(i) < 1 then INDEX(i) is set to 1
  !     If INDEX(i) > NPts(i) then INDEX(i) is set to NPts 
  !
  !     NOTE: There are two options for indexing based on whether the linear scale of the PDF is LSD or variance
  !           If variance is the linear scale then the square of the LSD is used to find the index
  !           A flag is set in the PDF description identifying which variable defines the linear scale
  !
  !
  !  The N indices are then combines to get the offest into the stored vector array
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   24/02/2006    Creation                                                   CPO
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
    IMPLICIT NONE

!    REAL(KIND=GbcsReal) :: Look_Up_Clear_Txt_PDF

    TYPE(PDF_LUT_Struct), INTENT (IN   ) :: PDF
    TYPE(ImagePixel),     INTENT (INOUT) :: PX

    INTEGER, DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Indx
    INTEGER :: Chan
    INTEGER :: Offset

    INTEGER :: Chan_ID

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Look_Up_Clear_Txt_PDF'


      IF ( ALLOCATED( PDF%LUT ) ) THEN

        DO Chan = 1,PDF%NChans

          IF ( PDF%Data_Flag == 1 ) THEN
            Chan_ID = PDF%Chan_ID(Chan)
            Indx(Chan) = CEILING( ( PX%LSD( Chan_ID ) - PDF%Orig(Chan) ) / PDF%Incr(Chan) )
          ELSE
            Chan_ID = PDF%Chan_ID(Chan)
            Indx(Chan) = CEILING( ( ( PX%LSD( Chan_ID ) * PX%LSD( Chan_ID ) ) - PDF%Orig(Chan) ) / PDF%Incr(Chan) )
          END IF

          IF (Indx(Chan) < 1) Indx(Chan) = 1
          IF (Indx(Chan) > PDF%Dims(Chan)) Indx(Chan) = PDF%Dims(Chan)

        END DO

        Offset = 0

        DO Chan = PDF%NChans,2,-1

          Offset = ( Offset + ( Indx(Chan) - 1 ) ) * PDF%Dims(Chan-1) 

        END DO

        Offset = Offset + Indx(1)

        Look_Up_Clear_Txt_PDF = PDF%LUT(Offset)

      ELSE

        ! Calculate PDFs from theoretical considerations

        Look_Up_Clear_Txt_PDF = Calculate_Clear_Text_PDF( PDF, PX )


!        WRITE(Gbcs_Log_File_Unit,*)'  ERROR : Clear Texture PDF LUT not allocated'
!        WRITE(Gbcs_Log_File_Unit,*)'          SUBROUTINE - Look_Up_PDF'
!        WRITE(Gbcs_Log_File_Unit,*)'          CODE - /GbcsMod_DataReaders/GbcsPDFLoaders.f90'

!        STOP

      END IF

    RETURN

  END FUNCTION Look_Up_Clear_Txt_PDF

  ! Wrapper code for front/nofront for clear textural PDF
  REAL(KIND=GbcsReal) FUNCTION Calculate_Clear_Text_PDF( PDF, PX )&
       RESULT(outProb)
  
    TYPE(PDF_LUT_Struct), INTENT (IN   ) :: PDF
    TYPE(ImagePixel),     INTENT (INOUT) :: PX

    REAL(GbcsReal) :: prob_front_weight = 0.1
    REAL(GbcsReal) :: prob_nofront
    REAL(GbcsReal) :: prob_front

    prob_nofront = Calculate_Clear_Text_PDF_Gauss( PDF, PX )
    prob_front = Calculate_Clear_Text_PDF_Gauss( PDF, PX, FRONT=.TRUE. )

    outProb = (1.-prob_front_weight)*prob_nofront + &
         prob_front_weight*prob_front

!    outProb = prob_nofront
!    print *,outProb

  END FUNCTION Calculate_Clear_Text_PDF

  ! Fortran version of Stewarts calc_p_y3_y4_clr routine from original
  ! Bayesian code
  REAL(KIND=GbcsReal) FUNCTION Calculate_Clear_Text_PDF_Gauss( PDF, PX, &
       FRONT, MIN_FRONT )RESULT(outProb)

    USE GbcsMatrixOps

    TYPE(PDF_LUT_Struct), INTENT (IN   ) :: PDF
    TYPE(ImagePixel),     INTENT (INOUT) :: PX
    LOGICAL, INTENT(IN), OPTIONAL :: FRONT
    REAL, INTENT(IN), OPTIONAL :: MIN_FRONT

    LOGICAL :: USE_FRONT

    REAL(GbcsReal) :: sig(Gbcs_Max_Chans)
    REAL(GbcsReal) :: variability(Gbcs_Max_Chans)
    REAL(GbcsReal) :: front_var(Gbcs_Max_Chans)
    INTEGER :: STAT
    ! Pointers
    REAL(GbcsDble), POINTER :: ytb(:)
    REAL(GbcsDble), POINTER :: dyt_add(:)
    REAL(GbcsDble), POINTER :: dyt_sub(:)
    REAL(GbcsDble), POINTER :: Matrix(:,:)
    REAL(GbcsDble), POINTER :: invMatrix(:,:)
    REAL(GbcsDble), POINTER :: outVals(:)
    ! DayTime
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: ytb_day(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: dyt_add_day(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: dyt_sub_day(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: Matrix_day(:,:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: invMatrix_day(:,:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: outVals_day(:)
    ! NightTime
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: ytb_night(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: dyt_add_night(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: dyt_sub_night(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: Matrix_night(:,:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: invMatrix_night(:,:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: outVals_night(:)
    ! Twilight
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: ytb_twilight(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: dyt_add_twilight(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: dyt_sub_twilight(:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: Matrix_twilight(:,:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: invMatrix_twilight(:,:)
    REAL(GbcsDble), ALLOCATABLE, SAVE, TARGET :: outVals_twilight(:)

    REAL(GbcsReal) :: min_front_size
    REAL(GbcsReal) :: rho ! correlation of two LSDs random noise only    
    REAL(GbcsDble) :: determinant
    REAL(GbcsDble) :: add_value
    REAL(GbcsDble) :: sub_value

    INTEGER :: I, J
    INTEGER :: Gbcs_Chan
    INTEGER, Pointer :: PX_Chan_Store(:)
    LOGICAL :: PX_Chan_Store_Day_Alloc = .FALSE.
    LOGICAL :: PX_Chan_Store_Night_Alloc = .FALSE.
    LOGICAL :: PX_Chan_Store_Twilight_Alloc = .FALSE.
    INTEGER, ALLOCATABLE, SAVE, TARGET :: PX_Chan_Store_Day(:)
    INTEGER, ALLOCATABLE, SAVE, TARGET :: PX_Chan_Store_Night(:)
    INTEGER, ALLOCATABLE, SAVE, TARGET :: PX_Chan_Store_Twilight(:)

    USE_FRONT = .FALSE.
    IF( PRESENT(FRONT) )THEN
       USE_FRONT = FRONT
    ENDIF
    min_front_size = 0.5
    IF( PRESENT(MIN_FRONT) )THEN
       min_front_size = min_front
    ENDIF

    ! Allocate memory if needed
    IF( PX%DayNight .eq. Gbcs_DayTime )THEN
       IF( .not.PX_Chan_Store_Day_Alloc )THEN
          ALLOCATE(PX_Chan_Store_Day(PDF%Nchans),STAT=STAT)          
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ! Fill PX_Chan_Store with indexing to do have to re-loop
          DO I=1,PDF%NChans
             DO J=1,PX%N_Chans
                IF( PDF%Chan_ID(I) .eq. PX%Chan_ID(J) )THEN
                   PX_Chan_Store_Day(I) = J
                ENDIF
             END DO
          END DO
          PX_Chan_Store_Day_Alloc = .TRUE.
          ALLOCATE(ytb_day(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate ytb',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(dyt_add_day(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_add',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(dyt_sub_day(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(outVals_day(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(Matrix_day(PDF%Nchans,PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate Matrix',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(invMatrix_day(PDF%Nchans,PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate invMatrix',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
       ENDIF
       PX_Chan_Store => PX_Chan_Store_Day
       ytb => ytb_day
       dyt_add => dyt_add_day
       dyt_sub => dyt_sub_day
       outVals => outVals_day
       Matrix => Matrix_day
       invMatrix => invMatrix_day
    ELSE IF( PX%DayNight .eq. Gbcs_Twilight )THEN
       IF( .not.PX_Chan_Store_Twilight_Alloc )THEN
          ALLOCATE(PX_Chan_Store_Twilight(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ! Fill PX_Chan_Store with indexing to do have to re-loop
          DO I=1,PDF%NChans
             DO J=1,PX%N_Chans
                IF( PDF%Chan_ID(I) .eq. PX%Chan_ID(J) )THEN
                   PX_Chan_Store_Twilight(I) = J
                ENDIF
             END DO
          END DO
          PX_Chan_Store_Twilight_Alloc = .TRUE.
          ALLOCATE(ytb_twilight(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate ytb',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(dyt_add_twilight(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_add',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(dyt_sub_twilight(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(outVals_twilight(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(Matrix_twilight(PDF%Nchans,PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate Matrix',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(invMatrix_twilight(PDF%Nchans,PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate invMatrix',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
       ENDIF
       PX_Chan_Store => PX_Chan_Store_Twilight
       ytb => ytb_twilight
       dyt_add => dyt_add_twilight
       dyt_sub => dyt_sub_twilight
       outVals => outVals_twilight
       Matrix => Matrix_twilight
       invMatrix => invMatrix_twilight
    ELSE IF( PX%DayNight .eq. Gbcs_NightTime )THEN
       IF( .not.PX_Chan_Store_Night_Alloc )THEN
          ALLOCATE(PX_Chan_Store_Night(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ! Fill PX_Chan_Store with indexing to do have to re-loop
          DO I=1,PDF%NChans
             DO J=1,PX%N_Chans
                IF( PDF%Chan_ID(I) .eq. PX%Chan_ID(J) )THEN
                   PX_Chan_Store_Night(I) = J
                ENDIF
             END DO
          END DO
          PX_Chan_Store_Night_Alloc = .TRUE.
          ALLOCATE(ytb_night(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate ytb',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(dyt_add_night(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_add',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(dyt_sub_night(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(outVals_night(PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate dyt_sub',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(Matrix_night(PDF%Nchans,PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate Matrix',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
          ALLOCATE(invMatrix_night(PDF%Nchans,PDF%Nchans),STAT=STAT)
          IF( 0 .ne. STAT )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot allocate invMatrix',&
                  'Calculate_Clear_PDF_Text',&
                  'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
          ENDIF
       ENDIF
       PX_Chan_Store => PX_Chan_Store_Night
       ytb => ytb_night
       dyt_add => dyt_add_night
       dyt_sub => dyt_sub_night
       outVals => outVals_night
       Matrix => Matrix_night
       invMatrix => invMatrix_night
    ENDIF

    IF( USE_FRONT )THEN
       DO I=1,PDF%NChans
          sig(I) = min_front_size * PX%H_Matrix(1,PX_Chan_Store(I))
       END DO
    ELSE
       sig(:) = 0.
    ENDIF

    DO I=1,PDF%NChans
       gbcs_chan = PX%Chan_ID(PX_Chan_Store(I)) 
       ytb(I) = sqrt(PX%eYo(Gbcs_Chan)**2+sig(I)**2)
       variability(I) = PX%eYo(Gbcs_Chan) * PX%eLSD(Gbcs_Chan)
       front_var(I) = sig(I) * PX%eLSD(Gbcs_Chan)
       ! Get +/- values
       dyt_add(I) = PX%LSD(Gbcs_Chan) + ytb(I)
       dyt_sub(I) = PX%LSD(Gbcs_Chan) - ytb(I)
    END DO

    ! Fill error covariance matrix
    rho = 1.0 !This is a guess at correction from old Bayesian code
    DO I=1,PDF%NChans
       DO J=1,PDF%NChans
          IF( J .eq. I )THEN
             Matrix(J,I) = variability(I)**2 + front_var(I)**2
          ELSE
             Matrix(J,I) = rho*front_var(J)*front_var(I)
          ENDIF
       END DO
    END DO
    
    CALL INVERT(Matrix,PDF%NChans,invMatrix,determinant,STAT)
    IF( 0 .ne. STAT )THEN
       call Gbcs_Critical(.TRUE.,'Cannot calculate matrix inverse',&
            'Calculate_Clear_Text_PDF',&
            'GbcsMod_DataReaders/GbcsPDFLoaders.f90')
    ENDIF

    ! Do Matrix multiplications
    outVals = MATMUL(dyt_add,invMatrix)
    add_value = SUM(outVals * dyt_add)
    
    outVals = MATMUL(dyt_sub,invMatrix)
    sub_value = SUM(outVals * dyt_sub)

    ! Find probability
    outProb = (exp(-0.5d0*add_value) + exp(-0.5d0*sub_value)) / &
         (((2.d0*D_Pi)**(PDF%NChans/2.))*sqrt(determinant))

    RETURN

  END FUNCTION Calculate_Clear_Text_PDF_Gauss

  REAL(KIND=GbcsReal) FUNCTION Look_Up_PDF( PDF , PX )

  !
  ! Description:
  !
  !  Retrieve value from look up table in PDF structure
  !
  ! Method:
  !
  !  The type of PDF (spectral or textural) is determined by the flag in the PDF structure
  !
  !  The PDF LUT is loaded as a vector array of binned proability densisties.
  !
  !  The index along each dimension of the N-dimensional array is given by
  !
  !     INDEX(i) =  CEILING( ( BT(i) - ORIGIN(i) ) / BinWidth(i) ) - CEILIING = round up, and i = 1 to N
  !     
  !     If INDEX(i) < 1 then INDEX(i) is set to 1
  !     If INDEX(i) > NPts(i) then INDEX(i) is set to NPts 
  !
  !     NOTE: If the LUT is textural then the LSD is used insteadd of the BT to calculate the indices
  !
  !  The N indices are then combines to get the offest into the stored vector array
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   18/05/2005    Creation                                                   CPO
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
    USE GbcsErrorHandler
    IMPLICIT NONE

!    REAL(KIND=GbcsReal) :: Look_Up_PDF

    TYPE(PDF_LUT_Struct), INTENT (IN   ) :: PDF
    TYPE(ImagePixel),     INTENT (INOUT) :: PX

    INTEGER, DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Indx
    INTEGER :: Chan
    INTEGER :: Offset
    REAL(KIND=GbcsReal) :: Yobs , LSD

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Look_Up_PDF'

      IF ( ALLOCATED( PDF%LUT ) ) THEN

        DO Chan = 1,PDF%NChans

          IF ( PDF%Spectral ) THEN
            Yobs = MAX( PX%Yobs( PDF%Chan_ID(Chan) ) , PDF%Orig(Chan) )
            Indx(Chan) = CEILING( ( Yobs - PDF%Orig(Chan) ) / PDF%Incr(Chan) )
          ELSE
            LSD = MAX( PX%LSD( PDF%Chan_ID(Chan) ) , PDF%Orig(Chan) )
            Indx(Chan) = CEILING( ( LSD - PDF%Orig(Chan) ) / PDF%Incr(Chan) )
          END IF

          IF (Indx(Chan) < 1) Indx(Chan) = 1
          IF (Indx(Chan) > PDF%Dims(Chan)) Indx(Chan) = PDF%Dims(Chan)

        END DO

        Offset = 0

        DO Chan = PDF%NChans,2,-1

          Offset = ( Offset + ( Indx(Chan) - 1 ) ) * PDF%Dims(Chan-1) 

        END DO

        Offset = Offset + Indx(1)
        Look_Up_PDF = PDF%LUT(Offset)

      ELSE

        WRITE(Gbcs_Log_File_Unit,*)'  ERROR : Cloudy PDF LUT not allocated'
        WRITE(Gbcs_Log_File_Unit,*)'          SUBROUTINE - Look_Up_PDF'
        WRITE(Gbcs_Log_File_Unit,*)'          CODE - /GbcsPrg_ProcessImagery/GbcsGetPixelFields.f90'

        STOP

      END IF

    RETURN

  END FUNCTION Look_Up_PDF

END MODULE GbcsPDFLoaders

