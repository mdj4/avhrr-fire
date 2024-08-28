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



!
!  GBCS Error Handling Code
!

MODULE GbcsErrorHandler

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
! 0.0   03/11/2005    Creation                                                   CPO
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

! Global Type Definitions:

 
  IMPLICIT NONE


  ! Error Record Data Structure

  TYPE Gbcs_Error_Record
    INTEGER                           :: Error_Code
    CHARACTER(LEN=256), DIMENSION(10) :: Extra_Information
    INTEGER                           :: Num_Extra_Info
    CHARACTER(LEN=256)                :: Subroutine_Name
    CHARACTER(LEN=256)                :: Module_Name
    CHARACTER(LEN=256)                :: Software_File_Name
  END TYPE Gbcs_Error_Record

  ! Error Code IDs

  !   File Errors                    - 1000s
!  INTEGER, PARAMETER :: Gbcs_Error_File_Does_Not_Exist             = 1000
!  INTEGER, PARAMETER :: Gbcs_Error_Directory_Does_Not_Exist        = 1001
!
!  !   HDF5 Errors                    - 1100s
!
!  !   GRIB Errors                    - 1200s
!  INTEGER, PARAMETER :: Gbcs_Error_GRIB_Not_A_Record               = 1200
!  INTEGER, PARAMETER :: Gbcs_Error_GRIB_Bit_Maps_Not_Supported     = 1201
!  INTEGER, PARAMETER :: Gbcs_Error_GRIB_Not_End_Of_Data_Section    = 1202
!
!  !   NetCDF Errors                  - 1300s
!
!  !   SADIST-2 Errors                - 1400s
!
!  !   PDF Loading Errors             - 1500s
!  INTEGER, PARAMETER :: Gbcs_Error_Invalid_PDF_Flag_Combination    = 1500
!  INTEGER, PARAMETER :: Gbcs_Error_Invalid_Satellite_ID            = 1501
!
!  !   General Data Errors            - 2000s
!  INTEGER, PARAMETER :: Gbcs_Error_Data_Record_Not_Available       = 2000
!  INTEGER, PARAMETER :: Gbcs_Error_Data_Space_Not_Allocated        = 2001
!  INTEGER, PARAMETER :: Gbcs_Error_Data_Space_Not_Dealloacted      = 2002
!  
!  !   Initialization Errors          - 3000s
!  INTEGER, PARAMETER :: Gbcs_Error_Unkown_Satellite                = 3000
!  INTEGER, PARAMETER :: Gbcs_Error_Unknown_Forecast_Model          = 3001
!  INTEGER, PARAMETER :: Gbcs_Error_Unknown_RTM                     = 3002
!  INTEGER, PARAMETER :: Gbcs_Error_No_Satellite_Info_File          = 3010
!  INTEGER, PARAMETER :: Gbcs_Error_No_Forecast_Model_Info_File     = 3011
!  INTEGER, PARAMETER :: Gbcs_Error_No_RTM_Info_File                = 3012
!
!  !   Processing Errors              - 4000s
!  INTEGER, PARAMETER :: Gbcs_Error_No_Background_SST               = 4000
!  INTEGER, PARAMETER :: Gbcs_Error_No_Background_LST               = 4001
!  INTEGER, PARAMETER :: Gbcs_Error_No_Background_TCWV              = 4002
!  INTEGER, PARAMETER :: Gbcs_Error_Missing_Data                    = 4500
!
!  !   Mathematical Erros             - 5000s
!  INTEGER, PARAMETER :: Gbcs_Error_Division_By_Zero                = 5000
!  INTEGER, PARAMETER :: Gbcs_Error_Underflow                       = 5001
!  INTEGER, PARAMETER :: Gbcs_Error_Overflow                        = 5002
!  INTEGER, PARAMETER :: Gbcs_Error_Matrix_Inner_Dims_Unmatched     = 5010
!  INTEGER, PARAMETER :: Gbcs_Error_Matrix_Singular                 = 5011
!  INTEGER, PARAMETER :: Gbcs_Error_Matrix_Not_Invertable           = 5012
!  INTEGER, PARAMETER :: Gbcs_Error_Interpolate_dx_Too_Small        = 5020

  !   RTM Errors                     - 6000s

  CHARACTER(LEN=8), DIMENSION(5), PARAMETER, PRIVATE :: errorstatus_text = &
    (/ 'DEBUG   ', 'INFO    ',  'WARNING ', 'ERROR   ', 'CRITICAL' /)

  INTEGER, PARAMETER, PUBLIC :: Gbcs_Log_Debug    = 1
  INTEGER, PARAMETER, PUBLIC :: Gbcs_Log_Info     = 2
  INTEGER, PARAMETER, PUBLIC :: Gbcs_Log_Warning  = 3
  INTEGER, PARAMETER, PUBLIC :: Gbcs_Log_Error    = 4
  INTEGER, PARAMETER, PUBLIC :: Gbcs_Log_Critical = 5

  INTEGER, SAVE, PRIVATE     :: Gbcs_Log_Level    = 0


  INTEGER, SAVE :: Gbcs_Log_File_Unit = 6  ! Default to standard output
  INTEGER, SAVE :: Gbcs_Verbosity     = 4  ! Default maximum verbosity

CONTAINS

!------------------------------------------------------------------------------
!S+
! NAME:
!       Gbcs_Log
!
! PURPOSE:
!       Print a string to the Gbcs log file unit
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Gbcd_Log( Error_Status, Error_Message, Error_Routine, Error_Module )
!
! INPUT ARGUMENTS:
!       Error_Status
!       Error_Message
!
! OPTIONAL INPUT ARGUMENTS:
!       Error_Routine
!       Error_Module
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
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
!       Written by:   Owen Embury
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Gbcs_Log( Error_Status, &
                       Error_Message, &
                       Error_Routine, &
                       Error_Module )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    INTEGER,          INTENT(IN)           :: Error_Status
    CHARACTER(LEN=*), INTENT(IN)           :: Error_Message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Routine
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Module

    ! Ignore messages lower than current log level

    IF  (Error_Status < Gbcs_Log_Level) THEN
      RETURN
    END IF

    WRITE(Gbcs_Log_File_Unit,'(a)') Error_Message

    IF (PRESENT(Error_Routine)) THEN
      WRITE(Gbcs_Log_File_Unit,'(a)') ' ROUTINE - '//Error_Routine
    END IF
    
    IF (PRESENT(Error_Module)) THEN
      WRITE(Gbcs_Log_File_Unit,'(a)') '  MODULE - '//Error_Module
    END IF

  END SUBROUTINE Gbcs_Log

!------------------------------------------------------------------------------
!S+
! NAME:
!       Gbcs_Critical / Error / Warning / Info / Debug
!
! PURPOSE:
!       Print a string to the Gbcs log file unit is the logical flag is true.
!       In the case of Gbcs_Critical program will also be terminated.
!       Unlike direct calls to Gbcs_Log, these subroutines prefix the message
!       with the error level (CRITICAL, ERROR, etc)
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Gbcs_Critical( Error_Flag, Error_Message, Error_Routine, Error_Module )
!
! INPUT ARGUMENTS:
!       Error_Flag
!       Error_Message
!
! OPTIONAL INPUT ARGUMENTS:
!       Error_Routine
!       Error_Module
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       Gbcs_Log
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Gbcs_Critical( Error_Flag, &
                            Error_Message, &
                            Error_Routine, &
                            Error_Module )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    LOGICAL,          INTENT(IN)           :: Error_Flag
    CHARACTER(LEN=*), INTENT(IN)           :: Error_Message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Routine
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Module
  
    IF ( Error_Flag ) THEN
      CALL Gbcs_Log(Gbcs_Log_Critical, &
                    errorstatus_text(Gbcs_Log_Critical)//' : '//Error_Message, &
                    Error_Routine, &
                    Error_Module)
      STOP 1 ! -- Indicates error - exact behaviour depends on compiler, but
             !    is likely to be print '1' to stderr and/or return 1 as the
             !    exit status
    END IF
  END SUBROUTINE Gbcs_Critical

  ! ----------------

  SUBROUTINE Gbcs_Error( Error_Flag, &
                         Error_Message, &
                         Error_Routine, &
                         Error_Module )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    LOGICAL,          INTENT(IN)           :: Error_Flag
    CHARACTER(LEN=*), INTENT(IN)           :: Error_Message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Routine
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Module
  
    IF ( Error_Flag ) THEN
      CALL Gbcs_Log(Gbcs_Log_Error, &
                    errorstatus_text(Gbcs_Log_Error)//' : '//Error_Message, &
                    Error_Routine, &
                    Error_Module)
    END IF
  END SUBROUTINE Gbcs_Error

  ! ----------------

  SUBROUTINE Gbcs_Warning( Error_Flag, &
                           Error_Message, &
                           Error_Routine, &
                           Error_Module )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    LOGICAL,          INTENT(IN)           :: Error_Flag
    CHARACTER(LEN=*), INTENT(IN)           :: Error_Message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Routine
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Module
  
    IF ( Error_Flag ) THEN
      CALL Gbcs_Log(Gbcs_Log_Warning, &
                    errorstatus_text(Gbcs_Log_Warning)//' : '//Error_Message, &
                    Error_Routine, &
                    Error_Module)
    END IF
  END SUBROUTINE Gbcs_Warning

  ! ----------------

  SUBROUTINE Gbcs_Info( Error_Flag, &
                        Error_Message, &
                        Error_Routine, &
                        Error_Module )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    LOGICAL,          INTENT(IN)           :: Error_Flag
    CHARACTER(LEN=*), INTENT(IN)           :: Error_Message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Routine
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Module
  
    IF ( Error_Flag ) THEN
      CALL Gbcs_Log(Gbcs_Log_Info, &
                    errorstatus_text(Gbcs_Log_Info)//' : '//Error_Message, &
                    Error_Routine, &
                    Error_Module)
    END IF
  END SUBROUTINE Gbcs_Info

  ! ----------------

  SUBROUTINE Gbcs_Debug( Error_Flag, &
                         Error_Message, &
                         Error_Routine, &
                         Error_Module )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    LOGICAL,          INTENT(IN)           :: Error_Flag
    CHARACTER(LEN=*), INTENT(IN)           :: Error_Message
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Routine
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: Error_Module
  
    IF ( Error_Flag ) THEN
      CALL Gbcs_Log(Gbcs_Log_Debug, &
                    errorstatus_text(Gbcs_Log_Debug)//' : '//Error_Message, &
                    Error_Routine, &
                    Error_Module)
    END IF
  END SUBROUTINE Gbcs_Debug


!------------------------------------------------------------------------------
!S+
! NAME:
!       Gbcs_SetLogLevel
!
! PURPOSE:
!       Changes the current log level, to allow more/fewer messages to be outputted
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Gbcs_SetLogLevel( Log_Level )
!
! INPUT ARGUMENTS:
!       Log_Level
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
! CALLS:
!       None
!
! SIDE EFFECTS:
!       Changes the current Gbcs_Log_Level
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Gbcs_SetLogLevel( Log_Level )

    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    INTEGER,         INTENT(IN)           :: Log_Level
  
    Gbcs_Log_Level = Log_Level
    
  END SUBROUTINE Gbcs_SetLogLevel
  

!------------------------------------------------------------------------------
!S+
! NAME:
!       Gbcs_SetLogFile
!
! PURPOSE:
!       Opens a new logfile
!
! CATEGORY:
!       Error utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Gbcs_SetLogFile( File_Name )
!
! INPUT ARGUMENTS:
!       File_Name
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
! CALLS:
!       None
!
! SIDE EFFECTS:
!       Changes the log file
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury
!                     IAES, University of Edinburgh
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Gbcs_SetLogFile( File_Name )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN = *), INTENT(IN)           :: File_Name
  
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Gbcs_SetLogFile'

    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER(LEN=100) :: Log_file
    LOGICAL            :: Unit_Used


    IF (Gbcs_Log_File_Unit /= 6) CLOSE(Gbcs_Log_File_Unit)
    
    Log_file           = TRIM(File_Name) // ".log"
    ! -- Find a free file unit
    Gbcs_Log_File_Unit = 90
    Unit_Used = .TRUE.
    DO WHILE( Unit_Used )
      Gbcs_Log_File_Unit = Gbcs_Log_File_Unit + 1
      INQUIRE(UNIT=Gbcs_Log_File_Unit, OPENED=Unit_Used)
    END DO
   
    OPEN( UNIT=Gbcs_Log_File_Unit, FILE=TRIM(Log_file), FORM="FORMATTED" )
    
  END SUBROUTINE Gbcs_SetLogFile


END MODULE GbcsErrorHandler
