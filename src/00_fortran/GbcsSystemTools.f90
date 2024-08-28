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



!+ This module contains useful system functions

MODULE GbcsSystemTools

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

! Modules used:

  USE GbcsKinds
  USE GbcsPath, ONLY: File_Exists
  IMPLICIT NONE

  CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsSystemTools.mod'


CONTAINS


  ! System number to string converters

  SUBROUTINE Int2Str( Value , Str )

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
  ! 0.0   28/09/2006    Creation                                                   CPO
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

    IMPLICIT NONE

    INTEGER ,          INTENT (IN   ) :: Value
    CHARACTER(LEN=*) , INTENT (  OUT) :: Str

    INTEGER :: Dec_Val
    INTEGER :: New_Val
    INTEGER :: Dec_Num
    INTEGER :: Sgn
    INTEGER :: ii

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Int2Str'

      IF ( Value .NE. 0 ) THEN

        Sgn = 0
        IF ( Value < 0 ) Sgn = 1

        Dec_Num = INT( LOG10( ABS( REAL( Value ) ) ) ) + 1

        New_Val = ABS( Value )

        Str = ''

        DO ii = 1,Dec_Num

          Dec_Val = MOD( New_Val , 10 )

          Str = CHAR( Dec_Val + ICHAR('0') ) // TRIM( Str )

          New_Val = INT( New_Val / 10 )

        END DO

        Str = TRIM( Str )
        IF ( Sgn == 1 ) Str = '-'//TRIM( Str )

      ELSE

        Str = '0'

      END IF

    RETURN

  END SUBROUTINE Int2Str


  ! System Time Subroutines

  SUBROUTINE Get_Time( T_Stamp )

  !
  ! Description:
  !
  ! Get the current local time from the computer system clock.
  ! Converts the time string into a real.
  !
  ! Method:
  !
  ! Date and time information are retrieved using the FORTRAN 'DATE_AND_TIME' 
  ! intrinsic subroutine.
  !
  ! The time data is converted to a floating point number using:
  ! 
  !              m       s        s1000
  !    T =  h + ---- + ------ + ---------    ( hours )  
  !              60     3600     3600000
  !
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

  ! Modules used:

  USE GbcsTypes

  IMPLICIT NONE

    TYPE( Time_Stamp ) :: T_Stamp
    INTEGER, DIMENSION(8) :: VALUES

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Get_Time'

      CALL DATE_AND_TIME( T_Stamp%CDATE , T_Stamp%CTIME , T_Stamp%CZONE , T_Stamp%VALUES )

      VALUES = T_Stamp%VALUES
      T_Stamp%RTIME = VALUES(5) + ( VALUES(6) + ( VALUES(7) + ( VALUES(8) / 1000.0 ) ) / 60.0 ) / 60.0

    RETURN

  END SUBROUTINE Get_Time



  FUNCTION Get_Elapsed_Time( Start_Time , Stop_Time )

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

  ! Modules used:

    REAL :: Get_Elapsed_Time

    REAL :: Start_Time
    REAL :: Stop_Time

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Get_Elapsed_Time'

      IF ( Stop_Time < Start_Time ) Stop_Time = Stop_Time + 24.0

      Get_Elapsed_Time = Stop_Time - Start_Time

    RETURN

  END FUNCTION Get_Elapsed_Time


  SUBROUTINE Convert_HMS_To_Str( Time , TimeStr )

  !
  ! Description:
  !
  ! Method:
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   15/03/2006    Creation                                                   CPO
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

    REAL,             INTENT (IN   ) :: Time
    CHARACTER(LEN=*), INTENT (  OUT) :: TimeStr

    REAL    :: rm,rs
    INTEGER :: h,m,s,ms

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Convert_HMS_To_Str'

      h = INT( Time )
      rm = ( Time - INT( Time ) ) * 60.0
      m = INT( rm )
      rs = ( rm - INT( rm ) ) * 60.0
      s = INT( rs )
      ms = INT( ( rs - INT( rs ) ) * 1000.0 )

      TimeStr = CHAR(INT(h/10.0)+48)//CHAR(MOD(h,10)+48)//':'               &
                 //CHAR(INT(m/10.0)+48)//CHAR(MOD(m,10)+48)//':'            &
                 //CHAR(INT(s/10.0)+48)//CHAR(MOD(s,10)+48)//'.'            &
                 //CHAR(INT(ms/100.0)+48)//CHAR(INT(MOD(ms,100)/10.0)+48)//CHAR(MOD(MOD(ms,100),10)+48)

    RETURN

  END SUBROUTINE Convert_HMS_To_Str


  SUBROUTINE Convert_YMD_To_Str( Year , Month , Day , TimeStr )

  !
  ! Description:
  !
  ! Method:
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   15/03/2006    Creation                                                   CPO
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

    INTEGER,          INTENT (IN   ) :: Year, Month, Day
    CHARACTER(LEN=*), INTENT (  OUT) :: TimeStr

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Convert_YMD_To_Str'

      WRITE( UNIT=TimeStr , FMT='(I4,1X,I0.2,1X,I0.2)' ) Year , Month , Day

    RETURN

  END SUBROUTINE Convert_YMD_To_Str


  ! Sytem File Unit 

  FUNCTION Get_New_File_Unit()

  !
  ! Description:
  !
  !  Retrieves an unused file unit number
  !
  ! Method:
  !
  !  Starting from unit=100 search in steps of 10 for an unused file unit
  !  Useage checked using the INQUIRE function
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   15/03/2006    Creation                                                   CPO
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

    INTEGER :: Get_New_File_Unit
    LOGICAL :: Unit_Used
    INTEGER :: A_Unit
    
    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Get_New_File_Unit'

      A_UNIT = 90
     
      Unit_Used = .TRUE.

      DO WHILE( Unit_Used )

        A_Unit = A_Unit + 10
        INQUIRE ( UNIT=A_Unit , OPENED=Unit_Used )

      END DO

      Get_New_File_Unit = A_Unit

    RETURN
 
  END FUNCTION Get_New_File_Unit

  
  
  ! System byte unpacking

  INTEGER FUNCTION Unpack_Bytes( bytes )

  !
  ! Description:
  !
  !  Unpacks 1, 2 and 4 byte records into 4 byte integers
  !
  ! Method:
  !
  !  Assumes little endian data
  !  Assumes 1 and 2 byte integers are unsigned
  !  Sets the bits in the appropriate byte of the 4 byte integer based on the input bytes
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   15/03/2006    Creation                                                   CPO
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

    INTEGER(KIND=GbcsInt1),DIMENSION(:) :: bytes

    INTEGER :: nbytes
    INTEGER :: usnum
    INTEGER :: binexp
    INTEGER :: bnum
    INTEGER :: num

    INTEGER :: ii  

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Unpack_Bytes'


      num = 0

      nbytes = SIZE(bytes)

      DO ii = 1,nbytes

        binexp = ( ii - 1 ) * 8

        usnum = 0
        DO bnum = 0,7
          IF (BTEST(bytes(ii),bnum)) usnum = IBSET(usnum,bnum)
        END DO

        num = num + usnum * 2**binexp

      END DO

      Unpack_Bytes = num

    RETURN

  END FUNCTION Unpack_Bytes


  INTEGER FUNCTION Signed_Unpack_Bytes( bytes )

  !
  ! Description:
  !
  !  Unpacks 1, 2 and 4 byte records into 4 byte integers
  !
  ! Method:
  !
  !  Assumes little endian data
  !  Assumes 1 and 2 byte integers are signed
  !
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   15/03/2006    Creation                                                   CPO
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

    INTEGER(KIND=GbcsInt1),DIMENSION(:) :: bytes

    INTEGER :: nbytes
    INTEGER(KIND=GbcsInt1) :: num1
    INTEGER(KIND=GbcsInt2) :: num2
    INTEGER :: usnum
    INTEGER :: binexp
    INTEGER :: bnum
    INTEGER :: num

    INTEGER :: ii  

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Signed_Unpack_Bytes'


      num = 0

      nbytes = SIZE(bytes)

      DO ii = 1,nbytes

        binexp = ( ii - 1 ) * 8

        usnum = 0
        DO bnum = 0,7
          IF (BTEST(bytes(ii),bnum)) usnum = IBSET(usnum,bnum)
        END DO

        num = num + usnum * 2**binexp

      END DO

      SELECT CASE ( nbytes )

        CASE ( 1 ) 

          num1 = INT( num , KIND=GbcsInt1 )
          num = INT( num1 , KIND=GbcsInt4 )

        CASE ( 2 )

          num2 = INT( num , KIND=GbcsInt2 )
          num = INT( num2 , KIND=GbcsInt4 )

      END SELECT

      Signed_Unpack_Bytes = num

    RETURN

  END FUNCTION Signed_Unpack_Bytes


END MODULE GbcsSystemTools
