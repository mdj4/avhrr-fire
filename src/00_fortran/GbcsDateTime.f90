!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsDateTime
!
! PURPOSE:
!       Convert dates between various formats, determine day of year etc.
!
!       The main purpose of writting this module is to merge parts of code
!       from GbcsTimeSpace and GbcsSystemTools.
!       Also borrows from Paul van Delst's Date_Utility module
!
!       functions are declared elemental where ever possible
!
! NOTES:
!       Might change from Julian to Modified Julian Dates
!
!       Could move Get_Time back to SystemTools
!
!       Create a GbcsStringUtil for integer->string and date->string functions??
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsDateTime
!
! MODULES:
!       GbcsKinds
!
! CONTAINS:
!       Is_Leap_Year
!       Day_Of_Year
!       Day_Number_To_Date
!       Unix_To_JD
!       JD_To_Date
!       Date_To_JD
!       Get_Time
!       Time_Difference
!       Elapsed_Time
!       DateStr
!       TimeStr
!       parse_isodate
!
! DERIVED TYPES:
!       DateTime
!
! CREATION HISTORY:
!       Written by:   Owen Embury 26/02/2008
!                     IAES, University of Edinburgh
!
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
!M-
!----------------------------------------------------------------------------------


MODULE GbcsDateTime
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds
  
  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE
  
  INTERFACE Day_Of_Year
    MODULE PROCEDURE Day_Of_Year_int, Day_Of_Year_struct
  END INTERFACE Day_Of_Year

  INTERFACE JulianDay
    MODULE PROCEDURE JulianDay_int, JulianDay_struct
  END INTERFACE JulianDay

  INTERFACE OPERATOR (-)
    MODULE PROCEDURE DateTime_Difference
  END INTERFACE

  INTERFACE OPERATOR (==)
    MODULE PROCEDURE DateTime_Equal
  END INTERFACE

  PRIVATE :: number_strlen
  
  ! -------------
  ! Derived Types
  ! -------------
  TYPE DateTime
    INTEGER :: Year
    INTEGER :: Month      = 1
    INTEGER :: Day        = 1
    INTEGER :: Hour       = 0
    INTEGER :: Minute     = 0
    INTEGER :: Seconds    = 0
    INTEGER :: Sec1000    = 0
    INTEGER :: utc_offset = 0
  END TYPE DateTime

  TYPE TimeDelta
    INTEGER :: days
    INTEGER :: msec
  END TYPE TimeDelta

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Day of year corresponding to start of each month. NB add one to
  !    get the DoY corresponding to the 1st of each month
  INTEGER, PRIVATE, PARAMETER, DIMENSION(12) :: Month_Day_Number = &
    (/ 0,   31,  59,  90,  120, 151, 181, 212, 243, 273, 304, 334 /)

  ! -- Number of days per month in a non leap year
  INTEGER, PRIVATE, PARAMETER, DIMENSION(12) :: Days_Per_Month = &
    (/ 31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 /)

  ! -- Julian day number for start of Gregorian calendar (15/10/1582)
  INTEGER,             PRIVATE, PARAMETER :: start_gregorian = 2299161

  REAL(KIND=GbcsDble), PRIVATE, PARAMETER :: mjd_offset = 2400000.5_GbcsDble

  ! -- Julian day of unix epoch (1/1/1970)
  REAL(KIND=GbcsDble), PRIVATE, PARAMETER :: unix_epoch = 2440587.5_GbcsDble

  REAL(KIND=GbcsDble), PRIVATE, PARAMETER :: seconds_per_day = 86400.0_GbcsDble
  REAL(KIND=GbcsDble), PRIVATE, PARAMETER :: sec1000_per_day = 86400000.0_GbcsDble

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMisc/GbcsDateTime.f90'

CONTAINS

!------------------------------------------------------------------------------
!F+
! NAME:
!       Is_Leap_Year
!
! PURPOSE:
!       Function to determine if a specified year is a leap year.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Is_Leap_Year( Year )
!
! INPUT ARGUMENTS:
!       Year:   Integer specifying year
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
! RESULT:
!       .TRUE.  if year is a leap year
!       .FALSE. otherwise
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 26/02/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Leap_Year(Year) & 
                          RESULT (Leap_Year)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT( IN ) :: Year

    ! ------
    ! Result
    ! ------
    LOGICAL :: Leap_Year
    
    Leap_Year = .FALSE.    
    IF( (MOD(Year, 4)  ==0 .AND. MOD(Year, 100)/=0) .OR. &
        (MOD(Year, 400)==0) ) THEN
      Leap_Year = .TRUE.
    END IF

  END FUNCTION Is_Leap_Year


!------------------------------------------------------------------------------
!F+
! NAME:
!       Day_Of_Year_int
!
! PURPOSE:
!       Convert yyyy/mm/dd to day of year
!       Performs sanity check on yyyy and mm, but does not check dd
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Day_Of_Year_int(Year, Month, Day)
!
! INPUT ARGUMENTS:
!       Year:   Integer
!       Month:  Integer
!       Day:    Integer
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
! RESULT:
!       -1      If year or month is invalid
!       Day of year otherwise
!
! CALLS:
!       Is_Leap_Year
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 26/02/2008
!                     IAES, University of Edinburgh
!       08/08/11  OE  Clean up and rename
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Day_Of_Year_int(Year, Month, Day) RESULT (Day_Of_Year)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT(IN) :: Year
    INTEGER, INTENT(IN) :: Month
    INTEGER, INTENT(IN) :: Day

    ! ------
    ! Result
    ! ------
    INTEGER :: Day_Of_Year

    ! -- Check if date is valid
    IF((Year < 1) .OR. (Month < 1) .OR. (Month > 12)) THEN
      Day_Of_Year = -1
      RETURN
    END IF

    Day_Of_Year = Month_Day_Number( Month ) + Day

    IF( (Month > 2) .AND. Is_Leap_Year(Year) ) Day_Of_Year = Day_Of_Year + 1

  END FUNCTION Day_Of_Year_int


!------------------------------------------------------------------------------
!F+
! NAME:
!       Day_Of_Year_struct
!
! PURPOSE:
!       Convert yyyy/mm/dd to day of year
!       Performs sanity check on yyyy and mm, but does not check dd
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Day_Of_Year_struct( date )
!
! INPUT ARGUMENTS:
!       Year:   Integer
!       Month:  Integer
!       Day:    Integer
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
! RESULT:
!       -1      If year or month is invalid
!       Day of year otherwise
!
! CALLS:
!       Is_Leap_Year
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 26/02/2008
!                     IAES, University of Edinburgh
!       08/08/11  OE  Clean up and rename
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Day_Of_Year_struct(date) RESULT (Day_Of_Year)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date

    ! ------
    ! Result
    ! ------
    INTEGER :: Day_Of_Year
    
    Day_Of_Year = Day_Of_Year_int(date%Year, date%Month, date%Day)

  END FUNCTION Day_Of_Year_struct


!------------------------------------------------------------------------------
!S+
! NAME:
!       Day_Number_To_Date
!
! PURPOSE:
!       Calcualte mm/dd for given day of year
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Day_Number_To_Date(Year, Day_Number, Month, Day)
!
! INPUT ARGUMENTS:
!       Year:   Integer
!       Day_Number: Integer
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       Month:  Integer
!       Day:    Integer
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! CALLS:
!       Is_Leap_Year
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 27/02/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL SUBROUTINE Day_Number_To_Date(Year, Day_Number, Month, Day)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT( IN  ) :: Year
    INTEGER, INTENT( IN  ) :: Day_Number
    INTEGER, INTENT( OUT ) :: Month
    INTEGER, INTENT( OUT ) :: Day
    
    ! ---------------
    ! Local variables
    ! ---------------
    LOGICAL  ::  Leap_Year
    
    Month = 1
    Day   = Day_Number
    
    Leap_Year = Is_Leap_Year(Year)
    
    DO Month = 1, 12
      IF( Day <= Days_Per_Month(Month) ) THEN
        EXIT
      END IF
      
      IF( Month == 2 .AND. Leap_Year) THEN
        IF( Day == Days_Per_Month(Month)+1 ) THEN
          EXIT
        ELSE
          Day = Day - 1
        END IF
      END IF
      Day = Day - Days_Per_Month(Month)
    END DO

  END SUBROUTINE Day_Number_To_Date


!------------------------------------------------------------------------------
!F+
! NAME:
!       Unix_To_JD
!
! PURPOSE:
!       Convert a unix time (seconds) to Julian date
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       jday = Unix_To_JD( time )
!
! INPUT ARGUMENTS:
!       time - integer (seconds)
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
! RESULT:
!       jday - Julian day
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 20/07/2009
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Unix_To_JD( time ) & 
                        RESULT ( jday )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT(IN) :: time

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsDble) :: jday
    
    jday = unix_epoch + time / seconds_per_day

  END FUNCTION Unix_To_JD


!------------------------------------------------------------------------------
!F+
! NAME:
!       JulianDay
!
! PURPOSE:
!       Calculate the Julian Day for a given year, month, day
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       day = JulianDay(year, month, day)
!
! ARGUMENTS:
!       year:   integer
!       month:  integer
!       day:    integer
!
! RESULT:
!       Julian Day: integer
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       Assumes Julian calender up to 05/10/1582 followed by Gregorian
!       calendar from 15/10/1582 onwards
!
! PROCEDURE:
!       Meeus, Jean (1998) Astronomical Algorithms
!
! CREATION HISTORY:
!       Written by:   Owen Embury 03/04/2014
!                     University of Reading
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION JulianDay_int(year, month, day) RESULT(JulianDay)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT(IN) :: year
    INTEGER, INTENT(IN) :: month
    INTEGER, INTENT(IN) :: day

    ! ------
    ! Result
    ! ------
    INTEGER :: JulianDay

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: jy, jm, jd, ja

    IF( month <= 2 )THEN
      jy = year - 1
      jm = month + 13
      jd = day
    ELSE
      jy = year
      jm = month + 1
      jd = day
    END IF

    ! -- Adjust for missing 'year zero'
    IF( jy < 0 )THEN
      jy = jy + 1
    END IF

    JulianDay = FLOOR(365.25*jy) + FLOOR(30.6001*jm) + jd + 1720995

    ! -- Correct for leap years
    IF( JulianDay > start_gregorian ) THEN
      ja = FLOOR(jy * 0.01)
      JulianDay = JulianDay + 2 - ja + FLOOR(ja * 0.25)
    END IF

  END FUNCTION JulianDay_int

  ELEMENTAL FUNCTION JulianDay_struct(date) RESULT(JulianDay)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date

    ! ------
    ! Result
    ! ------
    INTEGER :: JulianDay
    
    JulianDay = JulianDay_int(date%year, date%month, date%day)
  END FUNCTION JulianDay_struct


!------------------------------------------------------------------------------
!F+
! NAME:
!       MillisecDay
!
! PURPOSE:
!       Return the number of milliseconds from midday
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       day = MillisecDay(date)
!
! ARGUMENTS:
!       date:   DateTime
!
! RESULT:
!       Number of milliseconds from midday
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 03/04/2014
!                     University of Reading
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION MillisecDay(date)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date

    ! ------
    ! Result
    ! ------
    INTEGER :: MillisecDay

    MillisecDay = (date%hour-12)* 3600000 + &
                   date%minute  * 60000   + &
                   date%seconds * 1000    + &
                   date%sec1000

  END FUNCTION MillisecDay


!------------------------------------------------------------------------------
!F+
! NAME:
!       Date_YD
!
! PURPOSE:
!       Return a DateTime given a year and day-of-year
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       date = Date_YD(year, doy)
!
! ARGUMENTS:
!       year:   INTEGER
!       doy:    INTEGER
!
! RESULT:
!       DateTime
!
! CALLS:
!       Day_Number_To_Date
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 03/04/2014
!                     University of Reading
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Date_YD(year, doy) RESULT(date)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    INTEGER, INTENT(IN) :: year
    INTEGER, INTENT(IN) :: doy

    ! ------
    ! Result
    ! ------
    TYPE(DateTime) :: date

    date%year  = year
    CALL Day_Number_To_Date(year, doy, date%month, date%day)

  END FUNCTION Date_YD


!------------------------------------------------------------------------------
!F+
! NAME:
!       DateTime_Difference
!
! PURPOSE:
!       Return the difference between two DateTimes
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       delta = DateTime_Difference(date1, date2)
!
! ARGUMENTS:
!       date1:  DateTime
!       date2:  DateTime
!
! RESULT:
!       TimeDelta
!
! CALLS:
!       JulianDay
!       MillisecDay
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 03/04/2014
!                     University of Reading
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION DateTime_Difference(date1, date2) RESULT(delta)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date1
    TYPE(DateTime), INTENT(IN) :: date2

    ! ------
    ! Result
    ! ------
    TYPE(TimeDelta) :: delta

    delta%days = JulianDay(date1)   - JulianDay(date2)
    delta%msec = MillisecDay(date1) - MillisecDay(date2) - &
                   (date1%utc_offset-date2%utc_offset)*60000

  END FUNCTION DateTime_Difference


!------------------------------------------------------------------------------
!F+
! NAME:
!       DateTime_Equal
!
! PURPOSE:
!       Check if two DateTimes are identical
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       equal = DateTime_Equal(date1, date2)
!
! ARGUMENTS:
!       date1:  DateTime
!       date2:  DateTime
!
! RESULT:
!       LOGICAL
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 03/04/2014
!                     University of Reading
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION DateTime_Equal(date1, date2) RESULT(equal)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date1
    TYPE(DateTime), INTENT(IN) :: date2

    ! ------
    ! Result
    ! ------
    LOGICAL :: equal
    
    equal = .FALSE.

    IF (date1%year       == date2%year      .AND. &
        date1%month      == date2%month     .AND. &
        date1%day        == date2%day       .AND. &
        date1%hour       == date2%hour      .AND. &
        date1%minute     == date2%minute    .AND. &
        date1%seconds    == date2%seconds   .AND. &
        date1%sec1000    == date2%sec1000   .AND. &
        date1%utc_offset == date2%utc_offset    ) &
            equal = .TRUE.

  END FUNCTION DateTime_Equal

!------------------------------------------------------------------------------
!F+
! NAME:
!       JD_To_Date
!
! PURPOSE:
!       Convert Julian date to DateTime structure
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       date = JD_To_Date( jday )
!
! ARGUMENTS:
!       jday:   real
!
! RESULT:
!       DateTime
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 27/02/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION JD_To_Date( jday ) & 
                        RESULT ( date )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), INTENT( IN ) :: jday

    ! ------
    ! Result
    ! ------
    TYPE(DateTime) :: date

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: Z, alpha
    INTEGER :: A, B, C, D, E
    REAL(KIND=GbcsDble)    :: jd, F
    
    jd = jday + 0.5
    Z = FLOOR(jd)
    F = jd - Z
    IF( jday < start_gregorian )THEN
      A = Z
    ELSE
      alpha = FLOOR(((Z-1867216)-0.25d0) / 36524.25d0)
      A     = Z + 1 + alpha - FLOOR(alpha*0.25d0)
    END IF
    B = 1524  + A
    C = 6680  + FLOOR(((B-2439870)-122.1d0)/365.25d0)
    D = 365*C + FLOOR(0.25d0 * C)
    E = FLOOR( (B-D)/30.6001d0 )
    
    date%Day = B - D - FLOOR(30.6001d0*E)

    IF( E < 13.5 )THEN
      date%Month = E - 1
    ELSE
      date%Month = E - 13
    END IF
    
    IF( date%Month < 2.5 )THEN
      date%Year = C - 4715
    ELSE
      date%Year = C - 4716
    END IF

    F = F * 24.0
    date%Hour    = FLOOR(F)
    F = (F - date%Hour)   * 60.0
    date%Minute  = FLOOR(F)
    F = (F - date%Minute) * 60.0
    date%Seconds = FLOOR(F)
    F = (F - date%Seconds) * 1000.0
    date%Sec1000 = FLOOR(F)

    date%utc_offset = 0
    
  END FUNCTION JD_To_Date


!------------------------------------------------------------------------------
!F+
! NAME:
!       Date_To_JD
!
! PURPOSE:
!       Convert DateTime to Julian date
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       jday = Date_To_JD( date )
!
! ARGUMENTS:
!       date:   DateTime
!
! RESULT:
!       Julian date REAL
!
! CALLS:
!       JulianDay
!       MillisecDay
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 27/02/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Date_To_JD( date ) & 
                        RESULT ( jday )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT( IN ) :: date

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsDble) :: jday

    
    jday = JulianDay(date) + MillisecDay(date) / sec1000_per_day &
           - (date%utc_offset*60.0_GbcsDble)/seconds_per_day

  END FUNCTION Date_To_JD


!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Time
!
! PURPOSE:
!       Get current time in DateTime format
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Get_Time(CurTime)
!
! INPUT ARGUMENTS:
!       None
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       CurTime
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! RESULT:
!       Current system time
!
! CALLS:
!       date_and_time
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 26/02/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  SUBROUTINE Get_Time(CurTime)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE( DateTime ), INTENT(OUT) :: CurTime

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Get_Time'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION(8)  ::  Date_Values
    
    CALL date_and_time(values = Date_Values)

    CurTime%Year      = Date_Values(1)
    CurTime%Month     = Date_Values(2)
    CurTime%Day       = Date_Values(3)
    CurTime%utc_offset= Date_Values(4)
    CurTime%Hour      = Date_Values(5)
    CurTime%Minute    = Date_Values(6)
    CurTime%Seconds   = Date_Values(7)
    CurTime%Sec1000   = Date_Values(8)

  END SUBROUTINE Get_Time


!------------------------------------------------------------------------------
!F+
! NAME:
!       Time_Difference
!
! PURPOSE:
!       Calculate difference in seconds between two DateTimes
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       delta = Time_Difference( time0, time1 )
!
! INPUT ARGUMENTS:
!       time0:  DateTime
!       time1:  DateTime
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
! RESULT:
!       Time difference in seconds
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 21/07/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Time_Difference( time0, time1 ) & 
                           RESULT( delta )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT( IN ) :: time0
    TYPE(DateTime), INTENT( IN ) :: time1

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsDble) :: delta

    delta = seconds_per_day * (Date_To_JD(time1) - Date_To_JD(time0))

  END FUNCTION Time_Difference

  FUNCTION Elapsed_Time( time ) &
                 RESULT( delta )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(INOUT) :: time

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsDble) :: delta

    ! ---------------
    ! Local Variables
    ! ---------------
    TYPE(DateTime) :: new_time

    CALL Get_Time(new_time)
    delta = Time_Difference(time, new_time)
    time = new_time
  END FUNCTION Elapsed_Time


!------------------------------------------------------------------------------
!F+
! NAME:
!       DateStr, TimeStr
!
! PURPOSE:
!       Convert the given DateTime structure to a string
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       string = DateStr(time)
!       string = TimeStr(time)
!
! INPUT ARGUMENTS:
!       time:  DateTime
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
! RESULT:
!       String containing either the date or time part of the date time struct
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       11/04/11  OE  Added header, removed dependency on GbcsStringUtil
!F-
!------------------------------------------------------------------------------
  PURE FUNCTION DateStr( date ) &
                 RESULT( string )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date
    
    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=10)          :: string
    
    WRITE(string,FMT='(I4.4,"-",I2.2,"-",I2.2)') date%year, date%month, date%day
    
  END FUNCTION DateStr

  PURE FUNCTION TimeStr( date ) &
                 RESULT( string )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(DateTime), INTENT(IN) :: date
    
    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=8)          :: string
    
    WRITE(string,FMT='(I2.2,":",I2.2,":",I2.2)') date%hour, date%minute, date%seconds
    
  END FUNCTION TimeStr


!------------------------------------------------------------------------------
!F+
! NAME:
!       parse_isodate
!
! PURPOSE:
!       Parse a text string following ISO 8601 format and return the date/time
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       date = parse_isodate(string, iostat)
!
! INPUT ARGUMENTS:
!       string
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       iostat
!
! RESULT:
!       DateTime object
!
! CALLS:
!       
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 11/04/2011
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  PURE SUBROUTINE number_strlen(string, length, terminator)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*),  INTENT(IN)  :: string
    INTEGER,           INTENT(OUT) :: length
    CHARACTER,         INTENT(OUT) :: terminator

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER(LEN=10), PARAMETER :: digits          = '0123456789'
    
    length = VERIFY(string, digits) - 1
    
    IF (length == -1) THEN
      length = LEN(string)
      terminator = ''
    ELSE
      terminator = string(length+1:length+1)
    END IF
    
  END SUBROUTINE

  FUNCTION parse_isodate(string, iostat) RESULT(date)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*),  INTENT(IN)  :: string
    INTEGER, OPTIONAL, INTENT(OUT) :: iostat
    
    ! ---------
    ! Function result
    ! ---------
    TYPE(DateTime) :: date
    
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: date_separators = '-/'
    CHARACTER( * ),  PARAMETER :: time_separators = ':'
    CHARACTER( * ),  PARAMETER :: deci_separators = '.,'
    CHARACTER( * ),  PARAMETER :: part_separators = ' T'
    CHARACTER( * ),  PARAMETER :: zone_separators = 'Z+-'
    CHARACTER(LEN=10), PARAMETER :: digits          = '0123456789'

    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER :: sep1, sep2, sep3
    INTEGER   :: i1, i2, i3
    INTEGER   :: len1, len2, len3
    INTEGER   :: n_time_elements
    INTEGER   :: tz_sign, tz_hour, tz_mins
    REAL      :: frac_time
    INTEGER   :: status

    status = 0
    date = DateTime(1900)
    
    ! -- Find first digit in string
    i1   = SCAN(string, digits)
    CALL number_strlen(string(i1:), len1, sep1)
    
    ! ---------------
    ! Parse date part
    ! ---------------
    IF (len1 == 4 .AND. VERIFY(sep1,date_separators) == 0) THEN
      i2   = i1 + len1 + 1
      CALL number_strlen(string(i2:), len2, sep2)
      
      i3   = i2 + len2 + 1
      CALL number_strlen(string(i3:), len3, sep3)
      
      SELECT CASE (len2)
        CASE (2)
          len1 = len1 + len2 + 1
          IF (sep1 == sep2) THEN
            IF (len3 /= 2) status = -1
            len1 = len1 + len3 + 1
          END IF
        
        CASE (3)
          len1 = len1 + len2 + 1
        
        CASE DEFAULT
          status = -1
      END SELECT
      
      IF (status == 0) THEN
        SELECT CASE (len1)
          CASE (7)
            ! -- Year-month
            READ(string(i1:), FMT='(I4,1X,I2)') date%year, date%month

          CASE (8)
            ! -- Eight digit field is year and day of year
            READ(string(i1:), FMT='(I4,1X,I3)') date%year, date%day
            CALL Day_Number_To_Date(date%year, date%day, date%month, date%day)

          CASE (10)
            READ(string(i1:), FMT='(I4,1X,I2,1X,I2)') date%year, date%month, date%day
          
          CASE DEFAULT
            status = -1
        END SELECT
      END IF

    ELSE
      SELECT CASE (len1)
        CASE (4)
          ! -- Just year
          READ(string(i1:), FMT='(I4)') date%year

        CASE (7)
          ! -- Seven digit number is year and day of year
          READ(string(i1:), FMT='(I4,I3)') date%year, date%day
          CALL Day_Number_To_Date(date%year, date%day, date%month, date%day)

        CASE (8)
          ! -- Eight digit number is yyyymmdd
          READ(string(i1:), FMT='(I4,I2,I2)') date%year, date%month, date%day
          
        CASE DEFAULT
          status = -1
      END SELECT
    END IF
    
    IF (PRESENT(iostat)) iostat = status
    IF (status /= 0) RETURN
    IF (i1+len1 > LEN_TRIM(string)) RETURN
    IF (VERIFY(string(i1+len1:i1+len1), part_separators) /= 0) RETURN

    ! -- Find time digits in string
    i1 = i1 + len1 + 1
    CALL number_strlen(string(i1:), len1, sep1)

    ! ---------------
    ! Parse time part
    ! ---------------
    IF (len1 == 2 .AND. VERIFY(sep1,time_separators) == 0) THEN
      i2   = i1 + len1 + 1
      CALL number_strlen(string(i2:), len2, sep2)
      
      i3   = i2 + len2 + 1
      CALL number_strlen(string(i3:), len3, sep3)

      IF (len2 == 2) THEN
        len1 = len1 + 3
        IF (sep1 == sep2) THEN
          IF (len3 == 2) THEN
            len1 = len1 + 3
          ELSE
            status = -1
          END IF
        END IF
      ELSE
        status = -1
      END IF
      
      IF (status == 0) THEN
        SELECT CASE (len1)
          CASE (5)
            READ(string(i1:), FMT='(I2,1X,I2)') date%hour, date%minute
            n_time_elements = 2
          
          CASE (8)
            READ(string(i1:), FMT='(I2,1X,I2,1X,I2)') date%hour, date%minute, date%seconds
            n_time_elements = 3
          
          CASE DEFAULT
            status = -1
        END SELECT
      END IF

    ELSE
      SELECT CASE (len1)
        CASE (2)
          READ(string(i1:), FMT='(I2)') date%hour
          n_time_elements = 1

        CASE (4)
          READ(string(i1:), FMT='(I2,I2)') date%hour, date%minute
          n_time_elements = 2

        CASE (6)
          READ(string(i1:), FMT='(I2,I2,I2)') date%hour, date%minute, date%seconds
          n_time_elements = 3

        CASE DEFAULT
          status = -1
      END SELECT
    END IF
    
    IF (PRESENT(iostat)) iostat = status
    IF (status /= 0) RETURN
    IF (i1+len1 > LEN_TRIM(string)) RETURN
    
    ! ---------------
    ! Do we have a fractional time?
    ! ---------------
    IF (VERIFY(string(i1+len1:i1+len1), deci_separators) == 0) THEN
      i1 = i1 + len1 + 1
      CALL number_strlen(string(i1:), len1, sep1)
      READ(string(i1:i1+len1-1), FMT=*) frac_time
      frac_time = frac_time / (10**len1)
      
      IF (n_time_elements == 1) THEN
        date%minute = FLOOR(frac_time * 60)
        frac_time = 60*frac_time - date%minute
        n_time_elements = 2
      END IF

      IF (n_time_elements == 2) THEN
        date%seconds = FLOOR(frac_time * 60)
        frac_time = 60*frac_time - date%seconds
        n_time_elements = 3
      END IF
      
      IF (n_time_elements == 3) THEN
        date%sec1000 = FLOOR(frac_time * 1000)
        frac_time = 1000*frac_time - date%sec1000
      END IF
      
    END IF
    
    IF (PRESENT(iostat)) iostat = status
    IF (status /= 0) RETURN
    IF (i1+len1 > LEN_TRIM(string)) RETURN
    IF (VERIFY(string(i1+len1:i1+len1), zone_separators) /= 0) RETURN

    ! -- Not needed as default tz is 0
    IF (string(i1+len1:i1+len1) == 'Z') THEN
      date%utc_offset = 0
      RETURN
    END IF

    tz_sign = 0
    tz_hour = 0
    tz_mins = 0

    IF (string(i1+len1:i1+len1) == '+') tz_sign = 1
    IF (string(i1+len1:i1+len1) == '-') tz_sign = -1

    ! -- Find zone digits in string
    i1 = i1 + len1 + 1
    CALL number_strlen(string(i1:), len1, sep1)

    IF (len1 == 2 .AND. VERIFY(sep1,time_separators) == 0) THEN
      i2   = i1 + len1 + 1
      CALL number_strlen(string(i2:), len2, sep2)
      
      IF (len2 == 2) THEN
        len1 = len1 + 3
        READ(string(i1:), FMT='(I2,1X,I2)') tz_hour, tz_mins
      ELSE
        status = -1
      END IF
      
    ELSE
      SELECT CASE (len1)
        CASE (2)
          READ(string(i1:), FMT='(I2)') tz_hour

        CASE (4)
          READ(string(i1:), FMT='(I2,I2)') tz_hour, tz_mins

        CASE DEFAULT
          status = -1
      END SELECT
    END IF
    
    IF (status == 0) date%utc_offset = tz_sign * (tz_hour*60 + tz_mins)
    
    IF (PRESENT(iostat)) iostat = status

  END FUNCTION parse_isodate



!------------------------------------------------------------------------------
!S+
! NAME:
!       write_isodate
!
! PURPOSE:
!       Write a DateTime struct to a string in ISO 8601 format
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       date = write_isodate(string, date)
!
! INPUT ARGUMENTS:
!       date
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       None
!
! OPTIONAL OUTPUT ARGUMENTS:
!       string
!
!
! CALLS:
!       
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 11/04/2011
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  PURE SUBROUTINE write_isodate(string, date, iso_format)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*),  INTENT(OUT) :: string
    TYPE(DateTime),    INTENT(IN)  :: date
    INTEGER, OPTIONAL, INTENT(IN)  :: iso_format
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER          :: iso_fmt
    INTEGER          :: tz
    CHARACTER(LEN=5) :: tz_str
    
    tz = date%utc_offset/60
    tz = tz*100 + (date%utc_offset - tz*60)
    WRITE(tz_str,'(I5.5)') tz
    
    IF (tz < 0) THEN
      tz_str(1:1) = '-'
    ELSE
      tz_str(1:1) = '+'
    END IF
    
    IF (tz == 0) tz_str = 'Z'
    
    iso_fmt = 0
    IF (PRESENT(iso_format)) iso_fmt = iso_format

    SELECT CASE (iso_fmt)
    CASE (1)
      WRITE(string,FMT='(I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2,A)') &
        date%year, date%month,  date%day,     &
        date%hour, date%minute, date%seconds, &
        tz_str

    CASE DEFAULT
      WRITE(string,FMT='(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,A)') &
        date%year, date%month,  date%day,     &
        date%hour, date%minute, date%seconds, &
        tz_str
    END SELECT

  END SUBROUTINE write_isodate

  PURE SUBROUTINE write_isoduration(string, time0, time1, n_elem)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*),  INTENT(OUT) :: string
    TYPE(DateTime),    INTENT(IN)  :: time0
    TYPE(DateTime),    INTENT(IN)  :: time1
    INTEGER, OPTIONAL, INTENT(IN)  :: n_elem

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER             :: nelem
    REAL(KIND=GbcsDble) :: delta

    delta = Time_Difference(time0, time1)
    
    nelem = 0
    IF (PRESENT(n_elem)) nelem = n_elem

    IF (delta > 86400 .OR. nelem == 4) THEN
      WRITE(string, '("P",I1,"DT",I2.2,"H",I2.2,"M",I2.2,"S")') &
          FLOOR(delta/86400), MOD(FLOOR(delta/3600),24), &
          MOD(FLOOR(delta/60),60), MOD(FLOOR(delta),60)
    ELSEIF (delta > 36000 .OR. nelem == 3) THEN
      WRITE(string, '("PT",I2.2,"H",I2.2,"M",I2.2,"S")') &
          FLOOR(delta/3600), MOD(FLOOR(delta/60),60), MOD(FLOOR(delta),60)
    ELSEIF (delta > 3600 .OR. nelem == 2) THEN
      WRITE(string, '("PT",I1,"H",I2.2,"M",I2.2,"S")') &
          FLOOR(delta/3600), MOD(FLOOR(delta/60),60), MOD(FLOOR(delta),60)
    ELSEIF (delta > 60) THEN
      WRITE(string, '("PT",I2.2,"M",I2.2,"S")') &
          MOD(FLOOR(delta/60),60), MOD(FLOOR(delta),60)
    ELSE
      WRITE(string, '("PT",I2.2,"S")') MOD(FLOOR(delta),60)
    END IF
    
  END SUBROUTINE write_isoduration
END MODULE GbcsDateTime
