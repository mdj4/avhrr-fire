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



!+ This module contains Time and Spatial Geometry functions

MODULE GbcsTimeSpace

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
! 0.0   05/09/2006    Creation                                                   CPO
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
  USE GbcsConstants
  USE GbcsTypes
  USE GbcsDateTime, ONLY: DateTime

  IMPLICIT NONE

  CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsTimeSpace.mod'


CONTAINS

  
  SUBROUTINE Equation_Of_Time( Time , EqnOfTime , Declination )

  !
  ! Description:
  !
  !   Calculate the equation of time and sun declination. 
  !
  ! Method: 
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/12/2006    Creation                                                   CPO
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

    USE GbcsKinds
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

    TYPE(DateTime), INTENT (IN   ) :: Time
    REAL(KIND=GbcsReal) , INTENT (  OUT) :: EqnOfTime
    REAL(KIND=GbcsReal) , INTENT (  OUT) :: Declination

    INTEGER :: a, b
    INTEGER :: year, month, day
    INTEGER :: hour, minute
    REAL(KIND=GbcsDble) :: second
    REAL(KIND=GbcsDble) :: JD, JC
    REAL(KIND=GbcsDble) :: nseconds
    REAL(KIND=GbcsDble) :: MeanObliqEcliptic
    REAL(KIND=GbcsDble) :: Omega
    REAL(KIND=GbcsDble) :: ObliqEcliptic_cor
    REAL(KIND=GbcsDble) :: GeomMeanLongSun
    REAL(KIND=GbcsDble) :: GeomMeanAnomalySun
    REAL(KIND=GbcsDble) :: Ecc, z, c1, c2, c3, c4, c5
    REAL(KIND=GbcsDble) :: EqnOfCentre
    REAL(KIND=GbcsDble) :: SunApparentLong
    REAL(KIND=GbcsDble) :: SunTrueLong
    REAL(KIND=GbcsDble) :: sin_SunDec
    REAL(KIND=GbcsDble) :: mrad, sinm, sin2m, sin3m

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Equation_Of_Time'

      year   = Time%Year
      month  = Time%Month
      day    = Time%Day
      hour   = Time%Hour
      minute = Time%Minute

      second = REAL( Time%Seconds , KIND=GbcsDble ) + REAL( Time%Sec1000 , KIND=GbcsDble ) / 1000.0_GbcsDble

      ! For Julian Day
      IF ( month <= 2   ) THEN
        year  = year - 1
        month = month + 12
      END IF

      !--------------------------------
      ! 1. Calculate the Julian Century
      !--------------------------------

      a = year/100
      b = 2 - a + (a/4)

      ! Julian day

      JD = INT(365.25*(year+4716)) + INT(30.6001*(month+1)) + day + b - 1524.5

      ! Add on day fraction

      JD = JD + (hour*3600.0 + minute*60.0 + second) / (86400.0)

      ! Convert to Julian Century (365.25 days) relative to the year 2000

      JC = (JD - 2451545.0)/36525.0

      !-------------------------------------------------------
      ! 2. Calculate the Mean Obliquity of the Ecliptic
      !    and the correction to the Obliquity of the Ecliptic
      !-------------------------------------------------------

      nseconds = 21.448 - JC * ( 46.8150 + JC * (0.00059-JC*(0.001813)) )
      MeanObliqEcliptic = 23.0 + ( 26.0+(nseconds/60.0) ) / 60.0

      Omega = 125.04 - 1934.136 * JC
      ObliqEcliptic_cor = MeanObliqEcliptic + 0.00256 * COS(Omega*Deg2Rad)

      !-----------------------------------------------------
      ! 3. Calculate the Geometric mean Longitude of the Sun
      !-----------------------------------------------------

      GeomMeanLongSun = 280.46646 + JC * (36000.76983 + 0.0003032 * JC)
      ! Ensure longitude is between 0 and 360 degrees
      GeomMeanLongSun = MODULO(GeomMeanLongSun,360.0_GbcsDble)

      !------------------------------
      ! 4. Calculate the Mean Anomaly
      !------------------------------

      GeomMeanAnomalySun = 357.52911 + JC * (35999.05029 - 0.0001537 * JC)

      !--------------------------------------------------
      ! 5. Calculate the Eccentricity of the Earths orbit
      !--------------------------------------------------

      Ecc = 0.016708634 - JC * (0.000042037 + 0.0000001267*JC)

      !----------------------------------
      ! 6. Calculate the Equation of Time
      !----------------------------------

      mrad  = GeomMeanAnomalySun * Deg2Rad
      sinm  = SIN(mrad)
      sin2m = SIN(2*mrad)
      sin3m = SIN(3*mrad)

      z = TAN( 0.5 * ObliqEcliptic_cor * deg2rad )
      z = z*z
      c1 = SIN( 2.0 * GeomMeanLongSun * deg2rad )
      c2 = sinm
      c3 = COS( 2.0 * GeomMeanLongSun * deg2rad )
      c4 = 2*c1*c3
      c5 = sin2m
      EqnOfTime = z*c1 - 2.0*Ecc*c2 + 4.0*Ecc*z*c2*c3 - 0.5*z*z*c4 - 1.25*Ecc*Ecc*c5
      EqnOfTime = EqnOfTime * Rad2Deg * 4.0

      !-----------------------------------
      ! 7. Calculate the solar declination
      !-----------------------------------

      ! Equation of centre

      EqnOfCentre = sinm * ( 1.914602 - JC * (0.004817+0.000014*JC) ) + &
                    sin2m * (0.019993-0.000101*JC) + sin3m * 0.000289

      ! Sun true longitude

      SunTrueLong = GeomMeanLongSun + EqnOfCentre

      ! Apparent longitude of the sun

      Omega = 125.04 - 1934.136 * JC
      SunApparentLong = SunTrueLong - 0.00569 - 0.00478 * SIN(Omega*deg2rad)

      ! Solar declination

      sin_SunDec = SIN(ObliqEcliptic_cor*deg2rad) * SIN(SunApparentLong*deg2rad)
      Declination = ASIN(sin_SunDec)

    RETURN

  END SUBROUTINE Equation_Of_Time


  FUNCTION Satellite_Zenith( Sat_Lon , Sat_Lat , Sat_Alt , Pix_Lon , Pix_Lat )

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
  ! 0.0   05/12/2006    Creation                                                   CPO
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
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Satellite_Zenith

    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Sat_Lon
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Sat_Lat
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Sat_Alt
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Pix_Lon
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Pix_Lat

    REAL(KIND=GbcsReal) , PARAMETER :: EarthRadius = 6378.17              ! Equatorial Radius of the Earth (km)

    REAL(KIND=GbcsDble) :: RRatio , cos_lat , cos_lon , cos_latlon , sin_latlon , sat_za
    REAL(KIND=GbcsDble) :: Tan_Satellite_Zenith
    REAL(KIND=GbcsDble) :: Tan_Satellite_Zenith_Denom

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Satellite_Zenith'

      RRatio = EarthRadius / (Sat_Alt + EarthRadius)

      cos_lat = COS( (Pix_Lat - Sat_Lat) * Deg2Rad )
      cos_lon = COS( (Pix_Lon - Sat_Lon) * Deg2Rad )

      cos_latlon = cos_lat * cos_lon

      sin_latlon = SQRT(1.0 - cos_latlon*cos_latlon)

      Tan_Satellite_Zenith_Denom = cos_latlon - RRatio

      IF ( Tan_Satellite_Zenith_Denom > 0.0 ) THEN
        Tan_Satellite_Zenith = sin_latlon / Tan_Satellite_Zenith_Denom
        sat_za = ATAN(Tan_Satellite_Zenith) * Rad2Deg
      END IF

      Satellite_Zenith = sat_za

    RETURN    

  END FUNCTION Satellite_Zenith


  FUNCTION Satellite_Azimuth( Sat_Lon , Sat_Lat , Pix_Lon , Pix_Lat )

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
  ! 0.0   05/12/2006    Creation                                                   CPO
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
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Satellite_Azimuth

    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Sat_Lon
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Sat_Lat
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Pix_Lon
    REAL(KIND=GbcsReal) , INTENT (IN   ) :: Pix_Lat

    REAL(KIND=GbcsReal) :: cos_lon , sin_lat , sin_lon , sat_az

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Satellite_Azimuth'

      cos_lon = COS( (Pix_Lon - Sat_Lon) * Deg2Rad )
      sin_lat = SIN( (Pix_Lat - Sat_Lat) * Deg2Rad )
      sin_lon = SIN( (Pix_Lon - Sat_Lon) * Deg2Rad )

      sat_az = ATAN2( sin_lon, -sin_lat*cos_lon )

      sat_az = sat_az * Rad2Deg

      IF ( sat_az < 0.0 ) sat_az = sat_az + 360.0

      sat_az = 360.0 - sat_az

      Satellite_Azimuth = sat_az

    RETURN    

  END FUNCTION Satellite_Azimuth


  SUBROUTINE Solar_Angles( Time , Longitude , Latitude , Solar_Zenith ,  Solar_Azimuth )

  ! Description:
  !
  !   Calculate the solar zenith and scattering angles for a given time, and
  !   lat,long grid.
  !
  ! Method: 
  !
  ! Owner: Manager of TRICS project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   05/12/2006    Creation                                                   CPO
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

    USE GbcsKinds
    USE GbcsConstants
    USE GbcsTypes 

    IMPLICIT NONE

    TYPE(DateTime) , INTENT (IN   ) :: Time          ! time information
    REAL(KIND=GbcsReal) ,  INTENT (IN   ) :: Longitude     ! longitude values
    REAL(KIND=GbcsReal) ,  INTENT (IN   ) :: Latitude      ! latitude values
    REAL(KIND=GbcsReal) ,  INTENT (  OUT) :: Solar_Zenith  ! solar zenith angle
    REAL(KIND=GbcsReal) ,  INTENT (  OUT) :: Solar_Azimuth ! solar azimuth angle

    INTEGER, PARAMETER :: nsegments = 8

    INTEGER :: hour, minute
    REAL(KIND=GbcsReal) :: second
    REAL(KIND=GbcsReal) :: HourAngle                 ! Hour angle of the sun
    REAL(KIND=GbcsReal) :: theta                     ! Solar zenith angle
    REAL(KIND=GbcsReal) :: tan_azim_num              ! Numerator and denominator of the expression
    REAL(KIND=GbcsReal) :: tan_azim_den              !   for the tangent of the solar azimuth angle
    REAL(KIND=GbcsReal) :: TimeOffset, TrueSolarTime
    REAL(KIND=GbcsReal) :: cos_theta
    REAL(KIND=GbcsReal) :: sin_lat, cos_lat
    REAL(KIND=GbcsReal) :: sin_dec, cos_dec
    REAL(KIND=GbcsReal) :: sin_hour, cos_hour
    REAL(KIND=GbcsReal) :: EqnOfTime                 ! Equation of time
    REAL(KIND=GbcsReal) :: Declination               ! Declination of the sun

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Solar_Angles'


      !-------------------------------------------------------------
      ! 1. Initialize variables and calculate the equation of time
      !-------------------------------------------------------------

      CALL Equation_Of_Time( Time , EqnOfTime , Declination )

      Solar_Zenith  = NAN_R
      Solar_Azimuth = NAN_R

      hour =   Time%Hour
      minute = Time%Minute
      second = REAL( Time%Seconds , KIND=GbcsReal ) + REAL( Time%Sec1000 , KIND=GbcsReal ) / 1000.0_GbcsReal

      IF ( ABS(Latitude) > 90.0 .OR. ABS(Longitude) > 360.0 ) RETURN

      !---------------------------------
      ! 2 Calculate solar zenith angle
      !---------------------------------

      ! Calculate true solar time and solar hour angle
      ! (apply a time offset for the longitude).

      TimeOffset    = EqnOfTime + (4.0*Longitude)
      TrueSolarTime = (hour*60.0) + minute + (second/60.0) + TimeOffset
      HourAngle     = ( TrueSolarTime/4.0 - 180.0 ) * Deg2Rad

      sin_lat  = SIN(Latitude*Deg2Rad)
      cos_lat  = COS(Latitude*Deg2Rad)
      sin_dec  = SIN(Declination)
      cos_dec  = COS(Declination)
      sin_hour = SIN(HourAngle)
      cos_hour = COS(HourAngle)

      ! Cosine of zenith angle

      cos_theta = (sin_lat * sin_dec) + (cos_lat * cos_dec * cos_hour) 

      IF ( ABS(cos_theta) > 1.0 ) RETURN

      ! Assign solar zenith angle in degrees

      theta = ACOS(cos_theta)
      Solar_Zenith = theta * Rad2Deg

      !--------------------------------------
      ! 3 Calculate the solar azimuth angle
      !--------------------------------------

      ! Use the convention where the azimuth angle is measured 0-360 degrees from
      ! 'north' (add 180 degrees to the result to achieve this).

      tan_azim_num = cos_dec * sin_hour
      tan_azim_den = (sin_lat * cos_dec * cos_hour) - (cos_lat * sin_dec)
      Solar_Azimuth = ATAN2(tan_azim_num, tan_azim_den) ! angle in radians from south

      Solar_Azimuth = Solar_Azimuth * Rad2Deg + 180.0   ! angle in degrees from north

    RETURN

  END SUBROUTINE Solar_Angles


  FUNCTION Relative_Azimuth( Sol_Azimuth , Sat_Azimuth )

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
  ! 0.0   30/11/2006    Creation                                                   CPO
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
    USE GbcsConstants
    USE GbcsTypes

    IMPLICIT NONE

    REAL(KIND=GbcsReal) :: Relative_Azimuth

    REAL(KIND=GbcsReal) :: Sol_Azimuth
    REAL(KIND=GbcsReal) :: Sat_Azimuth

    REAL(KIND=GbcsReal) :: Rel_Az

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Relative_Azimuth'

      Rel_Az = ABS( Sol_Azimuth - Sat_Azimuth )
      If ( Rel_Az > 180.0 ) Rel_Az = 360.0 - Rel_Az

      Relative_Azimuth = Rel_Az

    RETURN    

  END FUNCTION Relative_Azimuth


END MODULE GbcsTimeSpace
