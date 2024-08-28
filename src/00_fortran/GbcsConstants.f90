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



!+ This module contains general options and definitions for the GBCS system

MODULE GbcsConstants

!
! Description:
!
!  Contains all global constants, IDs, etc used by the GBCS code
!
! Method:
!
!  Each constant is defined by tyoe and a value set to the appropriate precsion
!
!  Constants available
!
!    Mathematical and physical constants
!    Missing value, tiny and large values
!    Maximum lengths and limits for data structures
!    Satellite IDs
!    Sensor IDs
!    Forecast model 2-D field IDs
!    Forecast model 3-D field IDs
!    Absorber IDs
!    Absorber name strings
!    Units IDs
!    Units strings
!    RTM IDs
!    Channel IDs
!    Pixel classification flags
!    Surface classification flags
!    Daytime/Twilight/Nighttime flags
!    Model data location IDs
!
!
! Owner: Manager of TRICS Project
!
! History:
! Version  Date       Comment
! -------  ----       -------
! 0.0   25/01/2005    Creation                                                   CPO
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

  IMPLICIT NONE

  CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsConstants.mod'

! Global (i.e. public) Declarations:

! Global named constants:

  REAL(KIND=GbcsReal), PARAMETER :: Pi      = 3.141592654             ! Pi                  ( single precision )
  REAL(KIND=GbcsReal), PARAMETER :: Deg2Rad = Pi/180.0                ! degrees to radians  ( single precision )
  REAL(KIND=GbcsReal), PARAMETER :: Rad2Deg = 180.0/Pi                ! radians to degrees  ( single precision )
  REAL(KIND=GbcsDble), PARAMETER :: D_Pi  = 3.14159265358979323846_GbcsDble  ! Pi                  ( double precision )
  REAL(KIND=GbcsDble), PARAMETER :: D_Deg2Rad = D_Pi/180.0            ! degrees to radians  ( double precision )
  REAL(KIND=GbcsDble), PARAMETER :: D_Rad2Deg = 180.0/D_Pi            ! radians to degrees  ( double precision )
  REAL(KIND=GbcsReal), PARAMETER :: Planck_C1 = 0.000011910659        ! Planck Constant C1 
  REAL(KIND=GbcsReal), PARAMETER :: Planck_C2 = 1.438833              ! Planck Constant C2
  REAL(KIND=GbcsReal), PARAMETER :: ZeroK = -273.15                   ! 0K in Celcius
  REAL(KIND=GbcsReal), PARAMETER :: Standard_Gravity = 9.80665       ! m/s^2
  
! Global missing value constants:

  INTEGER, PARAMETER             :: NAN_I = -32768
  REAL(KIND=GbcsReal), PARAMETER :: NAN_MO = -32768.0*32768.0
  REAL(KIND=GbcsReal), PARAMETER :: NAN_R = -1.0e+30
  REAL(KIND=GbcsDble), PARAMETER :: NAN_D = -1.0e+300_GbcsDble
  REAL(KIND=GbcsReal), PARAMETER :: Gbcs_Tiny = 1.0e-35

! Global Type Definitions:

! Various lengths and limits

  INTEGER, PARAMETER :: FileNameLen     = 256 ! Length for file names
  INTEGER, PARAMETER :: ModelIdLen      = 6   ! Length of model identifier
  INTEGER, PARAMETER :: MaxFCRanges     = 40  ! Max number of forecast ranges
  INTEGER, PARAMETER :: MaxFields       = 256 ! Max number of fields
  INTEGER, PARAMETER :: MaxLevs         = 40  ! Max number of model levels
  INTEGER, PARAMETER :: CharLen         = 24  ! Standard length for characters
  INTEGER, PARAMETER :: UnitName        = 8   ! Length for name of model units

  INTEGER, PARAMETER :: Gbcs_Max_Chans  = 30     ! Maximum number of channels allowed
  INTEGER, PARAMETER :: Gbcs_Max_Corrections = 4 ! Maximum number of BT corrections allowed
  INTEGER, PARAMETER :: Gbcs_Max_Fields = 60     ! Maximum number of Forecast fields allowed

  INTEGER, PARAMETER :: Gbcs_Max_RTMs = 8        ! Maximum number of RTMs allowed
  
  INTEGER, PARAMETER :: Gbcs_Max_PDF_LUT_Dims = 4 ! Maximum number of PDF LUT dimensions allowed

  INTEGER, PARAMETER :: Gbcs_Max_Absorb = 10      ! Maximum number of absorbers in RTM calculation

  INTEGER, PARAMETER :: Gbcs_Max_Aerosol = 8      ! Maximum number of aerosol types in RTM calculation

! Internal Idicies

  ! Satellite IDs

  INTEGER, PARAMETER :: Gbcs_NOAA       =  1
  INTEGER, PARAMETER :: Gbcs_DMSP       =  2
  INTEGER, PARAMETER :: Gbcs_METEOSAT   =  3
  INTEGER, PARAMETER :: Gbcs_GOES       =  4
  INTEGER, PARAMETER :: Gbcs_GMS        =  5
  INTEGER, PARAMETER :: Gbcs_FY_2       =  6
  INTEGER, PARAMETER :: Gbcs_TRIMM      =  7
  INTEGER, PARAMETER :: Gbcs_ERS        =  8
  INTEGER, PARAMETER :: Gbcs_EOS        =  9
  INTEGER, PARAMETER :: Gbcs_METOP      = 10
  INTEGER, PARAMETER :: Gbcs_ENVISAT    = 11
  INTEGER, PARAMETER :: Gbcs_MSG        = 12
  INTEGER, PARAMETER :: Gbcs_FY_1       = 13
  INTEGER, PARAMETER :: Gbcs_ADEOS      = 14
  INTEGER, PARAMETER :: Gbcs_MTSAT      = 15
  INTEGER, PARAMETER :: Gbcs_CORIOLIS   = 16
  INTEGER, PARAMETER :: Gbcs_COMS       = 20
  INTEGER, PARAMETER :: Gbcs_AQUA       = 21
  INTEGER, PARAMETER :: Gbcs_TERRA      = 22

  ! Sensor IDs

  INTEGER, PARAMETER :: Gbcs_HIRS        =  0
  INTEGER, PARAMETER :: Gbcs_MSU         =  1
  INTEGER, PARAMETER :: Gbcs_SSU         =  2
  INTEGER, PARAMETER :: Gbcs_AMSU_A      =  3
  INTEGER, PARAMETER :: Gbcs_AMSU_B      =  4
  INTEGER, PARAMETER :: Gbcs_AVHRR       =  5
  INTEGER, PARAMETER :: Gbcs_SSMI        =  6
  INTEGER, PARAMETER :: Gbcs_VTPR1       =  7
  INTEGER, PARAMETER :: Gbcs_VTPR2       =  8
  INTEGER, PARAMETER :: Gbcs_TMI         =  9
  INTEGER, PARAMETER :: Gbcs_SSMIS       = 10
  INTEGER, PARAMETER :: Gbcs_AIRS        = 11
  INTEGER, PARAMETER :: Gbcs_HSB         = 12
  INTEGER, PARAMETER :: Gbcs_MODIS       = 13
  INTEGER, PARAMETER :: Gbcs_ATSR        = 14
  INTEGER, PARAMETER :: Gbcs_MHS         = 15
  INTEGER, PARAMETER :: Gbcs_IASI        = 16
  INTEGER, PARAMETER :: Gbcs_AMSR        = 17
  INTEGER, PARAMETER :: Gbcs_MVIRI       = 20
  INTEGER, PARAMETER :: Gbcs_SEVIRI      = 21
  INTEGER, PARAMETER :: Gbcs_GOES_IMG    = 22
  INTEGER, PARAMETER :: Gbcs_GOES_SND    = 23
  INTEGER, PARAMETER :: Gbcs_GMS_IMG     = 24
  INTEGER, PARAMETER :: Gbcs_MTSAT_IMG   = 24
  INTEGER, PARAMETER :: Gbcs_FY2_VISSR   = 25
  INTEGER, PARAMETER :: Gbcs_FY1_MVISR   = 26
  INTEGER, PARAMETER :: Gbcs_CriS        = 27
  INTEGER, PARAMETER :: Gbcs_CMISS       = 28
  INTEGER, PARAMETER :: Gbcs_VIIRS       = 29
  INTEGER, PARAMETER :: Gbcs_WINDSAT     = 30
  INTEGER, PARAMETER :: Gbcs_COMS_MI     = 31

  ! Forecast Model 2D Fields

  INTEGER, PARAMETER :: Gbcs_Map_OROG     =  1  ! Orography
  INTEGER, PARAMETER :: Gbcs_Map_P_SURF   =  2  ! Surface Pressure
  INTEGER, PARAMETER :: Gbcs_Map_P_MSL    =  3  ! Mean Sea Level Pressure
  INTEGER, PARAMETER :: Gbcs_Map_T_SURF   =  4  ! Surface Temperature
  INTEGER, PARAMETER :: Gbcs_Map_T_SKIN   =  5  ! Skin Temperature
  INTEGER, PARAMETER :: Gbcs_Map_T_2M     =  6  ! 2m Temperature
  INTEGER, PARAMETER :: Gbcs_Map_SST      =  7  ! Sea Surface Temperature
  INTEGER, PARAMETER :: Gbcs_Map_LST      =  8  ! Land Surface Temperature
  INTEGER, PARAMETER :: Gbcs_Map_WV_2M    =  9  ! 2m Water Vapour
  INTEGER, PARAMETER :: Gbcs_Map_TCWV     = 10  ! Total Column Water Vapour
  INTEGER, PARAMETER :: Gbcs_Map_U_SURF   = 11  ! Surface Eastward Component of Wind
  INTEGER, PARAMETER :: Gbcs_Map_V_SURF   = 12  ! Surface Northward Component of Wind
  INTEGER, PARAMETER :: Gbcs_Map_Wind_10M = 13  ! 10m Wind Speed
  INTEGER, PARAMETER :: Gbcs_Map_Snow     = 14  ! Snow Amount
  INTEGER, PARAMETER :: Gbcs_Map_Sea_Ice  = 15  ! Sea Ice Cover
  INTEGER, PARAMETER :: Gbcs_Map_CTP      = 16  ! Cloud Top Pressure
  INTEGER, PARAMETER :: Gbcs_Map_Emiss    = 17  ! Emissivity
  INTEGER, PARAMETER :: Gbcs_Map_z1000    = 18  ! 1000mb height
  INTEGER, PARAMETER :: Gbcs_Map_AOD      = 19  ! Aerosol optical depth
  INTEGER, PARAMETER :: Gbcs_Map_Volc     = 20  ! Volcanic aerosol
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_Dust_Rad          =30 ! Dust
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_SeaSalt_SAM_Rad   =31 ! Sea Salt SAM
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_SeaSalt_SSCM1_Rad =32 ! Sea Salt SSCM1
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_SeaSalt_SSCM2_Rad =33 ! Sea Salt SSCM2
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_SeaSalt_SSCM3_Rad =34 ! Sea Salt SSCM3
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_Organic_Carbon_Rad=35 ! Organic Carbon
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_Black_Carbon_Rad  =36 ! Black Carbon
  INTEGER, PARAMETER :: Gbcs_Map_Aerosol_Sulfate_Rad       =37 ! Sulfate
  INTEGER, PARAMETER :: Gbcs_Map_Tmin                      =40

  ! User defined fields which maybe needed for particular
  ! applications, but are meaningless to the main Gbcs library
  INTEGER, PARAMETER :: Gbcs_Map_User4    = 27  ! User defined field 4
  INTEGER, PARAMETER :: Gbcs_Map_User3    = 28  ! User defined field 3
  INTEGER, PARAMETER :: Gbcs_Map_User2    = 29  ! User defined field 2
  INTEGER, PARAMETER :: Gbcs_Map_User1    = 30  ! User defined field 1

  ! Forecast Model 3D Fields

  INTEGER, PARAMETER :: Gbcs_Map_GH_3D    =  1  ! Geopotential Height 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_P_3D     =  2  ! Pressure 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_T_3D     =  3  ! Temperature 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_WV_3D    =  4  ! Water Vapour 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_O3_3D    =  5  ! Ozone 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_CO2_3D   =  6  ! Carbon Dioxide 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_CO_3D    =  7  ! Carbon Monoxide 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_U_WIND   =  8  ! U Component of Wind 3D Field
  INTEGER, PARAMETER :: Gbcs_Map_V_WIND   =  9  ! V Component of Wind Field
  INTEGER, PARAMETER :: Gbcs_Map_W_WIND   = 10  ! W Component of Wind Field

  ! Absorber Identifier

  INTEGER, PARAMETER :: Gbcs_Absorber_WV  = 1  ! Water Vapour
  INTEGER, PARAMETER :: Gbcs_Absorber_O3  = 2  ! Ozone
  INTEGER, PARAMETER :: Gbcs_Absorber_CO2 = 3  ! Carbon Dioxide
  INTEGER, PARAMETER :: Gbcs_Absorber_CO  = 4  ! Carbon Monoxide

  ! Aerosol Identifier

  INTEGER, PARAMETER :: Gbcs_Aerosol_Dust           = 1  ! Dust
  INTEGER, PARAMETER :: Gbcs_Aerosol_SeaSalt_SAM    = 2  ! Sea Salt SAM
  INTEGER, PARAMETER :: Gbcs_Aerosol_SeaSalt_SSCM1  = 3  ! Sea Salt SSCM1
  INTEGER, PARAMETER :: Gbcs_Aerosol_SeaSalt_SSCM2  = 4  ! Sea Salt SSCM2
  INTEGER, PARAMETER :: Gbcs_Aerosol_SeaSalt_SSCM3  = 5  ! Sea Salt SSCM3
  INTEGER, PARAMETER :: Gbcs_Aerosol_Organic_Carbon = 6  ! Organic Carbon
  INTEGER, PARAMETER :: Gbcs_Aerosol_Black_Carbon   = 7  ! Black Carbon
  INTEGER, PARAMETER :: Gbcs_Aerosol_Sulfate        = 8  ! Sulfate

  ! Absorber Strings

  CHARACTER(LEN=40), DIMENSION(20) :: Gbcs_Absorb_Str

  DATA Gbcs_Absorb_Str /  'Water Vapour'                            ,   &  !  1
                          'Ozone'                                   ,   &  !  2
                          'Carbon Dioxide'                          ,   &  !  3
                          'Carbon Monoxide'                         ,   &  !  4
                          ''                                        ,   &  !  5  (Undefined)
                          ''                                        ,   &  !  6  (Undefined)
                          ''                                        ,   &  !  7  (Undefined)
                          ''                                        ,   &  !  8  (Undefined)
                          ''                                        ,   &  !  9  (Undefined)
                          ''                                        ,   &  ! 10  (Undefined)
                          ''                                        ,   &  ! 11  (Undefined)
                          ''                                        ,   &  ! 12  (Undefined)
                          ''                                        ,   &  ! 13  (Undefined)
                          ''                                        ,   &  ! 14  (Undefined)
                          ''                                        ,   &  ! 15  (Undefined)
                          ''                                        ,   &  ! 16  (Undefined)
                          ''                                        ,   &  ! 17  (Undefined)
                          ''                                        ,   &  ! 18  (Undefined)
                          ''                                        ,   &  ! 19  (Undefined)
                          ''                                      /        ! 20  (Undefined)

  ! Units Identifier

  INTEGER, PARAMETER :: Gbcs_Units_kg_per_m3 =  1  ! mass density
  INTEGER, PARAMETER :: Gbcs_Units_kg_per_kg =  2  ! mixing ratio
  INTEGER, PARAMETER :: Gbcs_Units_ppmv      =  3  ! ppmv
  INTEGER, PARAMETER :: Gbcs_Units_Kelvin    =  4  ! Degrees Kelvin
  INTEGER, PARAMETER :: Gbcs_Units_Celsius   =  5  ! Degrees Celsius
  INTEGER, PARAMETER :: Gbcs_Units_Pa        =  6  ! Pascals
  INTEGER, PARAMETER :: Gbcs_Units_hPa       =  7  ! hecta-Pascals
  INTEGER, PARAMETER :: Gbcs_Units_mb        =  8  ! millibars
  INTEGER, PARAMETER :: Gbcs_Units_cm        =  9  ! centimetres
  INTEGER, PARAMETER :: Gbcs_Units_m         = 10  ! metres
  INTEGER, PARAMETER :: Gbcs_Units_km        = 11  ! kilometres
  INTEGER, PARAMETER :: Gbcs_Units_ms        = 12  ! milliseconds
  INTEGER, PARAMETER :: Gbcs_Units_s         = 13  ! seconds
  INTEGER, PARAMETER :: Gbcs_Units_min       = 14  ! minutes
  INTEGER, PARAMETER :: Gbcs_Units_hr        = 15  ! hours
  INTEGER, PARAMETER :: Gbcs_Units_km_per_hr = 16  ! km/hr
  INTEGER, PARAMETER :: Gbcs_Units_m_per_s   = 17  ! m/s
  INTEGER, PARAMETER :: Gbcs_Units_kg_per_m2 = 18  ! kg/m^2
  INTEGER, PARAMETER :: Gbcs_Units_RH        = 19  ! Relative humidity
  INTEGER, PARAMETER :: Gbcs_Units_g_per_kg  = 20  ! mixing ratio
  INTEGER, PARAMETER :: Gbcs_Units_m2_per_s2 = 21  ! geopotential height


  ! Unit Strings

  CHARACTER(LEN=20), DIMENSION(20) :: Gbcs_Units_Str

  DATA Gbcs_Units_Str /  'kg/m^3'               ,   &  !  1
                         'kg/kg'                ,   &  !  2
                         'ppmv'                 ,   &  !  3
                         'K'                    ,   &  !  4
                         'C'                    ,   &  !  5
                         'Pa'                   ,   &  !  6
                         'hPa'                  ,   &  !  7
                         'mb'                   ,   &  !  8
                         'cm'                   ,   &  !  9
                         'm'                    ,   &  ! 10
                         'km'                   ,   &  ! 11
                         'ms'                   ,   &  ! 12
                         's'                    ,   &  ! 13
                         'min'                  ,   &  ! 14
                         'hr'                   ,   &  ! 15
                         'km/hr'                ,   &  ! 16
                         'm/s'                  ,   &  ! 17
                         'kg/m^2'               ,   &  ! 18
                         '%'                    ,   &  ! 19
                         'g/kg'               /        ! 20

  ! Radiative Transfer Model Identifier

  INTEGER, PARAMETER :: Gbcs_CRTM_IR_ID    = 1  ! NOAA CRTM IR Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_RTTOV_ID      = 2  ! RTTOV Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_RTTOV_10_ID   = 3  ! RTTOV 10 Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_6S_ID         = 4  ! 6S Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_VISRTM_ID     = 5  ! Improved Visible Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_CRTM_VIS_ID   = 6  ! CRTM Visible Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_CRTM_VISIR_ID = 7  ! CRTM Visible and IR Radiative Transfer Model
  INTEGER, PARAMETER :: Gbcs_RTTOV_11_ID   = 8  ! RTTOV 11 Radiative Transfer Model

  ! Channel Identification Numbers

  INTEGER, PARAMETER :: Gbcs_VIS_BBD =  1  ! Broadband Visible data
  INTEGER, PARAMETER :: Gbcs_VIS_005 =  2  ! 0.5 um Visible data
  INTEGER, PARAMETER :: Gbcs_VIS_006 =  3  ! 0.6 um Visible data
  INTEGER, PARAMETER :: Gbcs_VIS_008 =  4  ! 0.8 um Visible data
  INTEGER, PARAMETER :: Gbcs_NIR_01x =  5  !   1 um Near InfraRed data
  INTEGER, PARAMETER :: Gbcs_IR_03x  =  6  !   3 um InfraRed data
  INTEGER, PARAMETER :: Gbcs_IR_08x  =  7  !   8 um InfraRed data
  INTEGER, PARAMETER :: Gbcs_IR_10x  =  8  !  10 um InfraRed data
  INTEGER, PARAMETER :: Gbcs_IR_12x  =  9  !  12 um InfraRed data
  INTEGER, PARAMETER :: Gbcs_IR_13x  = 10  !  13 um InfraRed data
  INTEGER, PARAMETER :: Gbcs_WV_06x  = 11  !   6 um InfraRed data (Water Vapor absorber)
  INTEGER, PARAMETER :: Gbcs_WV_07x  = 12  !   7 um InfraRed data (Water Vapor absorber)
  INTEGER, PARAMETER :: Gbcs_OZ_09x  = 13  !   9 um InfraRed data (Ozone absorber)

! Internal Flag values

  ! General Pixel Classification

  INTEGER, PARAMETER :: Gbcs_LAND         = 0  ! 0/1  sea/land
  INTEGER, PARAMETER :: Gbcs_ICE          = 1  ! 2
  INTEGER, PARAMETER :: Gbcs_CLOUD        = 2
  INTEGER, PARAMETER :: Gbcs_GLINT        = 3  ! 8
  INTEGER, PARAMETER :: Gbcs_LIMB         = 4  ! 16
  INTEGER, PARAMETER :: Gbcs_DAY          = 5  ! 32
  INTEGER, PARAMETER :: Gbcs_NIGHT        = 6  ! 64
  INTEGER, PARAMETER :: Gbcs_SPACE        = 7  ! 128
  INTEGER, PARAMETER :: Gbcs_WATER        = 8

!  INTEGER, PARAMETER :: Gbcs_SPACE        = 1
!  INTEGER, PARAMETER :: Gbcs_LAND         = 2
!  INTEGER, PARAMETER :: Gbcs_WATER        = 3
!  INTEGER, PARAMETER :: Gbcs_CLOUD        = 4
!  INTEGER, PARAMETER :: Gbcs_GLINT        = 5
!  INTEGER, PARAMETER :: Gbcs_LIMB         = 6
!  INTEGER, PARAMETER :: Gbcs_ICE          = 7

  ! Pixel bitmask flags
  INTEGER, PARAMETER :: Gbcs_Flag_Land     = 0
  INTEGER, PARAMETER :: Gbcs_Flag_Ice      = 1
  INTEGER, PARAMETER :: Gbcs_Flag_Cloud    = 2
  INTEGER, PARAMETER :: Gbcs_Flag_Glint    = 3
  INTEGER, PARAMETER :: Gbcs_Flag_Limb     = 4
  INTEGER, PARAMETER :: Gbcs_Flag_Day      = 5
  INTEGER, PARAMETER :: Gbcs_Flag_Night    = 6
  INTEGER, PARAMETER :: Gbcs_Flag_Space    = 7
  

  ! Surface Classifications

  INTEGER, PARAMETER :: Gbcs_Surface_Land   = 0
  INTEGER, PARAMETER :: Gbcs_Surface_Sea    = 1
  INTEGER, PARAMETER :: Gbcs_Surface_Ice    = 2
  INTEGER, PARAMETER :: Gbcs_Surface_Space  = 127

  ! Day / Twilight / Nighttime Classification

  INTEGER, PARAMETER :: Gbcs_Daytime    = 0
  INTEGER, PARAMETER :: Gbcs_Twilight   = 1
  INTEGER, PARAMETER :: Gbcs_Nighttime  = 2

  ! Where data values are in relation to model grid points

  INTEGER, PARAMETER :: Gbcs_At_Corner    = 0   ! At grid points
  INTEGER, PARAMETER :: Gbcs_At_Centre    = 1   ! In centre of cells defined by grid points

  ! Model Grid type
  INTEGER, PARAMETER :: Gbcs_Model_Grid_Rect    = 0   ! Rectangular grid
  INTEGER, PARAMETER :: Gbcs_Model_Grid_Gaus    = 1   ! Gaussian grid

  ! Output file types
  INTEGER, PARAMETER :: Gbcs_Output_RAW = 0
  INTEGER, PARAMETER :: Gbcs_Output_MCIDAS = 1

  ! Maximum allowed satellite angle
  REAL(GbcsReal), PARAMETER :: Gbcs_Max_Satellite_Angle = 65.

  ! visible PDF file names
  CHARACTER(len=3), PARAMETER, DIMENSION(3) :: VIS_PDF_STR = (/ "006", "008", "016" /)  

  ! Error status
  INTEGER, PARAMETER :: Gbcs_Status_Success = 0
  INTEGER, PARAMETER :: Gbcs_Status_Error   = 1

  INTERFACE
    PURE FUNCTION Gbcs_Version()
      CHARACTER(LEN=32) :: Gbcs_Version
    END FUNCTION
  END INTERFACE
  
END MODULE GbcsConstants
