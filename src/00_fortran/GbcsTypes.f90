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



!+ This module contains the definition of GBCS data types

MODULE GbcsTypes

!
! Description:
!
!  Contains the type defintions for the data structures used in the GBCS code
!
! Method:
!
!  The data structures are defined using the FORTRAN TYPE descriptors
!
!  The main types defined are
!
!    JobDescriptor   - Job description structure that input script data is stored in
!    Satellite       - Satellite structure that satellite information is loaded into
!    Imagery         - Imagery structure that imagery is loaded into
!    Forecast_Model  - Forecast model structure that model data is loaded into
!    RTM_Descriptor  - RTM description structure that RTM information is loaded in to,
!                      also stores RTM profiles 
!    Aux_Data        - Auxillary data structure that the forecast model fields are loaded into
!    Atm_Profile     - Atmospheric profiles structure that stores all 2-D and 3-D atmosphere
!                      fields for a single location
!    PDF_LUT_Struct  - PDF look up table structure containing LUT description and look up table
!                      data
!    Rtrv_Defn       - Surface temperature retrieval function definition and coefficient table
!    Cov_Matrix      - Covariance matrix ( B ) definition and table(s)
!    TestPoint_Descriptor - Test point location, surrounding pixels to store and output file to
!                      save the data in
!    Grid_Descriptor - Grid description for model fields, number rows, columns and levels,
!                      origin coords, lat/lon increments, data location
!    Model_Map_2D    - Map of model nearest neighbour points to a pixel, line and element
!                      values, interpolation weights
!
!  The secondary structures are
!
!    Data1D_I8       - Allocatable 1-D array of bytes
!    Data2D_I8       - Allocatable 2-D array of bytes
!    Data3D_I8       - Allocatable 3-D array of bytes
!    Data1D_I16      - Allocatable 1-D array of 2 byte integers
!    Data2D_I16      - Allocatable 2-D array of 2 byte integers
!    Data3D_I16      - Allocatable 3-D array of 2 byte integers
!    Data1D_I32      - Allocatable 1-D array of 4 byte integers
!    Data2D_I32      - Allocatable 2-D array of 4 byte integers
!    Data3D_I32      - Allocatable 3-D array of 4 byte integers
!    Data1D_R32      - Allocatable 1-D array of 4 byte reals
!    Data2D_R32      - Allocatable 2-D array of 4 byte reals
!    Data3D_R32      - Allocatable 3-D array of 4 byte reals
!
!    Time_Stamp      - time stamp structure used to store computer time data
!    Time_Structure  - integer based structure for storing date and time information
!    LineElem        - Line/element pair of integers 
!    LatLon          - Latitude/longitude pair of reals
!    RADec           - Right ascension/declination pair of reals
!    
!
!
! Owner: Manager of TRICS Project
!
! History:
! Version  Date       Comment
! -------  ----       -------
! 0.0   03/01/2005    Creation                                                   CPO
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
  USE GbcsBaseTypes
  USE GbcsConstants
  USE GbcsDateTime,  ONLY: DateTime
  USE GbcsPDFLookup, ONLY: PDF_Struct
  USE GbcsPixel,     ONLY: ImagePixel

  IMPLICIT NONE

  CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsTypes.mod'

! Global named constants:

! Global Type Definitions:

  TYPE Time_Stamp
    CHARACTER(LEN=8)      :: CDATE                                      ! Date as a character string
    CHARACTER(LEN=10)     :: CTIME                                      ! Time as a character string
    CHARACTER(LEN=5)      :: CZONE                                      ! Time zone as a character string
    INTEGER, DIMENSION(8) :: VALUES                                     ! Date and time as integer values (Y,M,D,dUTC,h,m,s,s1000)
    REAL                  :: RTIME                                      ! Time in hours as a real
  END TYPE Time_Stamp


  TYPE TestPoint_Descriptor
    LOGICAL :: Active                                                   ! Active process flag
    INTEGER :: LC                                                       ! Line number of test pixel
    INTEGER :: EC                                                       ! Element number of test pixel
    INTEGER :: L1 , L2                                                  ! Line offsets to surrounding pixels
    INTEGER :: E1 , E2                                                  ! Element offsets to surrounding pixels
    CHARACTER(LEN=100) :: PathName                                      ! Test point output path
    CHARACTER(LEN=100) :: FileName                                      ! Test point output file name
    INTEGER :: File_Unit                                                ! Test point file unit number
    LOGICAL :: File_Open                                                ! File open flag
  END TYPE TestPoint_Descriptor


  TYPE Grid_Descriptor
    LOGICAL :: Earth_Coord                                              ! Flag identifying coord type: .TRUE. = Earth Coords, .FALSE. = Image Coords
    LOGICAL :: RTM_Format                                               ! Profiles in RTM format ( .TRUE. , .FALSE. )
    LOGICAL :: Full_Globe = .TRUE.                                      ! Model fields cover all longitudes
    INTEGER :: NLines                                                   ! Number of lines in grid
    INTEGER :: NElems                                                   ! Number of elements per grid line
    INTEGER :: NLevels                                                  ! Number of levels in grid
    LOGICAL :: Upwards                                                  ! Profiles upwards from surface ( .TRUE. / .FALSE. )
    TYPE(LatLon) :: LLCrnr                                              ! Latitude and longitude of lower left hand corner of grid
    TYPE(LatLon) :: URCrnr                                              ! Latitude and longitude of upper right hand corner of grid
    TYPE(LatLon) :: Incr                                                ! Latitude and longitude grid increments
    TYPE(LineElem) :: I_LLCrnr                                          ! Line and element of lower left hand corner of grid
    TYPE(LineElem) :: I_URCrnr                                          ! Line and element of upper right hand corner of grid
    TYPE(LineElem) :: I_Incr                                            ! Line and element grid increments
    INTEGER :: DataLoc                                                  ! Model data location relative to grid: 0 = corner, 1 = centre
    INTEGER :: Type                                                     ! Model data grid type: 0 = Rectangular, 1 = Gaussian
  END TYPE Grid_Descriptor


  TYPE Cov_Matrix
    INTEGER :: NBands                                                   ! Number of latitude bands used upto a maximum of 10
    INTEGER :: NFields                                                  ! Number of fields used in matrix
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_ID                     ! Internal GBCS Field IDs
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: NLevels                      ! Number of levels per field
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Type                         ! Number of levels per field
    INTEGER :: MSize                                                    ! Size of the square matrix
    REAL(KIND=GbcsReal), DIMENSION(10,2) :: Limits                      ! Latitude band limits upto a maximum of 10 bands
    REAL(KIND=GbcsReal), DIMENSION(:,:,:), ALLOCATABLE :: Data          ! Covariance matrix for each band
  END TYPE Cov_Matrix


  TYPE PDF_LUT_Struct
    LOGICAL :: Spectral                                                 ! Spectral or textural PDF flag
    INTEGER :: NChans                                                   ! Number of channels in PDF
    INTEGER, DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Dims                   ! Dimension length per channel
    INTEGER, DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Chan_ID                ! Channel ID numbers
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Orig       ! Origin value for each dimension (i.e. channel)
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Incr       ! increment along each dimension (i.e. channel)
    INTEGER :: Data_Flag                                                ! 0 = BT, 1 = LSD, 2 = VAR
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE :: LUT               ! Look Up Table data
  END TYPE PDF_LUT_Struct    


  TYPE Atm_Profile
    INTEGER :: I_Lat                                                    ! Latitude index on model grid
    INTEGER :: I_Lon                                                    ! Longitude index on model grid
    INTEGER :: NLevels                                                  ! Number of levels in profile
    INTEGER :: NAbsorb                                                  ! Number of available absorbers
    INTEGER :: NAerosols                                                ! Number of available aerosol components
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_ID                  ! ID of available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_Units               ! Units of available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Aerosol_ID                   ! ID of available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Aerosol_Units                ! Units of available absorbers
    REAL(KIND=GbcsReal) :: lat
    REAL(KIND=GbcsReal) :: lon
    REAL(KIND=GbcsReal) :: elem
    REAL(KIND=GbcsReal) :: line
    REAL(KIND=GbcsReal),DIMENSION(:), ALLOCATABLE :: z                  ! Level heights above surface
    REAL(KIND=GbcsReal),DIMENSION(:), ALLOCATABLE :: P                  ! Level pressure
    REAL(KIND=GbcsReal),DIMENSION(:), ALLOCATABLE :: P_layer            ! Layer pressure
    REAL(KIND=GbcsReal),DIMENSION(:), ALLOCATABLE :: T                  ! Level temperature
    REAL(KIND=GbcsReal),DIMENSION(:), ALLOCATABLE :: aerosol            ! Layer aerosol    
    TYPE(Data1D_R32), DIMENSION(Gbcs_Max_Fields)  :: A                  ! Level absorber concentrations
    TYPE(Data1D_R32), DIMENSION(Gbcs_Max_Aerosol) :: AerosolConc        ! Level aerosol concentrations for different types
    REAL, DIMENSION(Gbcs_Max_Aerosol) :: Aerosol_Radius                 ! Level aerosol radius
    REAL(KIND=GbcsReal) :: z0                                           ! Surface height
    REAL(KIND=GbcsReal) :: P_2m                                         ! 2m pressure
    REAL(KIND=GbcsReal) :: T_2m                                         ! 2m temperature
    REAL(KIND=GbcsReal) :: Tmin                                         ! 2m temperature
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Surface_Absorber_Units       ! Units of available absorbers
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Absorb) :: A_2m             ! 2m absorber concentrations
    REAL(KIND=GbcsReal) :: T_Skin                                       ! Skin temperature
    REAL(KIND=GbcsReal) :: U_2m                                         ! Eastward component of 2m wind field
    REAL(KIND=GbcsReal) :: V_2m                                         ! Northward component of 2m wind field
    REAL(KIND=GbcsReal) :: Wind_10m                                     ! 10m Wind speed
    REAL(KIND=GbcsReal) :: SatZA                                        ! Satellite zenith angle at profile location
    REAL(KIND=GbcsReal) :: SolZA                                        ! Solar zenith angle at profile location
    REAL(KIND=GbcsReal) :: SatAz                                        ! Satellite azimuth angle at profile location
    REAL(KIND=GbcsReal) :: SolAz                                        ! Solar azimuth angle at profile location
    REAL(KIND=GbcsReal) :: RelAz                                        ! Satellite-Sun relative azimuth
    REAL(KIND=GbcsReal) :: EffZA                                        ! Effective zenith angle for sun-glint correction
    REAL(KIND=GbcsReal) :: TCWV                                         ! Total column water vapour
    REAL(KIND=GbcsReal) :: relhum                                       ! (Near) surface relative-humidity
    REAL(KIND=GbcsReal) :: Salinity                                     ! Salinity of water, dg/l (sea water = 350)
    REAL(KIND=GbcsReal) :: Instrument_Temp                              ! Instrument temperature
    INTEGER :: Surface_Type                                             ! Surface type beneath profile
    INTEGER :: Land_Class
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Emissivity        ! Channel surface emissivity
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Emiss_Var         ! Channel Emissivity Variance
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Reflectivity      ! Channel surface reflectivity
    LOGICAL :: Allocated = .FALSE.                                      ! Data space allocated flag: default = .FALSE.
    TYPE(DateTime)      :: Time
  END TYPE Atm_Profile


  TYPE JobDescriptor
    CHARACTER(LEN=80)  :: Name                                          ! Job name used to identify files and create directories, etc
    CHARACTER(LEN=256) :: InputFile                                     ! Input file defining job
    CHARACTER(LEN=256) :: OutputPath                                    ! Output directory path
    CHARACTER(LEN=256) :: GbcsDataPath                                  ! GBCS data directory path
    CHARACTER(LEN=256) :: Sat_Name                                      ! Name of satellite to be used
    INTEGER            :: Platform_Num                                  ! Satellite platform number  e.g. for MSG PN=1, for GOES PN = 8 10 12
    CHARACTER(LEN=80)  :: ImgDataFrmt                                   ! Data format of imagery
    CHARACTER(LEN=200) :: ImgDataDir                                    ! Data directory containing imagery
    CHARACTER(LEN=200) :: ImgDataFile                                   ! Data file containing imagery
    CHARACTER(LEN=80)  :: PrdDataFrmt                                   ! Output Data format (RAW/MCIDAS)
    LOGICAL :: ClassesThere                                             ! Is classes there
    INTEGER :: Classes                                                  ! Num of classes (0:n, 2:cld-clr; 3:cld-clr-dust)
    INTEGER :: N_Chans                                                  ! Number of channels to be loaded
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Chan_ID                       ! Satellite channel IDs for loaded channels
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Mod_NEdT          ! NEdT modifications
    TYPE(LineElem) :: Origin                                            ! Sub-region origin in pixel coordinates
    TYPE(LineElem) :: Npts                                              ! Number of lines and elements in sub-region
    CHARACTER(LEN=256) :: FCM_Name                                      ! Name of Forecast Model
    CHARACTER(LEN=20)  :: FCMDataFrmt                                   ! Data format of  forecast model fields
    CHARACTER(LEN=200) :: FCMDataDir                                    ! Data directory containing forecast model files
    CHARACTER(LEN=200) :: FCMDataFile                                   ! Data file containing forecast model fields
    INTEGER :: N_2D_Fields                                              ! Number of 2-D surface fields stored in files
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_2D_ID                  ! Model 2D field IDs to be used
    INTEGER :: N_3D_Fields                                              ! Number of 3-D atmospheric fields
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_3D_ID                  ! Model 3D field IDs to be used
    INTEGER :: N_Absorbers                                              ! Number of available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_ID                  ! Model Absorber IDs to be used
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_Units               ! Model Absorber units to be used
    CHARACTER(LEN=256) :: CovMatFile                                    ! Name of file containing covariance matirx
    INTEGER :: N_RTMs                                                   ! Number of RTMs to load
    CHARACTER(LEN=256), DIMENSION(Gbcs_Max_RTMs) :: RTM_Name            ! Names of RTMs to be used
    INTEGER, DIMENSION(Gbcs_Max_RTMs) :: RTM_ID                         ! RTM internal GBCS ID number 
    INTEGER, DIMENSION(Gbcs_Max_RTMs) :: RTM_Emiss                      ! Flag to set use of emissivity model (0 = don't use, 1 = use model)
    INTEGER, DIMENSION(Gbcs_Max_RTMs) :: RTM_Use_Solar_Angle            ! Flag to set use of CRTM solar scattering model (0 = don't use, 1 = use model)
    INTEGER, DIMENSION(Gbcs_Max_Chans,4) :: Bayes_Spec_Chans            ! Channel map for Bayesian Spectral calculation (1 = Day/Twilight, 2 = Night, 3=RTM_ID)
    INTEGER, DIMENSION(Gbcs_Max_Chans,3) :: Bayes_Text_Chans            ! Channel map for Bayesian Textural calculation (1 = Day/Twilight, 2 = Night, 3=RTM_ID)
    INTEGER, DIMENSION(Gbcs_Max_Chans)   :: Chans_RTM_Map               ! Channel mapping to RTms
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: eYb               ! Error in background modelled estimates for each channel
    CHARACTER(LEN=256) :: RTV_Name                                      ! Name of SST Retrieval Function
    LOGICAL :: TestPoint                                                ! Status flag for storing a test point
    CHARACTER(LEN=100) :: TestPointPath                                 ! Path used to store test point data
    CHARACTER(LEN=100) :: TestPointFile                                 ! File name used to store test point data
    INTEGER :: TestPointLine                                            ! Test point line location
    INTEGER :: TestPointElem                                            ! Test point element location
    TYPE( Time_Stamp )  :: Start_Time                                   ! Processing start time
    TYPE( Time_Stamp )  :: Stop_Time                                    ! Processing stop time
    REAL(KIND=GbcsReal) :: Elapsed_Time                                 ! Elapsed time for complete process
    LOGICAL :: OCEAN_ONLY                                               ! Only analyse over oceans 
    LOGICAL :: Use_Cloudy_Prior                                         ! Use cloudy prior information
    LOGICAL :: Use_HighRes_SST                                          ! Use High Resolution SST
    LOGICAL :: RADIANCE_BIAS_CORRECT                                    ! Do Radiance Bias correction
    CHARACTER(LEN=256) :: BIAS_CORRECT_DIR                              ! Directory containing bias correction
    CHARACTER(LEN=256) :: BIAS_CORRECT_FILE                             ! File containing radiance bias correction information
    REAL(Kind=GbcsReal) :: BIAS_TIME_STEP                               ! Time step of bias correction file in hours : -1 means no time stepping
    INTEGER :: BIAS_NO_DAYS_AVERAGE                                     ! Number of days to store and average over for the constant bias values
    REAL(KIND=GbcsReal) :: BIAS_PCLR_THRESHOLD
    LOGICAL :: RAD_BIAS_ONE_ALGO(Gbcs_Max_Chans)                        ! Determine if no day/night separation of algorithm
    INTEGER :: RAD_BIAS_ALGO_DAY(Gbcs_Max_Chans)                        ! single or daytime algorithm type
    INTEGER :: RAD_BIAS_ALGO_NIGHT(Gbcs_Max_Chans)                      ! nighttime algorithm type
    LOGICAL :: RAD_BIAS_APPLY_CLOUD                                     ! Apply radiance bias to cloud mask
    INTEGER :: physical_nday                                            ! Number of channels to use in OE daytime
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: physical_day_list             ! List of channels to usein physical retrieval (day)
    INTEGER :: physical_nnight                                          ! Number of channels to use in OE nighttime
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: physical_night_list           ! List of channels to usein physical retrieval (night)
    INTEGER :: physical_nstates                                         ! Number of states requested
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: physical_states               ! List of states requested
    CHARACTER(LEN=200) :: ExtraFileName                                 ! Spare space for an extra file name
  END TYPE JobDescriptor    


  TYPE Satellite
    CHARACTER(LEN=20) :: Name                                           ! Satellite name
    INTEGER :: Platform_Num                                             ! Platform number
    INTEGER :: Platform_ID                                              ! Platform ID number
    INTEGER :: Sensor_ID                                                ! Platform sensor ID number
    INTEGER :: N_Chans                                                  ! Number of channels available
    INTEGER, ALLOCATABLE, DIMENSION(:) :: Chan_ID                       ! Channel ID numbers for satellite
    INTEGER, ALLOCATABLE, DIMENSION(:) :: Chan_Map                      ! Channel ID number mappingd to internal GBCS indices
    REAL(KIND=GbcsReal), ALLOCATABLE, DIMENSION(:) :: NedT              ! Channel Noise Equivalent Differential Temperature
    REAL(KIND=GbcsReal), ALLOCATABLE, DIMENSION(:) :: CWN               ! Channel Central Wave Number
    REAL(KIND=GbcsReal), ALLOCATABLE, DIMENSION(:) :: P_Offset          ! Planck Weighting Correction Offset
    REAL(KIND=GbcsReal), ALLOCATABLE, DIMENSION(:) :: P_Slope           ! Planck Weighting Correction Slope
    TYPE(LatLon) :: Earth_Coords                                        ! Satellite Earth Coordinates 
    REAL(KIND=GbcsReal) :: Altitude                                     ! Satellite altitude (km)
    TYPE(LineElem) :: Dims                                              ! Dimensions of imagery
    CHARACTER(LEN=200) :: LandSeaMaskFile                               ! Data file containing land sea mask
  END TYPE Satellite    


  TYPE Imagery
    CHARACTER(LEN=256) :: GbcsDataPath                                  ! GBCS data directory path
    CHARACTER(LEN=20)  :: DataFrmt                                      ! Data format of imagery
    CHARACTER(LEN=200) :: DataDir                                       ! Data directory containing imagery
    CHARACTER(LEN=200) :: DataFile                                      ! Data file containing imagery
    INTEGER :: Platform_ID                                              ! Platform ID number
    INTEGER :: Sensor_ID                                                ! Platform sensor ID number
    TYPE(LineElem) :: Dims                                              ! Dimensions of imagery
    TYPE(LineElem) :: Origin                                            ! Sub-region origin in pixel coordinates
    TYPE(LineElem) :: Npts                                              ! Number of lines and elements in sub-region
    TYPE(LineElem) :: Overlap                                           ! Extra lines and elements in sub-region for padding
    INTEGER        :: id_integer
    LOGICAL :: Scan_Start_End_Available                                 ! Logical flag indicating presence of scan start/end times
    TYPE(DateTime) :: Start_Time                                        ! Start time of first scan line in image
    TYPE(DateTime) :: End_Time                                          ! End time of last scan line in image
    TYPE(DateTime) :: Mean_Time                                         ! Mean time for image
    INTEGER :: YearDayNumber                                            ! Year day number of image
    INTEGER :: N_Chans                                                  ! Number of channels to load
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Data_Map                      ! Input Channel mapping to satellite channel indices
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Gbcs_Map                      ! Input Channel mapping to internal GBCS indices
    REAL(KIND=GBcsReal), DIMENSION(Gbcs_Max_Chans) :: NEdT              ! Noise Equivalent dT
    REAL(KIND=GBcsReal), DIMENSION(Gbcs_Max_Chans) :: CWN               ! Central wave number
    REAL(KIND=GBcsReal), DIMENSION(Gbcs_Max_Chans) :: P_Offset          ! Planck correction offset
    REAL(KIND=GBcsReal), DIMENSION(Gbcs_Max_Chans) :: P_Slope           ! Planck correction slope
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans) :: Chan_Data            ! Thermal Imagery data
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans) :: Chan_LSD             ! Thermal Imagery Local Standard Deviation data
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: Lat             ! Imagery pixel latitudes
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: Lon             ! Imagery pixel longitudes
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: SatZA           ! Imagery pixel satellite zenith angles
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: SolZA           ! Imagery pixel solar zenith angles
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: RelAz           ! Imagery pixel relative azimuth angles
    CHARACTER(LEN=200) :: LandSeaMaskFile                               ! Data file containing land sea mask
    INTEGER(KIND=GbcsInt1), DIMENSION(:,:), ALLOCATABLE :: LandSeaMask  ! Imagery land/sea mask
    INTEGER(KIND=GbcsInt2), DIMENSION(:,:), ALLOCATABLE :: CloudMask    ! Operational cloud mask     
    INTEGER(KIND=GbcsInt2), DIMENSION(:,:), ALLOCATABLE :: ClearingFlags ! Operational cloud clearing flags
    INTEGER(KIND=GbcsInt2), DIMENSION(:,:), ALLOCATABLE :: QualityFlags ! Operational image quality control flags
    INTEGER(KIND=GbcsInt1), DIMENSION(:,:), ALLOCATABLE :: Bad_Data     ! Imagery land/sea mask
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: dBT_Atm         ! Atmospheric solar scattering BT correction
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: edBT_Atm        ! Error in Atmospheric scattering
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: dBT_Glint       ! Sunglint BT correction
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: edBT_Glint      ! Error in Sunglint correction
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: BT_MOD          ! BT corrected for solar contamination
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: eBT_MOD         ! Error in corrected BT
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: Surface_Class               ! Land surface class pixel map for emissivity correction to BT
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: DEM                         ! Digital elevation model
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans) :: Albedo               ! Channel Surface Albedo
    LOGICAL :: OCEAN_ONLY                                               ! Only analyse over oceans 
    LOGICAL :: Use_Cloudy_Prior                                         ! Use cloudy prior information
    LOGICAL :: Use_Cnst_Bias                                            ! Has a constant bias been subtracted from the data
    LOGICAL :: TEST_RADIANCE_BIAS_CORRECT                               ! Calculate Radiance Bias correction without applying it
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Cnst_Bias         ! Constant Bias value if used    
    LOGICAL :: Bias_Correct_Cld                                         ! Logical to apply bias correction to cloud detection 
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: Background_SST  ! background SST from a previous run
! Extra fields needed for UofE AVHRR data
    CHARACTER(LEN=8) :: AVHRR_Name
    REAL(KIND=GbcsReal), ALLOCATABLE :: ICT_Temp(:)
    INTEGER, ALLOCATABLE :: scanLineNumber(:)
    LOGICAL, ALLOCATABLE :: badTime(:)
    LOGICAL, ALLOCATABLE :: badNavigation(:)
    LOGICAL, ALLOCATABLE :: badCalibration(:)
    TYPE(DateTime), ALLOCATABLE :: scanline_time(:)
    TYPE(DateTime) :: Reference_Time
    REAL(KIND=GbcsReal), ALLOCATABLE :: scanline_dtime(:)
    REAL(KIND=GbcsReal), ALLOCATABLE :: pixel_dtime(:,:)
    REAL(KIND=GbcsReal) :: PReflToRad(Gbcs_Max_Chans)
    INTEGER(KIND=GbcsInt4), ALLOCATABLE :: CLAVRxMask(:,:)
    INTEGER(KIND=GbcsInt4), ALLOCATABLE :: CLAVRxPrb(:,:)
  END TYPE Imagery


  TYPE Forecast_Model
    CHARACTER(LEN=256) :: Name = " "                                    ! Name of Forecast Model
    CHARACTER(LEN=20)  :: DataFrmt = " "                                ! Data Format used to store forecast fields
    CHARACTER(LEN=200) :: DataDir  = " "                                ! Data directory containing forecast fields
    CHARACTER(LEN=200) :: DataFile = " "                                ! Data file containing forecast fields
    CHARACTER(LEN=200) :: LandSeaMaskFile                               ! File containg separated land/sea mask if needed
    TYPE(Grid_Descriptor) :: Grid                                       ! Grid data structure defining model grid
    INTEGER,             DIMENSION(:,:), ALLOCATABLE :: Elems           ! Image element data for image grid
    INTEGER,             DIMENSION(:,:), ALLOCATABLE :: Lines           ! Image line data for image grid
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: Lat             ! Latitude data for Earth grid
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: Lon             ! Longitude data for Earth grid
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: SatZA           ! Satellite zenith angle at each model grid point
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: SolZA           ! Solar zenith angle at each model grid point
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: SatAz           ! Satellite azimuth angle at each model grid point
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: SolAz           ! Solar azimuth angle at each model grid point
    REAL(KIND=GbcsReal), DIMENSION(:,:), POINTER     :: RelAz           ! Solar zenith angle at each model grid point
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: Surface_Type                ! Surface type definition: 0 = land, 1 = sea, 2 = ice
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: Surface_Class               ! Surface land class
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: Flags                       ! NWP info/quality flags
    INTEGER :: N_2D_Model_Fields = 0                                    ! Number of 2D model fields available
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_2D_ID                  ! 2D field ID numbers for model
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_2D_Map                 ! 2D field ID numbers for GBCS
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_2D_Units               ! 2D field model units
    CHARACTER(LEN=64), DIMENSION(Gbcs_Max_Fields) :: Field_2D_Name      ! 2D field name for model ie NetCDF var name
    INTEGER :: N_3D_Model_Fields = 0                                    ! Number of 3D model fields available
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_3D_ID                  ! 3D field ID numbers for model
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_3D_Map                 ! 3D field ID numbers for GBCS
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Field_3D_Units               ! 3D field model units
    CHARACTER(LEN=64), DIMENSION(Gbcs_Max_Fields) :: Field_3D_Name      ! 3D field name for model ie NetCDF var name
    INTEGER :: N_2D_Fields = 0                                          ! Number of 2-D fields used
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Data_Map_2D                  ! 2-D field mapping for model data
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Gbcs_Map_2D                  ! 2-D field mapping internal to GBCS
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Fields)  :: Srf                ! Surface fields
    INTEGER, DIMENSION(Gbcs_Max_Fields)  :: Srf_Index                   ! Surface fields
    INTEGER :: N_3D_Fields = 0                                          ! Number of 3-D fields used
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Data_Map_3D                  ! 3-D field mapping for model data
    INTEGER, DIMENSION(Gbcs_Max_Fields) :: Gbcs_Map_3D                  ! 3-D field mapping internal to GBCS
    TYPE(Data3D_R32), DIMENSION(Gbcs_Max_Fields)  :: Atm                ! Atmospheric fields
    INTEGER, DIMENSION(Gbcs_Max_Fields)  :: Atm_Index                   ! Atmospheric fields
    INTEGER :: NAbsorb = 0                                              ! Number of absorber fields available
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_ID                  ! ID for available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_Units               ! Units of available absorbers
    INTEGER :: NAerosol                                                  ! Number of absorber fields available
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Aerosol_ID                  ! ID for available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Aerosol_Units               ! Units of available absorbers
    REAL(KIND=GbcsReal) :: eSST                                         ! Error in the modelled SST (K)
    REAL(KIND=GbcsReal) :: eTCWV                                        ! Error in the modelled TCWV (fractional)
    REAL(KIND=GbcsReal) :: eU10                                         ! Error in the modelled 10m Wind Speed (m/s)
    LOGICAL :: RTM_Format = .FALSE.                                     ! Profile in format required by RTM: default = .FALSE.
  END TYPE Forecast_Model


  TYPE High_Res_SST_Struct
    CHARACTER(LEN=256) :: Name                                          ! Name of Climatology
    CHARACTER(LEN=20)  :: DataFrmt                                      ! Data Format used to store forecast fields
    CHARACTER(LEN=200) :: DataDir                                       ! Data directory containing forecast fields
    CHARACTER(LEN=200) :: DataFile                                      ! Data file containing forecast fields
    TYPE(Grid_Descriptor) :: Grid                                       ! Grid data structure defining model grid
    REAL(KIND=GbcsReal), DIMENSION(:,:,:), ALLOCATABLE :: SST           ! Real SST Data
    INTEGER :: Units                                                    ! Gbcs data units flag, e.g. Gbcs_Units_Kelvin, Gbcs_Units_Celsius
    INTEGER :: Decade_Loaded
  END TYPE High_Res_SST_Struct


  TYPE RTM_Descriptor
    CHARACTER(LEN=256) :: Name                                          ! RTM Name
    CHARACTER(LEN=256) :: DataPath                                      ! RTM data path
    INTEGER :: ID                                                       ! RTM ID Number 
    INTEGER :: Use_Emiss_Model                                          ! Flag to determine use of emissivity model  ( 0 = don't use, 1 = use )
    INTEGER :: Use_Solar_Angle                                          ! Flag to determine use of solar scattering in CRTM ( 0 = don't use, 1 = use )
    INTEGER :: NLevels                                                  ! Number of levels used by RTM
    INTEGER :: NLayers                                                  ! Number of layers used by RTM
    INTEGER :: N_Chans_Possible                                         ! Number of channels RTM can calculate for sensor
    INTEGER, DIMENSION(2,Gbcs_Max_Chans) :: RTM_Sat_Map                 ! Mapping between RTM indices and satellite channel indices
    INTEGER :: N_Chans_To_Calc                                          ! Number of channels to be calculated
    INTEGER, DIMENSION(2,Gbcs_Max_Chans) :: RTM_Chan_Map                ! Mapping between RTM indices , input channel indices, and GBCS indices
    INTEGER, DIMENSION(Gbcs_Max_Chans)   :: Gbcs_Map
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: BTR              ! Modelled Brightness Temperature / Reflectance
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: dBT              ! Solar BT correction
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: RAD              ! Modelled Radiance
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: TAU              ! Modelled Transmittance
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: surf_emis        ! Surface emissivity used
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: surf_refl        ! Surface reflectivity used
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: Opt_Depth        ! Modelled Optical Depth
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE:: TLN              ! RTM Tangent Linear
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: TLN_emiss        ! Tangent Linear wrt. emissivity
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans)  :: TLN_refle        ! Tangent Linear wrt. reflectivity
    TYPE(Atm_Profile), DIMENSION(4) :: Profiles                         ! Array of profile data structures on RTM levels for pixel neighbours
    INTEGER, DIMENSION(4) :: Qual_Flag                                  ! Quality Flag for Grid Mask profiles
  END TYPE RTM_Descriptor


  TYPE RTM_Interface
    INTEGER :: N_RTMs                                                   ! Number of RTMs avaliable
    INTEGER, DIMENSION(Gbcs_Max_RTMs) :: ID_Map                         ! RTM mapping to GBCS indices
    TYPE(RTM_Descriptor), DIMENSION(Gbcs_Max_RTMs) :: RTM_Set           ! Collection of RTMs
    INTEGER :: NLines                                                   ! Number of lines in model grid
    INTEGER :: NElems                                                   ! Number of elements in model grid
    INTEGER :: NAbsorb                                                  ! Number of available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_ID                  ! ID for available absorbers
    INTEGER, DIMENSION(Gbcs_Max_Absorb) :: Absorber_Units               ! Units of available absorbers
    INTEGER :: Num_Profiles_Processed                                   ! Running count of the number of profiles processed    ***
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: Run_Map                     ! Map of cells already processed
    INTEGER, DIMENSION(4) :: Grid_Mask                                  ! Mask defining if profile already processed
    LOGICAL :: Tan_Linear_Loaded = .FALSE.                              ! Flag to check if tangent linear structure initialised
    INTEGER :: N_Chans                                                  ! Total number of channels processed by RTMs
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Data_Map                      ! Input Channel mapping to satellite channel indices
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Gbcs_Map                      ! Input Channel mapping internal to GBCS indices
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: RTM_Map                       ! RTM-channel map 
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: BTR                 ! Modelled Brightness Temperature / Reflectance
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: dBT                 ! Solar contribution
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: RAD                 ! Modelled Radiance
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: TAU                 ! Modelled Transmittance
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: Opt_Depth           ! Modelled Optical Depth
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: eBTR              ! Errors in background BT/Reflectance estimates
    REAL(KIND=GbcsReal), DIMENSION(:,:,:,:), ALLOCATABLE :: H_Matrix    ! H Matrix Store
    REAL(KIND=GbcsReal), DIMENSION(4) :: SatZA                          ! Profile Satellite Zenith Angles calculated for polar orbiters
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: Emissivity          ! Profile Emissivity
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: dEmiss              ! Modelled dBT / dEmissivity
    TYPE(Data2D_R32), DIMENSION(Gbcs_Max_Chans)  :: dRefle              ! Modelled dBT / dReflectivity
    INTEGER :: Use_Solar_Correction_37                                  ! Does the RTM use a solar scatt. model for 3.7 (0=no/1=yes)
    LOGICAL :: Use_Aerosol
  END TYPE RTM_Interface

  TYPE Aux_Data
    CHARACTER(LEN=256)   :: GbcsDataPath                                ! GBCS data directory path
    TYPE(Forecast_Model) :: Model                                       ! Structure for forecast model fields
    CHARACTER(LEN=256)   :: CovMatFile                                  ! Name of file containing covariance matirx
    TYPE(Cov_Matrix)     :: CovMat                                      ! Structure for covariance matrix
    LOGICAL              :: Cov_matrix_loaded = .FALSE.                 ! Flag to check if covariance matirx structure has been set
    TYPE(Atm_Profile), DIMENSION(4) :: Profiles                         ! Structure for pixel neighbour profiles - used for conversion to RTM format
    TYPE(High_Res_SST_Struct) :: SST_Clim                               ! Structure for storing high resolution SST climatology or prior
    INTEGER, DIMENSION(2,5) :: N_Bayes_Chans                            ! Total number of channels used for Bayesian calculation
    INTEGER, DIMENSION(Gbcs_Max_Chans,5) :: Bayes_Spec_Chans            ! Channels used for Bayesian Spectral calculation (1 = Day/Twilight, 2 = Night)
    INTEGER, DIMENSION(Gbcs_Max_Chans,5) :: Bayes_Text_Chans            ! Channels used for Bayesian Textural calculation (1 = Day/Twilight, 2 = Night)
    INTEGER, DIMENSION(Gbcs_Max_Chans)   :: Bayes_RTM_Map               ! RTM-channel map for Bayesian Spectral calculations 
    LOGICAL              :: PDF_Loaded = .FALSE.                        ! Flag to check if PDF LUTs already loaded
    CHARACTER(LEN=5) :: SD_PDF_Num_Str                                  ! Spectral daytime PDF channel map
    CHARACTER(LEN=5) :: ST_PDF_Num_Str                                  ! Spectral twilight PDF channel map
    CHARACTER(LEN=5) :: SN_PDF_Num_Str                                  ! Spectral nighttime PDF channel map
    CHARACTER(LEN=5) :: TD_PDF_Num_Str                                  ! Textural daytime PDF channel map
    CHARACTER(LEN=5) :: TT_PDF_Num_Str                                  ! Textural twilight PDF channel map
    CHARACTER(LEN=5) :: TN_PDF_Num_Str                                  ! Tectural nighttime PDF channel map
    TYPE(PDF_LUT_Struct) :: PDF_SD                                      ! Structure for Spectral-Daytime Cloudy PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_ST                                      ! Structure for Spectral-Twilight Cloudy PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_SN                                      ! Structure for Spectral-Nighttime Cloudy PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_TD                                      ! Structure for Textural-Daytime Cloudy PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_TT                                      ! Structure for Textural-Twilight Cloudy PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_TN                                      ! Structure for Textural-Nighttime Cloudy PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_TD_CLR                                  ! Structure for Textural-Daytime Clear PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_TT_CLR                                  ! Structure for Textural-Twilight Clear PDF Look Up Table
    TYPE(PDF_LUT_Struct) :: PDF_TN_CLR                                  ! Structure for Textural-Nighttime Clear PDF Look Up Table
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE :: VIS_CLD_PDF
    CHARACTER(LEN=15) :: VIS_PDF_ID                                     ! File identifire for visible PDF
    TYPE(TestPoint_Descriptor) :: TestPoint                             ! Test point descriptor to allow output of test data
    LOGICAL              :: Use_HighRes_SST                             ! Use High Resolution SST
    LOGICAL              :: Aux_Data_Loaded = .FALSE.                   ! Flag to check if auxiliary data has been loaded
    LOGICAL              :: Dust_Loaded = .FALSE.                       ! Flag to check if auxiliary data has been loaded
    REAL(KIND=GbcsReal), DIMENSION(2,4,72)  :: cld_prior                ! Prior probability of cloud (land/sea,season,latitude)
    REAL(KIND=GbcsReal), DIMENSION(4,15,12) :: dust_prior               ! Prior probability of dust (season,longitude,latitude)
    LOGICAL              :: cldPrior_Loaded = .FALSE.                   ! Flag to check if PDF cld prior is already loaded
    CHARACTER(LEN=256)   :: pdf_filename
    TYPE(PDF_Struct)     :: pdf_TIR_min
    TYPE(PDF_Struct)     :: pdf_TIR_max
    TYPE(PDF_Struct)     :: pdf_TIR_dual
    TYPE(PDF_Struct)     :: pdf_VIS
    TYPE(PDF_Struct)     :: pdf_VIS_dual
    TYPE(PDF_Struct)     :: pdf_TXT
    TYPE(PDF_Struct)     :: pdf_TXT_clr
    TYPE(PDF_Struct)     :: pdf_TXT2
    TYPE(PDF_Struct)     :: pdf_TXT2_clr
    TYPE(PDF_Struct)     :: pdf_prior
    TYPE(PDF_Struct)     :: pdf_TXT_dual_cld
    TYPE(PDF_Struct)     :: pdf_TXT_dual_clr
    TYPE(PDF_Struct)     :: pdf_TXT2_dual_cld
    TYPE(PDF_Struct)     :: pdf_TXT2_dual_clr
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE    :: KperATPL       ! How sim'd dust BTs vary w atm path (12um, 11um, 3.9um) [K per unit atmos path length]
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE  :: dust_bts       ! BTs sim'd for dust at sat zen ang=0deg (12um, 11um, 3.9um)
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE    :: dust_sst       ! SST [K] for profs used in dust sims
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE    :: dust_tcwv      ! TCWV [g/m2] for profs used in dust sims
  END TYPE Aux_Data


END MODULE GbcsTypes
