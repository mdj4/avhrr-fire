!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsPixel
!
! PURPOSE:
!       Contains the definition of a Pixel data structure that holds all the
!       data for a single pixel used to calculate the probability of clear scene
!
! CATEGORY:
!       
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsPixel
!
! PUBLIC DATA:
!
! MODULES:
!
! CONTAINS:
!
! DERIVED TYPES:
!       ImagePixel
!
! CREATION HISTORY:
!       Written by:   Chris Old 03/01/2005
!                     IAES, University of Edinburgh
!       Moved secondary types to separate module:
!                     Owen Embury 03/08/2008
!
!   Copyright 2008 Chris Merchant, Chris Old, and Owen Embury
!   The Institute of Atmospheric and Environmental Science
!   The University of Edinburgh, UK
!
!   This file is part of the GBCS software package.
!
!   The GBCS software package is free software: you can redistribute it 
!   and/or modify it under the terms of the GNU General Public License 
!   as published by the Free Software Foundation, either version 3 of 
!   the License, or (at your option) any later version.
!
!   The GBCS software package is distributed in the hope that it will be 
!   useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
!   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with the GBCS software package.  
!   If not, see <http://www.gnu.org/licenses/>.
!M-
!----------------------------------------------------------------------------------

MODULE GbcsPixel

  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds
  USE GbcsConstants, ONLY: Gbcs_Max_Chans
  USE GbcsDateTime,  ONLY: DateTime

  USE GbcsBaseTypes

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE


  TYPE ImagePixel
    TYPE(LineElem) :: ImageCoord                                        ! Pixel image coordinates (line and element)
    TYPE(LineElem) :: ImageCoordOffset                                  ! Pixel image coordinates offset to image segment
    TYPE(LatLon) :: EarthCoord                                          ! Latitude and longitude of pixel
    TYPE(DateTime) :: Obs_Time                                          ! Observation time for pixel data
    INTEGER :: Surface_Type                                             ! Surface Type definition: 0 = land, 1 = sea, 2 = ice/snow
    INTEGER :: Land_Surface_Type                                        ! Land Surface Type 3 = Forest etc, see Gbcs_Constants.f90 
    INTEGER :: Flags
    INTEGER :: Class                                                    ! Pixel classification: See GbcsConstants.f90 for values
    INTEGER :: LandSea                                                  ! Land/Sea flag
    INTEGER :: DayNight                                                 ! Day/Twil/Night flag: 0 = day, 1 = twilight, 2 = night
    REAL(KIND=GbcsReal) :: SatZA                                        ! Satellite zenith angle at pixel location
    REAL(KIND=GbcsReal) :: SolZA                                        ! Solar zenith angle at pixel location
    REAL(KIND=GbcsReal) :: SatAz                                        ! Satellite azimuth angle at pixel location
    REAL(KIND=GbcsReal) :: SolAz                                        ! Solar azimuth angle at pixel location
    REAL(KIND=GbcsReal) :: RelAz                                        ! Satellite-Solar relative azimuth angle at pixel location
    REAL(KIND=GbcsReal) :: SSTb                                         ! Background surface temperature
    REAL(KIND=GbcsReal) :: eSSTb                                        ! Error in background surface temperature
    REAL(KIND=GbcsReal) :: TCWVb                                        ! Background total column water vapour
    REAL(KIND=GbcsReal) :: eTCWVb                                       ! Error in background total column water vapour
    REAL(KIND=GbcsReal) :: Tdiff                                        ! SST - min profile temperature
    REAL(KIND=GbcsReal) :: two_m_temp                                   ! 2 m temperature
    REAL(KIND=GbcsReal) :: Wind_10m                                     ! Background 10m wind speed
    REAL(KIND=GbcsReal) :: eU10b                                        ! Error in background 10m wind speed
    REAL(KIND=GbcsReal) :: Elev                                         ! Pixel elevation
    REAL(KIND=GbcsReal) :: Elevb                                        ! Background pixel elevation estimatede from profile elev
    REAL(KIND=GbcsReal) :: Iceb                                         ! Background pixel elevation estimatede from profile elev
    REAL(KIND=GbcsReal) :: Salinity                                     ! Salinity of water, dg/l (sea water = 350)
    REAL(KIND=GbcsReal) :: Instrument_Temp                              ! Salinity of water, dg/l (sea water = 350)
    TYPE(Model_Map_2D)  :: FMMap                                        ! Map of RTM processed forecast fields
    TYPE(WeightMP)      :: LocMP(4)                                     ! Location of nearest neighbour model profiles
    LOGICAL             :: profiles_do_not_match
    INTEGER :: N_Chans                                                  ! Number of channels loaded
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Chan_ID                       ! Channel ID Numbers for loaded channels
    INTEGER, DIMENSION(2) :: N_Bayes_Chans                              ! Number of channels used for Bayesian calculation ( 1=Spectral , 2=Textural )
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Bayes_Spec_Chans              ! Channel map for Bayesian Spectral calculation
    INTEGER, DIMENSION(Gbcs_Max_Chans) :: Bayes_Text_Chans              ! Channel map for Bayesian Textural calculation
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Yobs              ! Observed Y value
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: eYo               ! Observed Y value error
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Ybkg              ! Background modelled Y value (prior)
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Ybkg_uncorr       ! Background modelled Y value (prior) - no radiance bias correction
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: eYb               ! Background modelled Y value error (prior)
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: NedT              ! Observed Noise Equivalent Differential Temperature
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: LSD               ! Observed thermal channel LSD
    INTEGER,             DIMENSION(Gbcs_Max_Chans) :: nLSD              ! Number of cells used to calculate LSD
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: eLSD              ! Observed thermal channel LSD error
    REAL(KIND=GbcsDble), DIMENSION(Gbcs_Max_Chans) :: dY                ! Thermal BT difference vector
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: PDFCST            ! PDF for Clear Scene Texture
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: dBTdSST           ! BT gradiant with SST
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Emissivity        ! Channel Emissivity
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Emiss_Var         ! Channel Emissivity Variance
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: dBTdEmis          ! BT gradiant with emissivity
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Albedo            ! Channel Surface Albedo
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: BRDF              ! Channel Surface BRDF
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Opt_Depth         ! Channel Optical Depth
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: TAU               ! Channel transmittance
    REAL(KIND=GbcsReal)                            :: dBT_Atm           ! Atmospheric Correction (03x channel)
    REAL(KIND=GbcsReal)                            :: dBT_Glint         ! Atmospheric Correction (03x channel)
    REAL(KIND=GbcsReal), DIMENSION(Gbcs_Max_Chans) :: Bias_Correction   ! Bias Correction applied
    INTEGER :: State_Vector_Len                                         ! Length of the state vector used in H & B
    REAL(KIND=GbcsDble), DIMENSION(:,:), ALLOCATABLE :: H_Matrix        ! Tangent linear to RTM  ( H )
    REAL(KIND=GbcsDble), DIMENSION(:,:), ALLOCATABLE :: B_Matrix        ! Difference Covariance matrix   ( B )
    REAL(KIND=GbcsDble), DIMENSION(:,:), ALLOCATABLE :: R_Matrix        ! Obs-Model Covariance matrix    ( R )
    REAL(KIND=GbcsDble), DIMENSION(:,:), ALLOCATABLE :: S_Matrix        ! Total Covariance matrix        ( S )
    REAL(KIND=GbcsDble)                              :: Det_S           ! Determinant of S matrix
    INTEGER :: B_Matrix_Band                                            ! Current Covariance matrix band loaded
    REAL(KIND=GbcsReal) :: CldTIR                                       ! Probability of TIR obs if cloudy from LUT
    REAL(KIND=GbcsReal) :: CldVIS                                       ! Probability of VIS obs if cloudy from LUT
    REAL(KIND=GbcsReal) :: ST                                           ! Retrieved surface temperature
    REAL(KIND=GbcsReal) :: SigmaST                                      ! Error in retrieved surface temperature
    INTEGER                                        :: n_classes         ! Number of classes for Bayes. calc.
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: Prior_Prob        ! Prior probability that pixel belongs to each class (clr, cld, (dust))
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: PDF_spec          ! Probability of observations for each class (clr, cld, (dust))
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: PDF_text          ! Probability of observations for each class (clr, cld, (dust))
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: Post_Prob         ! Posterior probability that pixel belongs to each class (clr, cld, (dust))
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: Post_Spec         ! Posterior probability that pixel belongs to each class (clr, cld, (dust))
    REAL(KIND=GbcsDble), DIMENSION(:), ALLOCATABLE :: Post_Text         ! Posterior probability that pixel belongs to each class (clr, cld, (dust))
    REAL(KIND=GbcsDble) :: OE_Chi2                                      ! OE Chisq value
    REAL(KIND=GbcsDble) :: OE_sst                                       ! OE SST value
    REAL(KIND=GbcsDble) :: OE_tcwv                                      ! OE Total Column Water Vapour 
  END TYPE ImagePixel


  ! -----------------
  ! Module parameters
  ! -----------------
 
  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Globals/GbcsPixel.f90'

END MODULE GbcsPixel
