!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsPDFLookup
!
! PURPOSE:
!       Module for Probability Density Funtion Lookup tables. Load data
!       from NetCDF files and retrieve probability for a given Gbcs Pixel
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsPDFLookup
!
! PUBLIC DATA:
!
! MODULES:
!       GbcsKinds
!       GbcsPixel
!
! CONTAINS:
!       Load_PDF
!       Free_PDF
!       PDF_Index
!       PDF_Index_Fraction
!       PDF_Get_LUT_Index
!       Lookup_PDF
!
! DERIVED TYPES:
!       PDF_Struct
!
! NOTES:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 29/02/2008
!                     IAES, University of Edinburgh
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


MODULE GbcsPDFLookup
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds
  USE GbcsPixel
  
  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE


  ! -----------------
  ! Module parameters
  ! -----------------

  INTEGER, PRIVATE, PARAMETER :: Max_PDF_Dims = 7

  INTEGER, PARAMETER :: Gbcs_State_Latitude   = 1
  INTEGER, PARAMETER :: Gbcs_State_Longitude  = 2
  INTEGER, PARAMETER :: Gbcs_State_LandSea    = 3
  INTEGER, PARAMETER :: Gbcs_State_DayOfYear  = 4
  INTEGER, PARAMETER :: Gbcs_State_SatZenith  = 5
  INTEGER, PARAMETER :: Gbcs_State_SolZenith  = 6
  INTEGER, PARAMETER :: Gbcs_State_RelAzimuth = 7
  INTEGER, PARAMETER :: Gbcs_State_Surface_Type = 8 


  INTEGER, PARAMETER :: Index_Invalid  = -1
  INTEGER, PARAMETER :: Index_Channel  = 1
  INTEGER, PARAMETER :: Index_ChanDiff = 2
  INTEGER, PARAMETER :: Index_NWPVar   = 3
  INTEGER, PARAMETER :: Index_StateVar = 4
  INTEGER, PARAMETER :: Index_Texture  = 5
  INTEGER, PARAMETER :: Index_VTexture = 6

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Bayesian/GbcsPDFLookup.f90'


  TYPE PDF_Struct
    INTEGER                                        :: NDims
    INTEGER,             DIMENSION(Max_PDF_Dims)   :: index_type
    INTEGER,             DIMENSION(Max_PDF_Dims)   :: index1
    INTEGER,             DIMENSION(Max_PDF_Dims)   :: index2
    REAL(KIND=GbcsReal), DIMENSION(Max_PDF_Dims)   :: lower_bound = 0
    REAL(KIND=GbcsReal), DIMENSION(Max_PDF_Dims)   :: upper_bound = 1
    REAL(KIND=GbcsReal), DIMENSION(Max_PDF_Dims)   :: bin_width   = 1
    INTEGER,             DIMENSION(Max_PDF_Dims)   :: num_bins    = 1
    INTEGER,             DIMENSION(Max_PDF_Dims)   :: stride
    INTEGER,             DIMENSION(Max_PDF_Dims)   :: count
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE :: LUT
  END TYPE PDF_Struct

CONTAINS

!------------------------------------------------------------------------------
!F+
! NAME:
!       Load_PDF
!
! PURPOSE:
!       Read a PDF structure from the given NetCDF file
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Load_PDF(filename, PDF)
!
! INPUT ARGUMENTS:
!       filename
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       PDF
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! RESULT:
!       0   - success
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
!       Written by:   Owen Embury 29/02/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------

#ifdef USE_NETCDF
  FUNCTION Load_PDF(filename, PDF, lut_name) &
            RESULT (status)
    USE GbcsErrorHandler
    USE netcdf
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER( * ),           INTENT(IN)    :: filename
    TYPE(PDF_Struct),         INTENT(INOUT) :: PDF
    CHARACTER( * ), OPTIONAL, INTENT(IN) :: lut_name
    
    ! ------
    ! Result
    ! ------
    INTEGER :: status

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Load_PDF'
    CHARACTER( * ),  PARAMETER :: DEFAULT_LUT  = 'data'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER                 ::  err         ! - Temporary for checking return status
    INTEGER, DIMENSION(10)  ::  stat        ! - Array version for checking return stat.
    INTEGER                 ::  ncid, varid
    INTEGER                 ::  nDims, nAtts
    INTEGER                 ::  nElems, iDim, dimLen, i_err
    INTEGER, DIMENSION(nf90_max_var_dims) :: dimIds
    CHARACTER(len = nf90_max_name)        :: dimName
    
    INTEGER   :: attInt


    ! -- Set default status
    status = -1

    status = Free_PDF(PDF)

    err = nf90_open(TRIM(filename), NF90_NOWRITE, ncid)
    IF (err /= nf90_noerr) THEN
      CALL Gbcs_Error(.TRUE.,                                &
                      'Error opending PDF NetCDF:'//TRIM(filename), &
                      ROUTINE_NAME,                          &
                      MODULE_NAME)
      CALL Gbcs_Error(.TRUE., TRIM(nf90_strerror(err)))
      RETURN
    END IF
    
!    err = nf90_inquire(ncid, nDimensions, nVariables, nAttributes)

    ! -- Find PDF variable
    IF( PRESENT(lut_name) )THEN
      err = nf90_inq_varid(ncid, lut_name, varid)
    ELSE
      err = nf90_inq_varid(ncid, DEFAULT_LUT, varid)
    END IF

    IF (err /= nf90_noerr) THEN
      CALL Gbcs_Debug(.TRUE.,                                &
                      "PDF variable not found",              &
                      ROUTINE_NAME,                          &
                      MODULE_NAME)
      err = nf90_close(ncid)
      RETURN
    END IF

    err = nf90_inquire_variable(ncid, varid, nDims=nDims, dimids=dimIds, nAtts=nAtts)
    PDF % nDims = nDims
    
    ! -- Determine size of lookup table
    nElems = 1
    DO iDim = 1, nDims
      err = nf90_inquire_dimension(ncid, dimIds(iDim), dimName, dimLen)

      ! -- get_var parameters necessary as we are reading nD data into 1D array
      PDF % count(iDim) = dimLen
      PDF % stride(iDim) = nElems
      nElems = nElems * dimLen
    END DO

    ALLOCATE(PDF%LUT(nElems), STAT=err)
    IF (err /= 0) THEN
      CALL Gbcs_Error(.TRUE.,                                &
                      'Unable to allocate memory for PDF',   &
                      ROUTINE_NAME,                          &
                      MODULE_NAME)
      err = nf90_close(ncid)
      RETURN
    END IF
    
    ! -- and actually read the lookup table from the NetCDF file
    err = nf90_get_var(ncid, varid, PDF%LUT, count=PDF%count, map=PDF%stride)
    IF (err /= nf90_noerr) THEN
      CALL Gbcs_Error(.TRUE.,                                &
                      'Unable to read PDF:'//TRIM(filename), &
                      ROUTINE_NAME,                          &
                      MODULE_NAME)
      CALL Gbcs_Error(.TRUE., TRIM(nf90_strerror(err)))
      err = nf90_close(ncid)
      RETURN
    END IF
    
    
    ! -- loop though dimensions one more time to get start/end/bins etc
    !    dimension info is stored as attributes of identically named
    !    variable.
    DO iDim = 1, nDims
      stat = nf90_noerr
      stat(1) = nf90_inquire_dimension(ncid, dimIds(iDim), dimName, dimLen)
      stat(2) = nf90_inq_varid(ncid, dimName, varid)

      stat(3) = nf90_get_att(ncid, varid, "lower_bound", PDF%lower_bound(iDim))
      stat(4) = nf90_get_att(ncid, varid, "upper_bound", PDF%upper_bound(iDim))
      stat(5) = nf90_get_att(ncid, varid, "bin_width",   PDF%bin_width(iDim))
      stat(6) = nf90_get_att(ncid, varid, "num_bins",    PDF%num_bins(iDim))

      IF( ANY(stat /= nf90_noerr) ) THEN
        CALL Gbcs_Error(.TRUE.,                                &
                        'Error reading NetCDF attributes : '//TRIM(filename), &
                        ROUTINE_NAME,                          &
                        MODULE_NAME)
        DO i_err = 1, 6
          IF(stat(i_err) /= nf90_noerr) THEN
            CALL Gbcs_Error(.TRUE., TRIM(nf90_strerror(stat(i_err))))
          END IF
        END DO
        err = nf90_close(ncid)
        RETURN
      END IF


      PDF%index_type(iDim) = Index_Invalid

      ! -- Determine what type of indexing to use
      stat(1) = nf90_get_att(ncid, varid, "gbcs_chan", attInt)
      stat(2) = nf90_get_att(ncid, varid, "diff_chan", attInt)
      stat(3) = nf90_get_att(ncid, varid, "nwp_var",   attInt)
      stat(4) = nf90_get_att(ncid, varid, "gbcs_state",attInt)
      stat(5) = nf90_get_att(ncid, varid, "text_chan", attInt)
      stat(6) = nf90_get_att(ncid, varid, "text_vchan", attInt)

      
      ! -- Channel dimension
      IF( stat(1) == nf90_noerr ) THEN
        err = nf90_get_att(ncid, varid, "gbcs_chan", attInt)
        PDF%index_type(iDim) = Index_Channel
        PDF%index1(iDim)     = attInt
      END IF
      
      ! -- Check if this is a channel difference dimension
      IF( stat(2) == nf90_noerr ) THEN
        err = nf90_get_att(ncid, varid, "diff_chan", attInt)
        PDF%index_type(iDim) = Index_ChanDiff
        PDF%index2(iDim)     = attInt
      END IF
      
      ! -- nwp var dimension
      IF( stat(3) == nf90_noerr ) THEN
        err = nf90_get_att(ncid, varid, "nwp_var", attInt)
        PDF%index_type(iDim) = Index_NWPVar
        PDF%index1(iDim)     = attInt
      END IF
        
      ! -- State dimension
      IF( stat(4) == nf90_noerr ) THEN
        err = nf90_get_att(ncid, varid, "gbcs_state", attInt)
        PDF%index_type(iDim) = Index_StateVar
        PDF%index1(iDim)     = attInt
      END IF

      ! -- Texture dimension
      IF( stat(5) == nf90_noerr ) THEN
        err = nf90_get_att(ncid, varid, "text_chan", attInt)
        PDF%index_type(iDim) = Index_Texture
        PDF%index1(iDim)     = attInt
      END IF

      ! -- Texture dimension (using variance rather than LSD)
      IF( stat(6) == nf90_noerr ) THEN
        err = nf90_get_att(ncid, varid, "text_vchan", attInt)
        PDF%index_type(iDim) = Index_VTexture
        PDF%index1(iDim)     = attInt
      END IF
      
      IF( PDF%index_type(iDim) == Index_Invalid )THEN
        CALL Gbcs_Error(.TRUE., "Unsupported channel dimension")
      END IF
      

      ! -- Check for old-format solar zenith flag
      IF( (PDF%index_type(iDim) == Index_Channel) .AND. &
          (PDF%index1(iDim)     == 0)             ) THEN
        PDF%index_type(iDim) = Index_StateVar
        PDF%index1(iDim)     = Gbcs_State_SolZenith
        
      END IF

    END DO
    

    err = nf90_close(ncid)
    ! -- Should happen - we've already read the data so closing
    !    file shouldn't fail
    IF (err /= nf90_noerr) THEN
      CALL Gbcs_Error(.TRUE., TRIM(nf90_strerror(err)))
      RETURN
    END IF

    status = 0
    
  END FUNCTION Load_PDF
#else
  FUNCTION Load_PDF(filename, PDF, lut_name) &
            RESULT (status)
    USE GbcsErrorHandler
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER( * ),           INTENT(IN)    :: filename
    TYPE(PDF_Struct),         INTENT(INOUT) :: PDF
    CHARACTER( * ), OPTIONAL, INTENT(IN) :: lut_name
    
    ! ------
    ! Result
    ! ------
    INTEGER :: status

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Load_PDF'
    
    CALL Gbcs_Error(.TRUE., 'NetCDF library not available', ROUTINE_NAME, MODULE_NAME)
    status = -1
    
  END FUNCTION Load_PDF
#endif


!------------------------------------------------------------------------------
!F+
! NAME:
!       Free_PDF
!
! PURPOSE:
!       Free the memory allocated for a PDF lookup variable
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Free_PDF(PDF)
!
! INPUT ARGUMENTS:
!       filename
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       PDF
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! RESULT:
!       0   - success
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
!       Written by:   Owen Embury 03/03/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Free_PDF(PDF) &
            RESULT (status)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    TYPE(PDF_Struct),         INTENT(INOUT) :: PDF
    
    ! ------
    ! Result
    ! ------
    INTEGER :: status

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Free_PDF'


    status = 0
    
    IF( ALLOCATED(PDF%LUT) ) THEN
      DEALLOCATE(PDF%LUT, STAT=status)
    END IF
    
  END FUNCTION Free_PDF


!------------------------------------------------------------------------------
!F+
! NAME:
!       PDF_Get_LUT_Index
!
! PURPOSE:
!       Get the LUT index for the supplied pixel
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       array_index = PDF_Get_LUT_Index(PDF, pixel)
!
! INPUT ARGUMENTS:
!       PDF    - PDF structure
!       pixel  - ImagePixel structure
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       PDF
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! RESULT:
!       LUT index along each PDF dimension
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       Calls Free_PDF prior to loading - does not give an error message
!       if an existing PDF is overwritten.  CB - 24/10/12.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Owen Embury 03/08/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION PDF_Get_LUT_Index(PDF, pixel, outofbounds) &
                      RESULT(array_index)
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(PDF_Struct), INTENT(IN)  :: PDF
    TYPE(ImagePixel), INTENT(IN)  :: pixel
    LOGICAL,          INTENT(OUT) :: outofbounds

    ! ------
    ! Result
    ! ------
    INTEGER, DIMENSION(Max_PDF_Dims) :: array_index

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'PDF_Get_LUT_Index'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER                       :: i_dim
    REAL(KIND=GbcsReal)           :: value

    REAL(KIND=GbcsReal), DIMENSION(Max_PDF_Dims) :: values
    array_index = 0
    values      = 0
    outofbounds = .FALSE.

    DO i_dim = 1, PDF%NDims
      SELECT CASE (PDF%index_type(i_dim))
        ! -- Standard BT / reflectance dimension
        CASE (Index_Channel)
          value = pixel%Yobs(PDF%index1(i_dim))

        ! -- Channel difference
        CASE (Index_ChanDiff)
          IF (PDF%index2(i_dim) == -1) THEN
            value = pixel%Yobs(PDF%index1(i_dim)) - pixel%SSTb
          ELSE
            value = pixel%Yobs(PDF%index1(i_dim)) - pixel%Yobs(PDF%index2(i_dim))
          END IF

        ! -- NWP variable
        CASE (Index_NWPVar)
          ! Only support SST for now
          value = pixel%SSTb

        ! -- State variable
        CASE (Index_StateVar)
          SELECT CASE (PDF%index1(i_dim))
            CASE (Gbcs_State_Latitude)
              value = pixel%EarthCoord % Lat

            CASE (Gbcs_State_Longitude)
              value = pixel%EarthCoord % Lon

            CASE (Gbcs_State_LandSea)
              value = pixel%Surface_Type

            CASE (Gbcs_State_SatZenith)
              value = pixel%SatZA

            CASE (Gbcs_State_SolZenith)
              value = pixel%SolZA
 
            CASE (Gbcs_State_Surface_Type)
              value = pixel%Land_Surface_Type

            CASE default
              WRITE(*,*) 'unsupported pdf dim'
              value = PDF%lower_bound(i_dim)
          END SELECT

        ! -- Texture channel
        CASE (Index_Texture)
          value = pixel%LSD(PDF%index1(i_dim))

        ! -- Texture channel
        CASE (Index_VTexture)
          value = pixel%LSD(PDF%index1(i_dim)) * pixel%LSD(PDF%index1(i_dim))

        CASE default
          WRITE(*,*) 'unsupported pdf dim'
          value = PDF%lower_bound(i_dim)

      END SELECT
      values(i_dim) = value

    END DO
    
    array_index = FLOOR((values-PDF%lower_bound) / PDF%bin_width)
    
    IF (ANY((array_index < 0) .OR. (array_index >= PDF%num_bins))) THEN
      outofbounds = .TRUE.
      array_index = MIN(MAX(array_index,0), PDF%num_bins-1)
    END IF
      
  END FUNCTION PDF_Get_LUT_Index


!------------------------------------------------------------------------------
!F+
! NAME:
!       Lookup_PDF
!
! PURPOSE:
!       Retrieve a probability value from the PDF lookup table
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       prob = Lookup_PDF(PDF, pixel)
!
! INPUT ARGUMENTS:
!       PDF    - PDF structure
!       pixel  - ImagePixel structure
!
! OPTIONAL INPUT ARGUMENTS:
!       None
!
! OUTPUT ARGUMENTS:
!       PDF
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! RESULT:
!       prob - probability of 'state' given PDF
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
!       Written by:   Owen Embury 04/03/2008
!                     IAES, University of Edinburgh
!       03/08/2008  OE - Pass pixel structure directly now circular dependency
!                        on GbcsTypes is resolved
!F-
!------------------------------------------------------------------------------
  FUNCTION Lookup_PDF(PDF, pixel) &
               RESULT( pdf_val )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(PDF_Struct),       INTENT(IN) :: PDF
    TYPE(ImagePixel),       INTENT(IN) :: pixel

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsReal) :: pdf_val

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Lookup_PDF'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION(Max_PDF_Dims) :: array_index
    LOGICAL                          :: oob
    
    ! -- 
    IF( ALLOCATED(PDF%LUT) )THEN
      array_index = PDF_Get_LUT_Index(PDF, pixel, oob)
      pdf_val     = PDF%LUT(1 + SUM(array_index * PDF%stride))
    ELSE
      pdf_val = -1
    END IF

  END FUNCTION Lookup_PDF


!------------------------------------------------------------------------------
!F+
! NAME:
!       Lookup_PDF_pw
!
! PURPOSE:
!       Retrieve a probability value from the PDF lookup table. Applies 
!       path length weighted interpolation for satellite zenith angle bin
!       Assumes PDFs created for ATSR viewing geometry.
  FUNCTION Lookup_PDF_pw(PDF, pixel) &
               RESULT( pdf_val )
    USE GbcsConstants
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(PDF_Struct),       INTENT(IN) :: PDF
    TYPE(ImagePixel),       INTENT(IN) :: pixel

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsReal) :: pdf_val

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Lookup_PDF_pw'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION(Max_PDF_Dims) :: array_index
    LOGICAL                          :: oob
    INTEGER                          :: i_dim, z_dim
    REAL(KIND=GbcsReal) :: f
    
    ! -- 
    IF( ALLOCATED(PDF%LUT) )THEN
      z_dim = -1
      DO i_dim = 1, PDF%NDims
        IF ((PDF%index_type(i_dim) == Index_StateVar) .AND. &
            (PDF%index1(i_dim)     == Gbcs_State_SatZenith)) z_dim=i_dim
      END DO    
      array_index = PDF_Get_LUT_Index(PDF, pixel, oob)
      
      IF (z_dim > 0) THEN
        f = (1.0 / COS(pixel%SatZA*Deg2Rad) - 1) / 0.75
        IF (f < 0) f = 0
        IF (f > 1) f = 1
        array_index(z_dim) = 0
        pdf_val     = (1-f) * PDF%LUT(1 + SUM(array_index * PDF%stride))
        array_index(z_dim) = 1
        pdf_val     = pdf_val + f * PDF%LUT(1 + SUM(array_index * PDF%stride))
      ELSE
        pdf_val     = PDF%LUT(1 + SUM(array_index * PDF%stride))
      END IF
    ELSE
      pdf_val = -1
    END IF

  END FUNCTION Lookup_PDF_pw
  
END MODULE GbcsPDFLookup
