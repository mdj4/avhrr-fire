!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsPixelLoaders
!
! PURPOSE:
!       Subroutines to initialize pixel structure and load the B_Matrix, dy
!       Matrix & Cloudy Sky PDF values from LUTs
!       
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsPixelLoaders
!
! PUBLIC DATA:
!       None
!
! MODULES:
!       GbcsErrorHandler
!       GbcsTypes
!       GbcsInterpolators
!       GbcsForecastModel
!       GbcsPDFLoaders
!
! CONTAINS:
!       Initialize_Pixel
!       Allocate_Pixel
!       Release_Pixel
!       Get_Background_State
!       Get_Background_BTs
!       Get_B_Matrix
!       Get_R_Matrix
!       Get_Obs_Bkg_Difference
!       Calculate_LSD
!       Local_Std_Dev
!       Get_Clear_Txt_PDF_From_LUT
!       Get_Cloudy_PDF_From_LUT
!       Prob_VisObs_Cld_func1
!
! DERIVED TYPES:
!       None
!
! NOTES:
!
! CREATION HISTORY:
!       Written by:   Chris Old    09/08/2007
!                     IAES, University of Edinburgh
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
MODULE GbcsPixelLoaders
  ! ------------
  ! Modules used
  ! ------------
  USE GbcsErrorHandler
  USE GbcsTypes

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_ProcessImagery/GbcsPixelLoaders.f90'

CONTAINS

!------------------------------------------------------------------------------
!S+
! NAME:
!       Initialize_Pixel
!
! PURPOSE:
!       Initialize variables, flags, and outputs in the Pixel data structure.
!       Copies imagery BTs and reflectances to pixel structure 
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Initialize_Pixel(PX, IMG)
!
! INPUT ARGUMENTS:
!       IMG
!
! OUTPUT ARGUMENTS:
!       PX
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
!       Set all input variables to zero
!       Set all flags to -1
!       Set all output probabilities to -1
!
! CREATION HISTORY:
!       Written by:   Chris Old 13/10/2005
!                     IAES, University of Edinburgh
!
!       Modified to use new Prior/Post probability arrays
!       prefix        Owen Embury 15/07/2008
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Initialize_Pixel( PX, IMG )
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel), INTENT(INOUT) :: PX
    TYPE(Imagery),    INTENT(IN)    :: IMG

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Initialize_Pixel'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: elem, line
    INTEGER :: i_chan, gbcs_chan, data_chan
    
    elem = PX%ImageCoord%elem
    line = PX%ImageCoord%line

    PX%Flags = 0

    PX%Yobs  = 0.0
    PX%eYo   = 0.0
    DO i_chan = 1, IMG%N_Chans
      gbcs_chan          = IMG%Gbcs_Map(i_chan)
      data_chan          = IMG%Data_Map(i_chan)
      PX%Yobs(gbcs_chan) = IMG%Chan_Data(gbcs_chan)%d(elem, line)
!      PX%eYo (gbcs_chan) = PX%NedT(gbcs_chan)  ! UoE / ARC
!      PX%eYo (gbcs_chan) = IMG%NedT(data_chan) ! noaa old
      PX%eYo (gbcs_chan) = Get_Scaled_Error_from_NeDT(gbcs_chan,&
           IMG%NedT(data_chan),&
           PX%Yobs(gbcs_chan),300.,IMG%CWN(gbcs_chan),IMG%P_Offset(gbcs_chan),&
           IMG%P_Slope(gbcs_chan))
      PX%NeDT(gbcs_chan) = PX%eYo(gbcs_chan)
    END DO

    PX%LSD        = 0.0
    PX%nLSD       = 0
    PX%Ybkg       = 0.0
    PX%eYb        = 0.0
    PX%dY         = 0.0
    PX%SSTb       = 0.0
    PX%eSSTb      = 0.0
    PX%TCWVb      = 0.0
    PX%eTCWVb     = 0.0
    PX%Surface_Type = -1
    PX%Class      = -1
    PX%LandSea    = -1
    PX%DayNight   = -1
    PX%Prior_Prob = -1.0
    PX%PDF_spec   = -1.0
    PX%PDF_text   = -1.0
    PX%Post_Prob  = -1.0
    PX%Post_Spec  = -1.0
    PX%Post_Text  = -1.0
    PX%ST         = 0.0
    PX%SigmaST    = 0.0
    PX%Emissivity = 0.0
    PX%Albedo     = -1.0
    PX%dBTdSST    = 0.0
    PX%S_Matrix   = 0.0
  END SUBROUTINE Initialize_Pixel

!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Scaled_Error_From_NeDT
!
! PURPOSE:
!       Gets the pixel error from the NeDT which is defined at a particular
!       temperature so has to be scaled to reflect the observed temperature
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Value = Get_Scaled_Error_From_NeDT(FIndx,NedT,Yobs,NeDT_Temp,CWN,&
!                                          P_Offset,P_Slope)
!
! INPUT ARGUMENTS:
!       px    - ImagePixel structure
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   J. Mittaz 14/12/09
!                     CICS/ESSIS, University of Maryland
!
!S-
!------------------------------------------------------------------------------
REAL(GbcsReal) FUNCTION Get_Scaled_Error_From_NeDT(FIndx,NedT,Yobs,NeDT_Temp,&
     CWN,P_Offset,P_Slope)RESULT(Delta_T)
  
  USE GbcsScattPhysics

  INTEGER, INTENT(IN) :: FIndx
  REAL(GbcsReal), INTENT(IN) :: NeDT
  REAL(GbcsReal), INTENT(IN) :: Yobs
  REAL(GbcsReal), INTENT(IN) :: NeDT_Temp
  REAL(GbcsReal), INTENT(IN) :: CWN
  REAL(GbcsReal), INTENT(IN) :: P_Offset
  REAL(GbcsReal), INTENT(IN) :: P_Slope

  ! Local Variables
  REAL(GbcsReal) :: Radiance
  REAL(GbcsReal) :: Radiance1
  REAL(GbcsReal) :: Radiance2
  REAL(GbcsReal) :: Delta_Rad

  REAL(GbcsReal) :: BT1
  REAL(GbcsReal) :: BT2

  ! Only rescale error if we're in the IR
  IF( Gbcs_IR_03x .le. FIndx .AND. Yobs > 220.)THEN
     Radiance = T_to_Rad(NeDT_Temp,CWN,P_Offset,P_Slope)
     Radiance1 = T_to_Rad(NeDT_Temp+NeDT,CWN,P_Offset,P_Slope)
     Radiance2 = T_to_Rad(NeDT_Temp-NeDT,CWN,P_Offset,P_Slope)

     ! Average Delta R
     Delta_Rad = (ABS(Radiance1-Radiance)+ABS(Radiance2-Radiance))/2.
     
     Radiance = T_to_Rad(Yobs,CWN,P_Offset,P_Slope)
     BT1 = Rad_to_T(Radiance+Delta_Rad,CWN,P_Offset,P_Slope)
     BT2 = Rad_to_T(Radiance-Delta_Rad,CWN,P_Offset,P_Slope)
     
     Delta_T = (ABS(Yobs-BT1)+ABS(Yobs-BT2))/2.
  ELSE
     Delta_T = NeDT
  ENDIF
  RETURN

END FUNCTION Get_Scaled_Error_From_NeDT

!------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_Pixel
!
! PURPOSE:
!       Allocate the pixel matrices used for Bayesian calculations
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Allocate_Pixel(PX)
!
! INPUT ARGUMENTS:
!       px    - ImagePixel structure
!
! OPTIONAL INPUT ARGUMENTS:
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury     24/04/08
!                     IAES, University of Edinburgh
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Allocate_Pixel(PX)
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel),      INTENT (INOUT)    :: PX

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Allocate_Pixel'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: STAT


    ALLOCATE( PX%H_Matrix(PX%State_Vector_Len, PX%N_Chans),           &
              PX%B_Matrix(PX%State_Vector_Len, PX%State_Vector_Len),  &
              PX%R_Matrix(PX%N_Chans,          PX%N_Chans),           &
              PX%S_Matrix(PX%N_Chans,          PX%N_Chans),           &
              STAT=STAT )

    CALL Gbcs_Critical(STAT /= 0, 'Unable to allocate space for pixel H/B/R/S matricies', ROUTINE_NAME, MODULE_NAME)

    ALLOCATE( PX%Prior_Prob(PX%n_classes),   &
              PX%PDF_spec  (PX%n_classes),   &
              PX%PDF_text  (PX%n_classes),   &              
              PX%Post_Prob (PX%n_classes),   &
              PX%Post_Spec (PX%n_classes),   &
              PX%Post_Text (PX%n_classes),   &
              STAT=STAT )

    CALL Gbcs_Critical(STAT /= 0, 'Unable to allocate space for pixel probabilities', ROUTINE_NAME, MODULE_NAME)

  END SUBROUTINE Allocate_Pixel


!------------------------------------------------------------------------------
!F+
! NAME:
!       Release_Pixel
!
! PURPOSE:
!       Relases the pixel allocated structures
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       status = Release_Pixel( PX )
!
! INPUT ARGUMENTS:
!       PX
!
! OUTPUT ARGUMENTS:
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! FUNCTION RESULT:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!       Tests each allocatable variable to see if it has been allocated
!       If a variable is allocated it is dealloacted
!
! CREATION HISTORY:
!       Written by:   Chris Old 09/03/2005
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Release_Pixel( PX ) &
                  RESULT( status )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel), INTENT (INOUT) :: PX

    ! ---------
    ! Function result
    ! ---------
    INTEGER :: status

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Release_Pixel'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER, DIMENSION(10) :: STAT

    STAT   = 0
    status = 0

    IF (ALLOCATED(PX%B_Matrix))   DEALLOCATE(PX%B_Matrix,  STAT=STAT(1))
    IF (ALLOCATED(PX%H_Matrix))   DEALLOCATE(PX%H_Matrix,  STAT=STAT(2))
    IF (ALLOCATED(PX%R_Matrix))   DEALLOCATE(PX%R_Matrix,  STAT=STAT(3))
    IF (ALLOCATED(PX%S_Matrix))   DEALLOCATE(PX%S_Matrix,  STAT=STAT(4))
    IF (ALLOCATED(PX%Prior_Prob)) DEALLOCATE(PX%Prior_Prob,STAT=STAT(5))
    IF (ALLOCATED(PX%PDF_spec))   DEALLOCATE(PX%PDF_spec,  STAT=STAT(6))
    IF (ALLOCATED(PX%PDF_text))   DEALLOCATE(PX%PDF_text,  STAT=STAT(7))
    IF (ALLOCATED(PX%Post_Prob))  DEALLOCATE(PX%Post_Prob, STAT=STAT(8))
    IF (ALLOCATED(PX%Post_Spec))  DEALLOCATE(PX%Post_Spec, STAT=STAT(9))
    IF (ALLOCATED(PX%Post_Text))  DEALLOCATE(PX%Post_Text, STAT=STAT(10))
    
    IF (ANY( STAT /= 0)) status = -1

  END FUNCTION Release_Pixel


!------------------------------------------------------------------------------
!F+
! NAME:
!       Get_Background_State
!
! PURPOSE:
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       status = Get_Background_State( Model, PX )
!
! INPUT ARGUMENTS:
!       PX
!       Model
!
! OUTPUT ARGUMENTS:
!       PX
!
! FUNCTION RESULT:
!       0 on sucess
!       -1 if SST/TCWV/Wind data missing (does not check elev data)
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 16/07/2007
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Get_Background_State( Model, PX ) &
                         RESULT( status )
    USE GbcsInterpolators
    USE GbcsForecastModel
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Forecast_Model), INTENT(IN)    :: Model
    TYPE(ImagePixel),     INTENT(INOUT) :: PX
    
    ! ---------
    ! Function result
    ! ---------
    INTEGER :: status
    
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Get_Background_State'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: index

    status = 0

    index    = Get_SurfTemp_Index(Model, PX%Surface_Type)
    IF (index /= -1 ) THEN
      PX%SSTb  = InterpolateField(Model%Srf(index)%d, PX%LocMP)
      PX%eSSTb = Model%eSST
    ELSE
      status = -1
    END IF

    IF (ALLOCATED(Model%Srf(Gbcs_Map_TCWV)%d)) THEN
      PX%TCWVb  = InterpolateField(Model%Srf(Gbcs_Map_TCWV)%d, PX%LocMP)
      PX%eTCWVb = Model%eTCWV
    ELSE
      status = -1
    END IF

    IF (ALLOCATED(Model%Srf(Gbcs_Map_T_2M)%d)) THEN
      PX%two_m_temp  = InterpolateField(Model%Srf(Gbcs_Map_T_2M)%d, PX%LocMP)
    ELSE
      status = -1
    END IF

    IF (ALLOCATED(Model%Srf(Gbcs_Map_Wind_10M)%d)) THEN
      PX%Wind_10m = InterpolateField(Model%Srf(Gbcs_Map_Wind_10M)%d, PX%LocMP)
      IF (PX%Wind_10m < 0) THEN
        PX%WInd_10m = 0.
      END IF
      PX%eU10b    = Model%eU10
    ELSE
      PX%Wind_10m = 8.63
      PX%eU10b    = 4.186
    END IF

    IF (PX%Surface_Type == Gbcs_Surface_Land .AND. &
        ALLOCATED(Model%Srf(Gbcs_Map_OROG)%d)) THEN
      PX%Elevb = InterpolateField(Model%Srf(Gbcs_Map_OROG)%d, PX%LocMP)
    ELSE
      PX%Elevb = 0.0
    END IF

    IF (ALLOCATED(Model%Srf(Gbcs_Map_Sea_Ice)%d)) THEN
      PX%IceB = InterpolateField(Model%Srf(Gbcs_Map_Sea_Ice)%d, PX%LocMP)
    ELSE
      PX%IceB = 0.0
    END IF

  END FUNCTION Get_Background_State

!------------------------------------------------------------------------------
!F+
! NAME:
!       Get_Background_BTs
!
! PURPOSE:
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       status = Get_Background_BTs( RTM, Model, PX )
!
! INPUT ARGUMENTS:
!       PX
!       Model
!
! OUTPUT ARGUMENTS:
!       PX
!
! FUNCTION RESULT:
!       0 on sucess
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 16/07/2007
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Get_Background_BTs( RTM, Model, PX, use_hrsst, use_emiss ) &
                       RESULT( status )
    USE GbcsInterpolators
    USE GbcsForecastModel
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(RTM_Interface),  INTENT(IN)    :: RTM
    TYPE(Forecast_Model), INTENT(IN)    :: Model
    TYPE(ImagePixel),     INTENT(INOUT) :: PX
    LOGICAL, OPTIONAL,    INTENT(IN)    :: use_hrsst
    LOGICAL, OPTIONAL,    INTENT(IN)    :: use_emiss
    
    ! ---------
    ! Function result
    ! ---------
    INTEGER :: status
    
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Get_Background_BTs'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i_chan, gbcs_chan, index, ii
    REAL(KIND=GbcsReal) :: Val(4)
    LOGICAL           :: adjust_hrsst
    LOGICAL           :: adjust_emiss
    LOGICAL           :: Edinburgh_Vis_Channel

    status = 0

    IF (PRESENT(use_hrsst)) THEN
      adjust_hrsst = use_hrsst
    ELSE
      adjust_hrsst = .FALSE.
    END IF
    IF (PRESENT(use_emiss)) THEN
      adjust_emiss = use_emiss
    ELSE
      adjust_emiss = .FALSE.
    END IF

    ! If using edinburgh vis channel
    Edinburgh_Vis_Channel = .FALSE.
    DO ii=1,RTM%N_RTMs
       IF( RTM%ID_Map(ii) .eq. Gbcs_VISRTM_ID )THEN
          Edinburgh_Vis_Channel = .TRUE.
       ENDIF
    END DO

    DO i_chan = 1,RTM%N_Chans
      gbcs_chan = RTM%Gbcs_Map(i_chan)

      SELECT CASE ( gbcs_chan )
      CASE ( Gbcs_VIS_BBD, Gbcs_VIS_005, Gbcs_VIS_006, Gbcs_VIS_008, Gbcs_NIR_01x)
        ! If Edinburgh visible channel routine then run per pixel
        IF( Edinburgh_Vis_Channel )THEN
           ! -- Visible channels (RTM run per-pixel)
           PX%Ybkg(gbcs_chan) = RTM%BTR(gbcs_chan)%d(PX%LocMP(1)%elem,PX%LocMP(1)%line)
           PX%Ybkg_uncorr(gbcs_chan) = PX%Ybkg(gbcs_chan)
           PX%eYb(gbcs_chan)  = RTM%eBTR(gbcs_chan)
        ELSE
           ! -- RTM run per-profile
           Val(1) = RTM%BTR(gbcs_chan)%d(PX%LocMP(1)%elem,PX%LocMP(1)%line)
           Val(2) = RTM%BTR(gbcs_chan)%d(PX%LocMP(2)%elem,PX%LocMP(2)%line)
           Val(3) = RTM%BTR(gbcs_chan)%d(PX%LocMP(3)%elem,PX%LocMP(3)%line)
           Val(4) = RTM%BTR(gbcs_chan)%d(PX%LocMP(4)%elem,PX%LocMP(4)%line)            
           PX%Ybkg(gbcs_chan) = Val(1) * PX%LocMP(1)%weight + &
                Val(2) * PX%LocMP(2)%weight + &
                Val(3) * PX%LocMP(3)%weight + &
                Val(4) * PX%LocMP(4)%weight
           PX%Ybkg_uncorr(gbcs_chan) = PX%Ybkg(gbcs_chan)
           PX%eYb(gbcs_chan)  = RTM%eBTR(gbcs_chan)
        ENDIF

      CASE DEFAULT
        ! -- RTM run per-profile
        Val(1) = RTM%BTR(gbcs_chan)%d(PX%LocMP(1)%elem,PX%LocMP(1)%line)
        Val(2) = RTM%BTR(gbcs_chan)%d(PX%LocMP(2)%elem,PX%LocMP(2)%line)
        Val(3) = RTM%BTR(gbcs_chan)%d(PX%LocMP(3)%elem,PX%LocMP(3)%line)
        Val(4) = RTM%BTR(gbcs_chan)%d(PX%LocMP(4)%elem,PX%LocMP(4)%line)            
        IF (adjust_hrsst) THEN
          index    = Get_SurfTemp_Index(Model, PX%Surface_Type)
          DO ii=1, 4
            Val(ii) = Val(ii) + &
                 RTM%H_Matrix(1,i_chan,PX%LocMP(ii)%elem,PX%LocMP(ii)%line) * &
                 ( PX%SSTb - Model%Srf(index)%d(PX%LocMP(ii)%elem,PX%LocMP(ii)%line) )
          END DO
        END IF

        IF (adjust_emiss) THEN
          DO ii=1, 4
            Val(ii) = Val(ii) + RTM%dEmiss(gbcs_chan)%d(PX%LocMP(ii)%elem,PX%LocMP(ii)%line) * &
                              ( PX%Emissivity(gbcs_chan) - RTM%Emissivity(gbcs_chan)%d(PX%LocMP(ii)%elem,PX%LocMP(ii)%line) )
          END DO
        END IF
        
        PX%Ybkg(gbcs_chan) = Val(1) * PX%LocMP(1)%weight + &
                             Val(2) * PX%LocMP(2)%weight + &
                             Val(3) * PX%LocMP(3)%weight + &
                             Val(4) * PX%LocMP(4)%weight
        PX%Ybkg_uncorr(gbcs_chan) = PX%Ybkg(gbcs_chan)

        IF (PX%profiles_do_not_match) THEN
          PX%eYb(gbcs_chan)  = 2.0
        ELSE
          PX%eYb(gbcs_chan)  = RTM%eBTR(gbcs_chan)
        END IF

        ! Store optical depths
        Val(1) = RTM%Opt_Depth(gbcs_chan)%d(PX%LocMP(1)%elem,PX%LocMP(1)%line)
        Val(2) = RTM%Opt_Depth(gbcs_chan)%d(PX%LocMP(2)%elem,PX%LocMP(2)%line)
        Val(3) = RTM%Opt_Depth(gbcs_chan)%d(PX%LocMP(3)%elem,PX%LocMP(3)%line)
        Val(4) = RTM%Opt_Depth(gbcs_chan)%d(PX%LocMP(4)%elem,PX%LocMP(4)%line)

        PX%Opt_Depth(gbcs_chan) = Val(1) * PX%LocMP(1)%weight + &
                             Val(2) * PX%LocMP(2)%weight + &
                             Val(3) * PX%LocMP(3)%weight + &
                             Val(4) * PX%LocMP(4)%weight
      END SELECT
    END DO

  END FUNCTION Get_Background_BTs

!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_B_Matrix
!
! PURPOSE:
!       Loads covariance ( B ) matrix values into pixel data structure
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Get_B_Matrix( AUX, PX )
!
! INPUT ARGUMENTS:
!       AUX
!       PX
!
! OUTPUT ARGUMENTS:
!       PX
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!       The matrix is stored in the AUXILIARY DATA structure.
!       The data are only reloaded if there is a change in the B matrix
!       to be used. This occurs if there are different matrices for a
!       series of latitude bands, etc
!
! CREATION HISTORY:
!       Written by:   Chris Old 25/05/2005
!                     IAES, University of Edinburgh
!
! 0.0   25/05/2005    Creation                                            CPO
! 0.1   21/09/2005    Generalised to allow for simple covariance matrix   CPO
! 0.2   06/08/2007    Generalised to allow for any covariance matrix      CPO
! 0.3   14/12/2009    Added in multipicative covariance (TCWV)           JPDM
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Get_B_Matrix( AUX , PX )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Aux_Data),   INTENT (IN)    :: AUX
    TYPE(ImagePixel), INTENT (INOUT) :: PX

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Get_B_Matrix'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: Band
    INTEGER :: FIndx
    INTEGER :: I

    Band = 1

    DO WHILE ( ( AUX%CovMat%Limits(Band,1) <= PX%EarthCoord%Lat ) .AND. &
                ( AUX%CovMat%Limits(Band,2) <  PX%EarthCoord%Lat ) .AND. &
                ( Band < AUX%CovMat%NBands ) )

      Band = Band + 1    

    END DO

!    IF ( Band /= PX%B_Matrix_Band ) THEN

       PX%B_Matrix_Band = Band

       PX%B_Matrix = AUX%CovMat%Data(:,:,Band)

       ! Look for multiplicative covariances
       DO I=1,AUX%CovMat%NFields

          IF( 2 .eq. AUX%CovMat%Type(I) )THEN
             FIndx = AUX%CovMat%Field_ID(I)
             SELECT CASE ( FIndx )
             CASE ( Gbcs_Map_TCWV )
                PX%B_Matrix(I,I) = PX%B_Matrix(I,I) * PX%TCWVb * PX%TCWVb
             CASE DEFAULT
                CALL Gbcs_Critical(.TRUE.,&
                     'Multiplicative Covariance is only supported for TCWV',&
                     'Get_B_Matrix','GbcsPixelLoaders.f90')
             END SELECT
          ENDIF
       END DO
!    END IF

  END SUBROUTINE Get_B_Matrix


!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_R_Matrix
!
! PURPOSE:
!       Loads model/observation covariance ( R ) matrix values into pixel data
!       structure
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Get_R_Matrix( PX )
!
! INPUT ARGUMENTS:
!       PX
!
! OUTPUT ARGUMENTS:
!       PX
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!       Model/Obs covariance matrix is a diagonal matrix where each diagonal
!       element is defined as:
!
!           eBT(Obs)^2 + eBT(Model)^2
!
! CREATION HISTORY:
!       Written by:   Chris Old 30/10/2006
!                     IAES, University of Edinburgh
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Get_R_Matrix( PX )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel), INTENT (INOUT) :: PX

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Get_R_Matrix'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: ii, Chan
    REAL(KIND=GbcsReal) :: SecTheta , eYb
      

    PX%R_Matrix(:,:) = 0.0

    DO ii = 1,PX%N_Chans

      Chan = PX%Chan_ID( ii )

      ! Scale RTM error by view angle
      SecTheta = 1.0 / COS( PX%SatZA * Deg2Rad )
      eYb = PX%eYb(Chan) * SecTheta
      IF ( PX%Surface_Type == Gbcs_Surface_Sea ) THEN
        PX%R_Matrix(ii,ii) = ( PX%eYo(Chan) * PX%eYo(Chan) ) + ( eYb * eYb )
      ELSE
        PX%R_Matrix(ii,ii) = ( PX%eYo(Chan) * PX%eYo(Chan) ) + ( eYb * eYb ) * 4.0
      END IF

    END DO

  END SUBROUTINE Get_R_Matrix


!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Obs_Bkg_Difference
!
! PURPOSE:
!       Calculate the difference vector dY(i) = BTo(i) - BTb(i)
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Get_Obs_Bkg_Difference( PX )
!
! INPUT ARGUMENTS:
!       PX
!
! OUTPUT ARGUMENTS:
!       PX
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!       Loop through the channels used in the Bayesian calculation and
!       calculate the difference between the observed and prior brightness
!       temperatures
!
! CREATION HISTORY:
!       Written by:   Chris Old 25/05/2005
!                     IAES, University of Edinburgh
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Get_Obs_Bkg_Difference( PX )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel), INTENT (INOUT) :: PX

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Get_Obs_Bkg_Difference'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: ii,Chan

    DO ii = 1,PX%N_Chans
      Chan = PX%Chan_ID( ii )

      PX%dY(ii) = PX%Yobs(Chan) - PX%Ybkg(Chan)    ! Observation - Background

    END DO

  END SUBROUTINE Get_Obs_Bkg_Difference



!------------------------------------------------------------------------------
!F+
! NAME:
!       Calculate_LSD
!
! PURPOSE:
!       Calculate the local standard devation at the specified position using a
!       3x3 grid of pixels
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       lsd = Calculate_LSD(IMG, chan, elem, line, numb)
!
! INPUT ARGUMENTS:
!       IMG
!       chan
!       elem
!       line
!
! OUTPUT ARGUMENTS:
!       numb  - number of pixels used in calculation
!
! FUNCTION RESULT:
!       Standard deviaion of the 3x3 pixels at the specified location
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
!       Written by:   Owen Embury 23/07/2008
!                     IAES, University of Edinburgh
!
!F-
!------------------------------------------------------------------------------
  FUNCTION Calculate_LSD( IMG, chan, elem, line, numb ) &
                  RESULT( lsd )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Imagery),       INTENT(IN) :: IMG
    INTEGER,             INTENT(IN) :: chan
    INTEGER,             INTENT(IN) :: elem
    INTEGER,             INTENT(IN) :: line
    INTEGER,             INTENT(OUT):: numb

    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsReal)  :: lsd

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Calculate_LSD'
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER             :: E, L
    REAL(KIND=GbcsDble) :: x, x2, n, val, offset
    
    x   = 0.0
    x2  = 0.0
    n   = 0.0
    lsd = -1.0

    ! Only have offsets for IR BT channels
    IF( 240. .lt. IMG%Chan_Data(chan)%d(elem,line) )THEN
       offset = 240.
    ELSE
       offset = 0.
    ENDIF
    
    ! Note this assumes we are sampling a full population rather than a sample
    ! of an underlying distribution
    ! Subtract 240K from BTs to avoid rounding errors with this fast way 
    ! of calculating LSD
    DO L = MAX(line-1, 1), MIN(line+1, IMG%Npts%line)
      DO E = MAX(elem-1, 1), MIN(elem+1, IMG%Npts%elem)
        val = IMG%Chan_Data(chan)%d(E,L) - offset
        IF ( (IMG%LandSeaMask(E,L) == IMG%LandSeaMask(elem,line)) .AND. &
             ( val > 0 )) THEN
          x  = x  + val
          x2 = x2 + val * val
          n  = n  + 1.0
        END IF
      END DO
    END DO

    IF (n > 0) THEN
      x  = x  / n
      x2 = x2 / n
      x2 = x2 - x**2
      IF (x2 >= 0) lsd = DSQRT(x2)
    END IF
    numb = n
    
  END FUNCTION Calculate_LSD   


!------------------------------------------------------------------------------
!S+
! NAME:
!       Local_Std_Dev
!
! PURPOSE:
!       Calculate the local standard devation at the current pixel based on a
!       3x3 grid of pixels
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Local_Std_Dev( PX, IMG )
!
! INPUT ARGUMENTS:
!       PX
!       IMG
!
! OUTPUT ARGUMENTS:
!       PX
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!       Calculate_LSD
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!       The local standard deviation is defined as:
!
!         ____________________________________
!         |         n                        |
!         |        ---
!         |   1    \           2     ____
!         |  ---   /    ( BT  )  -  ( BT )
!         |   n    ---      i
!        \|        i=1
!
!
!       Near the coast only the BTs from surrounding pixels with the same
!       surface type as the central pixel are used.
!       In the case where there is only the central pixel the LSD is set
!       to the NedT for the pixel BT
!
! CREATION HISTORY:
!       Written by:   Chris Old 04/07/2005
!                     IAES, University of Edinburgh
!       Modified to use pre-calcualted LSD if available, otherwise calls
!       Calculate_LSD            Owen Embury 23/07/2008
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Local_Std_Dev( PX , IMG )
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel),     INTENT (INOUT) :: PX
    TYPE(Imagery),        INTENT (IN)    :: IMG

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER  :: ROUTINE_NAME = 'Local_Std_Dev'

    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i_chan, i_gbcs, elem, line
    INTEGER :: n
    REAL(KIND=GbcsReal) :: lsd


    elem = PX%ImageCoord%elem
    line = PX%ImageCoord%line

    DO i_chan = 1, PX%N_Bayes_Chans(2) ! index 2 = number of texture channels
      i_gbcs = PX%Chan_ID( PX%Bayes_Text_Chans(i_chan) )

      IF ( ALLOCATED(IMG%Chan_LSD(i_gbcs)%d) ) THEN
        PX%LSD(i_gbcs)  = IMG%Chan_LSD(i_gbcs)%d(elem,line)
        PX%nLSD(i_gbcs) = 9
        PX%eLSD(i_gbcs) = SQRT( 2.0 / ( 9 - 1 ) )

      ELSE
        lsd = Calculate_LSD(IMG, i_gbcs, elem, line, n)
        PX%nLSD(i_gbcs) = INT(n)
      
        IF ( n > 1 ) THEN
          PX%LSD (i_gbcs) = lsd
          PX%eLSD(i_gbcs) = SQRT( 2.0 / (n-1) )
        ELSE
          PX%LSD (i_gbcs) = PX%NedT( i_gbcs )
          PX%eLSD(i_gbcs) = PX%eYo( i_gbcs )
        END IF

      END IF

    END DO

  END SUBROUTINE Local_Std_Dev


!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Clear_Txt_PDF_From_LUT
!
! PURPOSE:
!       
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Get_Clear_Txt_PDF_From_LUT( AUX, PX )
!
! INPUT ARGUMENTS:
!       PX
!       IMG
!
! OUTPUT ARGUMENTS:
!       PX
!
! OPTIONAL OUTPUT ARGUMENTS:
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! PROCEDURE:
!
! CREATION HISTORY:
!       Written by:   Chris Old 30/07/2005
!                     IAES, University of Edinburgh
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Get_Clear_Txt_PDF_From_LUT( AUX , PX )
    USE GbcsPDFLoaders, ONLY: Look_Up_Clear_Txt_PDF
    IMPLICIT NONE

    ! ---------
    ! Arguments
    ! ---------
    TYPE(Aux_Data),    INTENT (IN   ) :: AUX
    TYPE(ImagePixel),  INTENT (INOUT) :: PX

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_Clear_Txt_PDF_From_LUT'

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble) :: PDF_text

    IF (PX%N_Bayes_Chans(2) > 0) THEN
      IF ( PX%DayNight == Gbcs_Daytime ) THEN
        PDF_text = Look_Up_Clear_Txt_PDF( AUX%PDF_TD_CLR , PX )

      ELSE IF ( PX%DayNight == Gbcs_Twilight ) THEN
        PDF_text = Look_Up_Clear_Txt_PDF( AUX%PDF_TT_CLR , PX )

      ELSE
        PDF_text = Look_Up_Clear_Txt_PDF( AUX%PDF_TN_CLR , PX )
      END IF

      IF ( PDF_text < 1.0e-15 ) PDF_text = 1.0e-15

      PX%PDF_text(1) = PDF_text
    ELSE
      PX%PDF_text(1) = 0.0_GbcsDble
    END IF

  END SUBROUTINE Get_Clear_Txt_PDF_From_LUT


  SUBROUTINE Get_Cloudy_PDF_From_LUT( AUX, PX )

  !
  ! Description:
  !
  !  Retrieve Cloudy sky spectral and texture probability based on BTs and LSD of BTs respectively
  !
  ! Method:
  !
  !  Determine from the PIXEL flags whether it is day time or night time
  !  Retrieve the spectral and textural PDF values by passing the appropriate PDF structure to 
  !
  !    FUNCTION Look_Up_PDF( PDF , PX ) in this file
  !
  !  The results are stored in the PIXEL structure
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

    USE GbcsPDFLoaders, ONLY: Look_Up_PDF
    IMPLICIT NONE

    TYPE(Aux_Data),   INTENT (IN   ) :: AUX
    TYPE(ImagePixel), INTENT (INOUT) :: PX

    INTEGER :: ii
    REAL(KIND=GbcsReal) :: PDF_VIS
    INTEGER :: N_VIS_Chans

    INTEGER, DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Indx
    INTEGER, DIMENSION(Gbcs_Max_PDF_LUT_Dims) :: Chan_ID
    INTEGER :: Chan
    INTEGER :: Offset
    REAL(KIND=GbcsReal) :: Yobs
    REAL(KIND=GbcsDble) :: PDF_spec, PDF_text

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Get_Cloudy_PDF_From_LUT'



  ! Get Spectral PDF Value From LUT
  !---------------------------------------------------

      IF ( PX%DayNight == Gbcs_Daytime ) THEN

        PDF_spec = Look_Up_PDF( AUX%PDF_SD , PX )

        IF ( PX%Surface_Type /= Gbcs_Surface_Land ) THEN

          N_VIS_Chans = 0

          DO ii = 1,AUX%PDF_SD%NChans
            IF ( AUX%PDF_SD%Chan_ID(ii) <= Gbcs_NIR_01x ) THEN
              N_VIS_Chans = N_VIS_Chans + 1
              Chan_ID( N_VIS_Chans ) = AUX%PDF_SD%Chan_ID(ii)
            END IF
          END DO

          DO ii = 1,AUX%PDF_SD%NChans

            Chan = PX%Chan_ID(PX%Bayes_Spec_Chans(ii))
            IF ( AUX%PDF_SD%Chan_ID(ii) <= Gbcs_NIR_01x ) THEN

              SELECT CASE ( N_VIS_Chans )

                CASE ( 1 )        ! One VIS Channel
                  PDF_VIS   = Prob_VisObs_Cld_func1(PX%Yobs(Chan), Chan)

                  PX%CldTIR = PDF_spec
                  PX%CldVIS = PDF_VIS

                  PDF_spec  = PDF_spec * PDF_VIS

                  IF ( PDF_spec < 1.0e-10 ) PDF_spec = 1.0e-10

                CASE ( 2 )        ! Two VIS Channels

                  IF ( ALLOCATED( AUX%VIS_CLD_PDF ) ) THEN

                    DO Chan = 1,N_VIS_Chans

                      Yobs = MAX( PX%Yobs( Chan_ID(Chan) ) , 0.0 )
                      Indx(Chan) = CEILING( Yobs / 0.004 )

                      IF (Indx(Chan) < 1) Indx(Chan) = 1
                      IF (Indx(Chan) > 250) Indx(Chan) = 250

                    END DO

                    Offset = ( Indx(2) - 1 ) * 250 + Indx(1)

                    PX%CldTIR = PDF_spec
                    PX%CldVIS = AUX%VIS_CLD_PDF(Offset)

                    PDF_spec = PDF_spec * PX%CldVIS

                  ELSE

                    PX%CldTIR = PDF_spec
                    PX%CldVIS = -999.9

                  END IF

                  IF ( PDF_spec < 1.0e-10 ) PDF_spec = 1.0e-10

              END SELECT

            END IF

          END DO

        ELSE
        
          PX%CldTIR = PDF_spec
          PX%CldVIS = -999.9

          IF ( PDF_spec < 1.0e-10 ) PDF_spec = 1.0e-10

        END IF

      ELSE IF ( PX%DayNight == Gbcs_Nighttime ) THEN

        PDF_spec = Look_Up_PDF( AUX%PDF_SN , PX )
        PX%CldTIR = PDF_spec
        PX%CldVIS = -999.9

        IF ( PDF_spec < 1.0e-10 ) PDF_spec = 1.0e-10

      ELSE

        PDF_spec = Look_Up_PDF( AUX%PDF_ST , PX )
        PX%CldTIR = PDF_spec
        PX%CldVIS = -999.9

        IF ( PDF_spec < 1.0e-10 ) PDF_spec = 1.0e-10

      END IF


  ! Get Textural PDF Value From LUT
  !---------------------------------------------------

      IF ( PX%DayNight == Gbcs_Daytime ) THEN

        PDF_text = Look_Up_PDF( AUX%PDF_TD , PX )

        IF ( PDF_text < 1.0e-10 ) PDF_text = 1.0e-10

      ELSE IF ( PX%DayNight == Gbcs_Nighttime ) THEN

        PDF_text = Look_Up_PDF( AUX%PDF_TN , PX )

        IF ( PDF_text < 1.0e-10 ) PDF_text = 1.0e-10

      ELSE

        PDF_text = Look_Up_PDF( AUX%PDF_TT , PX )

        IF ( PDF_text < 1.0e-10 ) PDF_text = 1.0e-10

      END IF

    PX%PDF_spec(2) = PDF_spec
    PX%PDF_text(2) = PDF_text

    RETURN


  END SUBROUTINE Get_Cloudy_PDF_From_LUT



!------------------------------------------------------------------------------
!F+
! NAME:
!       Prob_VisObs_Cld_func1
!
! PURPOSE:
!       Calculate P(y_vis | x, cld) using a functional fit to some empirical
!       data.  This function is retained for usewhere no PDF LUT is available
!       and only one visible channel is being used.
!
!       Function assumes input reflectance is in range 0 - 1
!       Source of empirical data is either AATSR or MSG using operational mask
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       prob = Prob_VisObs_Cld_func1( refl, ChanID )
!
! INPUT ARGUMENTS:
!       refl   - Vis/NIR reflectance (0-1)
!       ChanId - Gbcs channel id
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
!       probability of observations assuming cloudy-sky
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
!       Written by:   Owen Embury 15/03/2008
!                     IAES, University of Edinburgh
!       Code originally part of Get_Cloudy_PDF_From_LUT written by CPO
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Prob_VisObs_Cld_func1( refl, ChanID) &
                                    RESULT( prob )
    USE GbcsTypes
    USE GbcsKinds

    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsReal), INTENT(IN) :: refl
    INTEGER,             INTENT(IN) :: ChanID

    ! ------
    ! Result
    ! ------
    REAL(KIND=GbcsReal)             :: prob

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),      PARAMETER  :: ROUTINE_NAME = 'Prob_VisObs_Cld_func1'
    REAL(KIND=GbcsDble), PARAMETER  :: sq2pi = SQRT(2.0d0 * D_Pi)


    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble)             :: G1, G2, G3, G4, G5
    
    IF( refl < 0 ) THEN
      prob = 100.0  ! if bad data force pixel to be cloud
      RETURN
    END IF
    SELECT CASE ( ChanID )
      CASE ( Gbcs_VIS_006 , GBCS_VIS_008 )
!        G1 = (1.0/(0.155*sq2pi))*EXP(-0.5*((SQRT(refl)-0.42)/0.15)**2.0)
!        G2 = (1.0/(0.75*sq2pi))*EXP(-0.5*((SQRT(refl)-0.75)/0.06)**2.0)
!        G3 = (1.0/(0.6*sq2pi))*EXP(-0.5*((SQRT(refl)-0.6)/0.04)**2.0)
!        prob = G1 + G2 + G3

        G1 = (1.0/(0.147*sq2pi))*EXP(-0.5*((LOG(refl)+2.30)/0.6  )**2.0)
        G2 = (1.0/(0.24*sq2pi)) *EXP(-0.5*((LOG(refl)+1.30)/0.3  )**2.0)
        G3 = (1.0/(0.72*sq2pi)) *EXP(-0.5*((LOG(refl)+0.63)/0.15 )**2.0)
        G4 = (1.0/(sq2pi))      *EXP(-0.5*((LOG(refl)+1.77)/0.2  )**2.0)
        G5 = (1.0/(sq2pi))      *EXP(-0.5*((LOG(refl)+0.35)/0.055)**2.0)
        prob = G1 + G2 + G3 + G4 + G5

      CASE ( Gbcs_NIR_01x )

        G1 = (1.0/(0.1*sq2pi))  *EXP(-0.5*((SQRT(refl)-0.42)/0.08)**2.0)
        G2 = (1.0/(0.3*sq2pi))  *EXP(-0.5*((SQRT(refl)-0.62)/0.06)**2.0)
        G3 = (1.0/(0.2*sq2pi))  *EXP(-0.5*((SQRT(refl)-0.21)/0.05)**2.0)

        prob = G1 + G2 + G3

      CASE DEFAULT
        prob = 5.335*EXP(-1.0*((refl**(-0.003)-1.0075)**2.0)/0.000006) + &
                     EXP(-1.0*((refl-0.35)**2.0)/0.02)

    END SELECT

  END FUNCTION Prob_VisObs_Cld_func1


!------------------------------------------------------------------------------
!F+
! NAME:
!       Bayes_Channels_Available
!
! PURPOSE:
!       Check that all the channels required for the Bayesian calculation are
!       available (Yobs >= 0)
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Bayes_Channels_Available( PX )
!
! INPUT ARGUMENTS:
!       PX
!
! FUNCTION RESULT:
!       TRUE / FALSE
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
!       Written by:   Owen Embury 10/12/2008
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  FUNCTION Bayes_Channels_Available( PX )
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    TYPE(ImagePixel),  INTENT(IN) :: PX
    
    ! ---------
    ! Function result
    ! ---------
    LOGICAL                       :: Bayes_Channels_Available
    
    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Bayes_Channels_Available'
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER             :: i_chan
    
    
    Bayes_Channels_Available = .FALSE.
    DO i_chan = 1, PX%N_Bayes_Chans(1)
      IF (PX%Yobs(PX%Chan_ID(PX%Bayes_Spec_Chans(i_chan))) < 0.0) RETURN
    END DO
  
    Bayes_Channels_Available = .TRUE.

  END FUNCTION Bayes_Channels_Available


END MODULE GbcsPixelLoaders
