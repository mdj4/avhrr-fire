MODULE Extract_L1B_Data_module
  
  USE GbcsKinds
  USE GbcsConstants
  USE GbcsTypes
  USE GbcsErrorHandler
  USE GbcsDateTime
  USE NOAA_LoadAVHRRLevel1B
  USE NETCDF

  IMPLICIT NONE

  INTERFACE isNaN
     MODULE PROCEDURE isNaN_Real
     MODULE PROCEDURE isNaN_Dble
  END INTERFACE

  PRIVATE
  PUBLIC :: read_all_data 
  PUBLIC :: output_data 

CONTAINS

  LOGICAL FUNCTION Check_Scanline(AVHRR,POS)

    TYPE(AVHRR_Data), INTENT(IN) :: AVHRR
    INTEGER, INTENT(IN) :: POS

    ! Local variables
    
    !
    ! For calibration information doesn't matter if navigation is off
    !
    IF( AVHRR%badTime(POS) .or. AVHRR%badCalibration(POS) )THEN
       Check_Scanline=.FALSE.
       RETURN
    ENDIF

    IF( (0. .gt. AVHRR%prt1(POS)) .or. (0. .gt. AVHRR%prt2(POS)) .or. &
         (0. .gt. AVHRR%prt3(POS)) .or. (0. .gt. AVHRR%prt4(POS)) )THEN
       Check_Scanline=.FALSE.
       RETURN
    ENDIF

    IF( 0. .gt. AVHRR%bb4(POS) )THEN
       Check_Scanline=.FALSE.
       RETURN
    ENDIF
    IF( ALLOCATED(AVHRR%bb5) )THEN
       IF( 0. .gt. AVHRR%bb5(POS) )THEN
          Check_Scanline=.FALSE.
          RETURN
       ENDIF
    ENDIF

    IF( 0. .gt. AVHRR%sp4(POS) )THEN
       Check_Scanline=.FALSE.
       RETURN
    ENDIF
    IF( ALLOCATED(AVHRR%sp5) )THEN
       IF( 0. .gt. AVHRR%sp5(POS) )THEN
          Check_Scanline=.FALSE.
          RETURN
       ENDIF
    ENDIF

    Check_Scanline=.TRUE.

  END FUNCTION Check_Scanline
  
  SUBROUTINE Copy_Scanline(AVHRR,POS1,AVHRR_New,POS2)

    TYPE(AVHRR_Data), INTENT(INOUT) :: AVHRR
    INTEGER, INTENT(IN) :: POS1
    TYPE(AVHRR_Data), INTENT(IN) :: AVHRR_New
    INTEGER, INTENT(IN) :: POS2

    AVHRR%scanLineNumber(POS1) = &
         AVHRR_New%scanLineNumber(POS2)
    AVHRR%badTime(POS1) = &
         AVHRR_New%badTime(POS2)
    AVHRR%badNavigation(POS1) = &
         AVHRR_New%badNavigation(POS2)
    AVHRR%badCalibration(POS2) = &
         AVHRR_New%badCalibration(POS2)
    AVHRR%Lon(:,POS2) = &
         AVHRR_New%Lon(:,POS2)
    AVHRR%Lat(:,POS1) = &
         AVHRR_New%Lat(:,POS2)
    AVHRR%satZA(:,POS1) = &
         AVHRR_New%satZA(:,POS2)
    AVHRR%solZA(:,POS1) = &
         AVHRR_New%solZA(:,POS2)
    IF( ALLOCATED(AVHRR%relAz) )THEN
       AVHRR%relAz(:,POS1) = &
            AVHRR_New%relAz(:,POS2)
    ENDIF
    AVHRR%counts1(:,POS1) = &
         AVHRR_New%counts1(:,POS2)
    AVHRR%counts2(:,POS1) = &
         AVHRR_New%counts2(:,POS2)
    AVHRR%counts3(:,POS1) = &
         AVHRR_New%counts3(:,POS2)
    AVHRR%counts4(:,POS1) = &
         AVHRR_New%counts4(:,POS2)
    IF( ALLOCATED(AVHRR%counts5) )THEN
       AVHRR%counts5(:,POS1) = &
            AVHRR_New%counts5(:,POS2)
    ENDIF
    AVHRR%array1(:,POS1) = &
         AVHRR_New%array1(:,POS2)
    AVHRR%array2(:,POS1) = &
         AVHRR_New%array2(:,POS2)
    AVHRR%array3A(:,POS1) = &
         AVHRR_New%array3A(:,POS2)
    AVHRR%array3B(:,POS1) = &
         AVHRR_New%array3B(:,POS2)
    AVHRR%array4(:,POS1) = &
         AVHRR_New%array4(:,POS2)
    AVHRR%array5(:,POS1) = &
         AVHRR_New%array5(:,POS2)
    AVHRR%year(POS1) = &
         AVHRR_New%year(POS2)
    AVHRR%month(POS1) = &
         AVHRR_New%month(POS2)
    AVHRR%day(POS1) = &
         AVHRR_New%day(POS2)
    AVHRR%dayNo(POS1) = &
         AVHRR_New%dayNo(POS2)
    AVHRR%hours(POS1) = &
         AVHRR_New%hours(POS2)
    AVHRR%UTC_msecs(POS1) = &
         AVHRR_New%UTC_msecs(POS2)
    AVHRR%time(POS1) = &
         AVHRR_New%time(POS2)
    AVHRR%prt1(POS1) = &
         AVHRR_New%prt1(POS2)
    AVHRR%prt2(POS1) = &
         AVHRR_New%prt2(POS2)
    AVHRR%prt3(POS1) = &
         AVHRR_New%prt3(POS2)
    AVHRR%prt4(POS1) = &
         AVHRR_New%prt4(POS2)
    AVHRR%bb3(POS1) = &
         AVHRR_New%bb3(POS2)
    AVHRR%bb4(POS1) = &
         AVHRR_New%bb4(POS2)
    IF( ALLOCATED(AVHRR%bb5) )THEN
       AVHRR%bb5(POS1) = &
            AVHRR_New%bb5(POS2)
    ENDIF
    AVHRR%sp3(POS1) = &
         AVHRR_New%sp3(POS2)
    AVHRR%sp4(POS1) = &
         AVHRR_New%sp4(POS2)
    IF( ALLOCATED(AVHRR%sp5) )THEN
       AVHRR%sp5(POS1) = &
            AVHRR_New%sp5(POS2)
    ENDIF
    AVHRR%bbodyFilter3(:,POS1) = &
         AVHRR_New%bbodyFilter3(:,POS2)
    AVHRR%bbodyFilter4(:,POS1) = &
         AVHRR_New%bbodyFilter4(:,POS2)
    IF( ALLOCATED(AVHRR%bbodyFilter5) )THEN
       AVHRR%bbodyFilter5(:,POS1) = &
            AVHRR_New%bbodyFilter5(:,POS2)
    ENDIF
    AVHRR%spaceFilter3(:,POS1) = &
         AVHRR_New%spaceFilter3(:,POS2)
    AVHRR%spaceFilter4(:,POS1) = &
         AVHRR_New%spaceFilter4(:,POS2)
    IF( ALLOCATED(AVHRR%spaceFilter5) )THEN
       AVHRR%spaceFilter5(:,POS1) = &
            AVHRR_New%spaceFilter5(:,POS2)
    ENDIF
    AVHRR%patch(POS1) = &
         AVHRR_New%patch(POS2)
    IF( ALLOCATED(AVHRR%patchExtended) )THEN
       AVHRR%patchExtended(POS1) = &
            AVHRR_New%patchExtended(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%Radiator) )THEN
       AVHRR%Radiator(POS1) = &
            AVHRR_New%Radiator(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%Cooler) )THEN
       AVHRR%Cooler(POS1) = &
            AVHRR_New%Cooler(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%a_d_conv) )THEN
       AVHRR%a_d_conv(POS1) = &
            AVHRR_New%a_d_conv(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%motor) )THEN
       AVHRR%motor(POS1) = &
            AVHRR_New%motor(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%motorCurrent) )THEN
       AVHRR%motorCurrent(POS1) = &
            AVHRR_New%motorCurrent(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%electronics) )THEN
       AVHRR%electronics(POS1) = &
            AVHRR_New%electronics(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%baseplate) )THEN
       AVHRR%baseplate(POS1) = &
            AVHRR_New%baseplate(POS2)
    ENDIF
    AVHRR%calib1(:,POS1) = &
         AVHRR_New%calib1(:,POS2)
    AVHRR%calib1_2(:,POS1) = &
         AVHRR_New%calib1_2(:,POS2)
    AVHRR%calib1_intercept(POS1) = &
         AVHRR_New%calib1_intercept(POS2)
    AVHRR%calib2(:,POS1) = &
         AVHRR_New%calib2(:,POS2)
    AVHRR%calib2_2(:,POS1) = &
         AVHRR_New%calib2_2(:,POS2)
    AVHRR%calib2_intercept(POS1) = &
         AVHRR_New%calib2_intercept(POS2)
    IF( ALLOCATED(AVHRR%calib3A) )THEN
       AVHRR%calib3A(:,POS1) = &
            AVHRR_New%calib3A(:,POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%calib3A_2) )THEN
       AVHRR%calib3A_2(:,POS1) = &
            AVHRR_New%calib3A_2(:,POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%calib3A_intercept) )THEN
       AVHRR%calib3A_intercept(POS1) = &
            AVHRR_New%calib3A_intercept(POS2)
    ENDIF
    AVHRR%calib3(:,POS1) = &
         AVHRR_New%calib3(:,POS2)
    AVHRR%calib4(:,POS1) = &
         AVHRR_New%calib4(:,POS2)
    IF( ALLOCATED(AVHRR%calib5) )THEN
       AVHRR%calib5(:,POS1) = &
            AVHRR_New%calib5(:,POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%clavr_mask) )THEN
       AVHRR%clavr_mask(:,POS1) = &
            AVHRR_New%clavr_mask(:,POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%clavrx_mask) )THEN
       AVHRR%clavrx_mask(:,POS1) = &
            AVHRR_New%clavrx_mask(:,POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%clavrx_prb) )THEN
       AVHRR%clavrx_prb(:,POS1) = &
            AVHRR_New%clavrx_prb(:,POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%orig_solar_contamination_3B) )THEN
       AVHRR%orig_solar_contamination_3B(POS1) = &
            AVHRR_New%orig_solar_contamination_3B(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%orig_solar_contamination_4) )THEN
       AVHRR%orig_solar_contamination_4(POS1) = &
            AVHRR_New%orig_solar_contamination_4(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%orig_solar_contamination_5) )THEN
       AVHRR%orig_solar_contamination_5(POS1) = &
            AVHRR_New%orig_solar_contamination_5(POS2)
    ENDIF
    IF( ALLOCATED(AVHRR%satelliteAltitude) )THEN
       AVHRR%satelliteAltitude(POS1) = &
            AVHRR_New%satelliteAltitude(POS2)
    ENDIF

  END SUBROUTINE Copy_Scanline
  
  LOGICAL FUNCTION corr_diff_time(time,I,size,newtime,J,outJ)

    REAL(GbcsDble), INTENT(IN) :: time(:)
    INTEGER, INTENT(IN) :: I
    INTEGER, INTENT(IN) :: size
    REAL(GbcsDble), INTENT(IN) :: newtime(:)
    INTEGER, INTENT(IN) :: J
    INTEGER, INTENT(OUT) :: outJ

    ! Local variables
    INTEGER :: K
    REAL(GbcsDble) :: min_diff
    REAL(GbcsDble) :: diff

    corr_diff_time = .TRUE.
    min_diff = 1d30
    outJ = -1
    FindLoop: DO K=-3,3 
       IF( J+K .gt. 0 .and. J+K .le. size )THEN
          diff = ABS(time(I)-newtime(J+K))
          IF( diff .gt. 2 )THEN
             EXIT FindLoop
          ENDIF
          IF( diff .lt. min_diff )THEN
             outJ = J+K
             min_diff = diff
          ENDIF
       ENDIF
    END DO FindLoop
    IF( -1 .eq. outJ )THEN
       corr_diff_time = .FALSE.
    ENDIF
    IF( min_diff .gt. 0.5 )THEN
       corr_diff_time = .FALSE.
    ENDIF

  END FUNCTION corr_diff_time

  SUBROUTINE Merge_AVHRR(AVHRR,AVHRR_New)

    TYPE(AVHRR_Data), INTENT(INOUT), TARGET :: AVHRR
    TYPE(AVHRR_Data), INTENT(IN) :: AVHRR_new
    
    ! Local variables
    INTEGER :: I
    INTEGER :: J
    INTEGER :: outJ
    INTEGER :: stat
    INTEGER :: start_pos
    INTEGER :: extra_lines
    INTEGER :: first_position
    INTEGER :: last_position
    INTEGER :: back_pos
    INTEGER :: back_pos2
    INTEGER :: offset
    
    REAL(GbcsDble) :: start_time 

    LOGICAL, ALLOCATABLE :: good_data(:)

    TYPE(AVHRR_Data), POINTER :: AVHRR_Ptr

    !
    ! Setup AVHRR pointer
    !
    AVHRR_Ptr => AVHRR
    

    !
    ! Get first valid scanline
    !
    start_time = -1
    start_pos = -1
    I=0
    DO WHILE(-1 .eq. start_time)
       I = I+1
       IF( I .gt. AVHRR_New%arraySize )THEN
          CALL Gbcs_Warning(.TRUE.,'No good data found','Merge_AVHRR',&
               'extract_l1b_data.f90')
          RETURN
       ENDIF
       IF( .not. isNaN(AVHRR_New%time(I)) )THEN
          IF( AVHRR_New%time(I) .gt. 0 )THEN
             start_time = AVHRR_New%time(I)
             start_pos = I
          ENDIF
       ENDIF
    END DO

    !
    ! Start comparing data and fill out missing data at end of original set
    !
    ALLOCATE(good_data(AVHRR%arraySize),STAT=stat)
    IF( 0 .ne. stat )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot allocate good_data',&
            'Merge_AVHRR','extract_l1b_data.f90')
    ENDIF
    good_data=.TRUE.
    DO I=1,AVHRR%arraySize
       !
       ! Check that line has a time
       !
       IF( .not. isNaN(AVHRR%time(I)) .and. 0 .lt. AVHRR%time(I) )THEN
          ! 
          ! Check if in overlap
          !
          IF( AVHRR%time(I) .ge. start_time )THEN
             !
             ! Check for bad navigation/flags etc.
             !
             IF( .not. Check_Scanline(AVHRR,I) )THEN
                !
                ! Bad data in first section
                !
                FindLoop2: DO J=start_pos,AVHRR_New%arraySize
                   !
                   ! As times don't exactly match have to look for
                   !
                   IF( corr_diff_time(AVHRR%time,I,AVHRR_New%arraySize,&
                        AVHRR_New%time,J,outJ) )THEN
                      ! 
                      ! Found same line
                      !
                      IF( Check_Scanline(AVHRR_New,outJ) )THEN
                         CALL Copy_Scanline(AVHRR,I,AVHRR_New,outJ)
                      ENDIF
                      EXIT FindLoop2
                   ENDIF
                END DO FindLoop2
             ENDIF
          ENDIF
       ELSE
          !
          ! See if we are at a point which overlaps with new data
          !
          ! If not time present fill on the basis of the last good time
          !
          !
          ! Look back to find last good time to work out how to fill
          !
          back_pos=-1
          FindBack: DO J=I,1,-1
             IF( 0. .lt. AVHRR%time(J) )THEN
                back_pos=J
                EXIT findBack
             ENDIF
          END DO FindBack
          !
          ! Found previous time
          !
          IF( 0 .lt. back_pos )THEN
             !
             ! Check it is in overlap
             ! 
             IF( AVHRR%time(back_pos) .ge. start_time )THEN
                ! 
                ! Calculate offset from last time
                !
                offset = I-back_pos
                !
                ! Find overlap position for last time
                !
                back_pos2 = -1
                FindLoop: DO J=start_pos,AVHRR_New%arraySize
                   !
                   ! As times don't exactly match have to look for
                   !
                   IF( corr_diff_time(AVHRR%time,back_pos,&
                        AVHRR_New%arraySize,&
                        AVHRR_New%time,J,outJ) )THEN
                      back_pos2=outJ
                      EXIT FindLoop
                   ENDIF
                END DO FindLoop
                IF( 0 .lt. back_pos2 )THEN
                   !
                   ! Now check new scanline with offset
                   !
                   IF( Check_Scanline(AVHRR_New,back_pos2+offset) )THEN
                      CALL Copy_Scanline(AVHRR,back_pos+offset,&
                           AVHRR_New,back_pos2+offset)
                   ELSE
                      good_data(I) = .FALSE.
                   ENDIF
                ELSE
                   good_data(I) = .FALSE.
                ENDIF
             ELSE
                good_data(I) = .FALSE.
             ENDIF
          ELSE
             good_data(I) = .FALSE.
          ENDIF
       ENDIF
    END DO

    !
    ! Now AVHRR structure should be as full as it can be find last good line
    !
    last_position=-1
    FindLast: DO I=AVHRR%arraySize,1,-1
       IF( good_data(I) )THEN
          last_position=I
          EXIT FindLast
       ENDIF
    END DO FindLast
    IF( -1 .eq. last_position )THEN
       CALL Gbcs_Critical(.TRUE.,'No good data found in first input structure',&
            'Merge_AVHRR','extract_l1b_data.f90')
    ENDIF
    IF( AVHRR%arraySize .lt. last_position )THEN
       last_position=last_position+1
    ELSE
       last_position=AVHRR%arraySize
    ENDIF

    ! 
    ! Find equivalent point in new input
    !
    first_position=-1
    FindFirst: DO I=1,AVHRR_New%arraySize
       IF( corr_diff_time(AVHRR%time,last_position,AVHRR_New%arraySize,AVHRR_New%time,I,outJ) )THEN
          first_position = outJ+1
          EXIT FindFirst
       ENDIF
    END DO FindFirst
    IF( -1 .eq. first_position )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot find first position',&
            'Merge_AVHRR','extract_l1b_data.f90')
    ENDIF

    !
    ! Append new data to old structure
    !
    extra_lines = AVHRR_New%arraySize - first_position - &
         (AVHRR%arraySize-last_position)
    CALL Reallocate_outData(AVHRR_Ptr,extra_lines)

    !
    ! Now add in extra data - original raw data
    !
    AVHRR%scanLineNumber(last_position:AVHRR%arraySize) = &
         AVHRR_New%scanLineNumber(first_position:AVHRR_New%arraySize)
    AVHRR%badTime(last_position:AVHRR%arraySize) = &
         AVHRR_New%badTime(first_position:AVHRR_New%arraySize)
    AVHRR%badNavigation(last_position:AVHRR%arraySize) = &
         AVHRR_New%badNavigation(first_position:AVHRR_New%arraySize)
    AVHRR%badCalibration(last_position:AVHRR%arraySize) = &
         AVHRR_New%badCalibration(first_position:AVHRR_New%arraySize)
    AVHRR%Lon(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%Lon(:,first_position:AVHRR_New%arraySize)
    AVHRR%Lat(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%Lat(:,first_position:AVHRR_New%arraySize)
    AVHRR%satZA(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%satZA(:,first_position:AVHRR_New%arraySize)
    AVHRR%solZA(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%solZA(:,first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%relAz) )THEN
       AVHRR%relAz(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%relAz(:,first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%counts1(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%counts1(:,first_position:AVHRR_New%arraySize)
    AVHRR%counts2(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%counts2(:,first_position:AVHRR_New%arraySize)
    AVHRR%counts3(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%counts3(:,first_position:AVHRR_New%arraySize)
    AVHRR%counts4(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%counts4(:,first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%counts5) )THEN
       AVHRR%counts5(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%counts5(:,first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%array1(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%array1(:,first_position:AVHRR_New%arraySize)
    AVHRR%array2(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%array2(:,first_position:AVHRR_New%arraySize)
    AVHRR%array3A(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%array3A(:,first_position:AVHRR_New%arraySize)
    AVHRR%array3B(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%array3B(:,first_position:AVHRR_New%arraySize)
    AVHRR%array4(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%array4(:,first_position:AVHRR_New%arraySize)
    AVHRR%array5(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%array5(:,first_position:AVHRR_New%arraySize)
    AVHRR%year(last_position:AVHRR%arraySize) = &
         AVHRR_New%year(first_position:AVHRR_New%arraySize)
    AVHRR%month(last_position:AVHRR%arraySize) = &
         AVHRR_New%month(first_position:AVHRR_New%arraySize)
    AVHRR%day(last_position:AVHRR%arraySize) = &
         AVHRR_New%day(first_position:AVHRR_New%arraySize)
    AVHRR%dayNo(last_position:AVHRR%arraySize) = &
         AVHRR_New%dayNo(first_position:AVHRR_New%arraySize)
    AVHRR%hours(last_position:AVHRR%arraySize) = &
         AVHRR_New%hours(first_position:AVHRR_New%arraySize)
    AVHRR%UTC_msecs(last_position:AVHRR%arraySize) = &
         AVHRR_New%UTC_msecs(first_position:AVHRR_New%arraySize)
    AVHRR%time(last_position:AVHRR%arraySize) = &
         AVHRR_New%time(first_position:AVHRR_New%arraySize)
    AVHRR%prt1(last_position:AVHRR%arraySize) = &
         AVHRR_New%prt1(first_position:AVHRR_New%arraySize)
    AVHRR%prt2(last_position:AVHRR%arraySize) = &
         AVHRR_New%prt2(first_position:AVHRR_New%arraySize)
    AVHRR%prt3(last_position:AVHRR%arraySize) = &
         AVHRR_New%prt3(first_position:AVHRR_New%arraySize)
    AVHRR%prt4(last_position:AVHRR%arraySize) = &
         AVHRR_New%prt4(first_position:AVHRR_New%arraySize)
    AVHRR%bb3(last_position:AVHRR%arraySize) = &
         AVHRR_New%bb3(first_position:AVHRR_New%arraySize)
    AVHRR%bb4(last_position:AVHRR%arraySize) = &
         AVHRR_New%bb4(first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%bb5) )THEN
       AVHRR%bb5(last_position:AVHRR%arraySize) = &
            AVHRR_New%bb5(first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%sp3(last_position:AVHRR%arraySize) = &
         AVHRR_New%sp3(first_position:AVHRR_New%arraySize)
    AVHRR%sp4(last_position:AVHRR%arraySize) = &
         AVHRR_New%sp4(first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%sp5) )THEN
       AVHRR%sp5(last_position:AVHRR%arraySize) = &
            AVHRR_New%sp5(first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%bbodyFilter3(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%bbodyFilter3(:,first_position:AVHRR_New%arraySize)
    AVHRR%bbodyFilter4(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%bbodyFilter4(:,first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%bbodyFilter5) )THEN
       AVHRR%bbodyFilter5(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%bbodyFilter5(:,first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%spaceFilter3(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%spaceFilter3(:,first_position:AVHRR_New%arraySize)
    AVHRR%spaceFilter4(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%spaceFilter4(:,first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%spaceFilter5) )THEN
       AVHRR%spaceFilter5(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%spaceFilter5(:,first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%patch(last_position:AVHRR%arraySize) = &
         AVHRR_New%patch(first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%patchExtended) )THEN
       AVHRR%patchExtended(last_position:AVHRR%arraySize) = &
            AVHRR_New%patchExtended(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%Radiator) )THEN
       AVHRR%Radiator(last_position:AVHRR%arraySize) = &
            AVHRR_New%Radiator(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%Cooler) )THEN
       AVHRR%Cooler(last_position:AVHRR%arraySize) = &
            AVHRR_New%Cooler(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%a_d_conv) )THEN
       AVHRR%a_d_conv(last_position:AVHRR%arraySize) = &
            AVHRR_New%a_d_conv(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%motor) )THEN
       AVHRR%motor(last_position:AVHRR%arraySize) = &
            AVHRR_New%motor(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%motorCurrent) )THEN
       AVHRR%motorCurrent(last_position:AVHRR%arraySize) = &
            AVHRR_New%motorCurrent(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%electronics) )THEN
       AVHRR%electronics(last_position:AVHRR%arraySize) = &
            AVHRR_New%electronics(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%baseplate) )THEN
       AVHRR%baseplate(last_position:AVHRR%arraySize) = &
            AVHRR_New%baseplate(first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%calib1(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%calib1(:,first_position:AVHRR_New%arraySize)
    AVHRR%calib1_2(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%calib1_2(:,first_position:AVHRR_New%arraySize)
    AVHRR%calib1_intercept(last_position:AVHRR%arraySize) = &
         AVHRR_New%calib1_intercept(first_position:AVHRR_New%arraySize)
    AVHRR%calib2(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%calib2(:,first_position:AVHRR_New%arraySize)
    AVHRR%calib2_2(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%calib2_2(:,first_position:AVHRR_New%arraySize)
    AVHRR%calib2_intercept(last_position:AVHRR%arraySize) = &
         AVHRR_New%calib2_intercept(first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%calib3A) )THEN
       AVHRR%calib3A(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%calib3A(:,first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%calib3A_2) )THEN
       AVHRR%calib3A_2(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%calib3A_2(:,first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%calib3A_intercept) )THEN
       AVHRR%calib3A_intercept(last_position:AVHRR%arraySize) = &
            AVHRR_New%calib3A_intercept(first_position:AVHRR_New%arraySize)
    ENDIF
    AVHRR%calib3(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%calib3(:,first_position:AVHRR_New%arraySize)
    AVHRR%calib4(:,last_position:AVHRR%arraySize) = &
         AVHRR_New%calib4(:,first_position:AVHRR_New%arraySize)
    IF( ALLOCATED(AVHRR%calib5) )THEN
       AVHRR%calib5(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%calib5(:,first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%clavr_mask) )THEN
       AVHRR%clavr_mask(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%clavr_mask(:,first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%clavrx_mask) )THEN
       AVHRR%clavrx_mask(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%clavrx_mask(:,first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%clavrx_prb) )THEN
       AVHRR%clavrx_prb(:,last_position:AVHRR%arraySize) = &
            AVHRR_New%clavrx_prb(:,first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%orig_solar_contamination_3B) )THEN
       AVHRR%orig_solar_contamination_3B(last_position:AVHRR%arraySize) = &
            AVHRR_New%orig_solar_contamination_3B(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%orig_solar_contamination_4) )THEN
       AVHRR%orig_solar_contamination_4(last_position:AVHRR%arraySize) = &
            AVHRR_New%orig_solar_contamination_4(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%orig_solar_contamination_5) )THEN
       AVHRR%orig_solar_contamination_5(last_position:AVHRR%arraySize) = &
            AVHRR_New%orig_solar_contamination_5(first_position:AVHRR_New%arraySize)
    ENDIF
    IF( ALLOCATED(AVHRR%satelliteAltitude) )THEN
       AVHRR%satelliteAltitude(last_position:AVHRR%arraySize) = &
            AVHRR_New%satelliteAltitude(first_position:AVHRR_New%arraySize)
    ENDIF
        
  END SUBROUTINE Merge_AVHRR

  LOGICAL FUNCTION check_overlap(file1,file2)

    CHARACTER(LEN=*), INTENT(IN) :: file1
    CHARACTER(LEN=*), INTENT(IN) :: file2

    ! Local variables
    INTEGER POS_Dir1
    INTEGER POS_Dir2
    INTEGER POS_Year1
    INTEGER POS_Year2
    INTEGER POS_Start1
    INTEGER POS_Start2
    INTEGER POS_End1
    INTEGER POS_End2

    REAL(GbcsDble) :: jday1_start
    REAL(GbcsDble) :: jday1_end
    REAL(GbcsDble) :: jday2_start
    REAL(GbcsDble) :: jday2_end

    !
    ! If directory included find position of last /
    ! Note if '/' not there then INDEX returns zero which is OK
    !
    POS_Dir1 = INDEX(file1,'/',.TRUE.)
    POS_Dir2 = INDEX(file2,'/',.TRUE.)

    !
    ! Check filelength - must have at least 30 characters for NOAA Level 1B data
    !
    IF( LEN_TRIM(file1)-POS_Dir1 .lt. 30 )THEN
       CALL Gbcs_Critical(.TRUE.,'File1 not a NOAA Level 1B file (length)',&
            'check_overlap','extract_l1b_data.f90')
    ENDIF
    IF( LEN_TRIM(file2)-POS_Dir2 .lt. 30 )THEN
       CALL Gbcs_Critical(.TRUE.,'File2 not a NOAA Level 1B file (length)',&
            'check_overlap','extract_l1b_data.f90')
    ENDIF

    !
    ! Check this is an AVHRR file (NOAA Level 1B)
    ! If it is, get locations of year, day, hour, min
    !
    IF( 'NSS.GHRR.' .ne. file1(POS_Dir1+1:POS_Dir1+9) )THEN
       print *,TRIM(file1)
       print *,file1(POS_Dir1+1:POS_Dir1+9)
       CALL Gbcs_Critical(.TRUE.,'File1 not a NOAA Level 1B file (NSS.)',&
            'check_overlap','extract_l1b_data.f90')
    ENDIF
    IF( 'NSS.GHRR.' .ne. file2(POS_Dir2+1:POS_Dir2+9) )THEN
       print *,TRIM(file2)
       print *,file2(POS_Dir2+1:POS_Dir2+9)
       CALL Gbcs_Critical(.TRUE.,'File1 not a NOAA Level 1B file (NSS.)',&
            'check_overlap','extract_l1b_data.f90')
    ENDIF

    IF( file1(POS_Dir1+12:POS_Dir1+13) .ne. '.D' )THEN
       print *,TRIM(file1)
       print *,file1(POS_Dir1+12:POS_Dir1+13)
       CALL Gbcs_Critical(.TRUE.,'File1 not a NOAA Level 1B file (.D)',&
            'check_overlap','extract_l1b_data.f90')
    ELSE
       POS_Year1 = POS_Dir1+14
    ENDIF

    IF( file2(POS_Dir2+12:POS_Dir2+13) .ne. '.D' )THEN
       print *,TRIM(file2)
       print *,file2(POS_Dir2+12:POS_Dir2+13)
       CALL Gbcs_Critical(.TRUE.,'File2 not a NOAA Level 1B file (.D)',&
            'check_overlap','extract_l1b_data.f90')
    ELSE
       POS_Year2 = POS_Dir2+14
    ENDIF

    IF( file1(POS_Dir1+19:POS_Dir1+20) .ne. '.S' )THEN
       print *,TRIM(file1)
       print *,file1(POS_Dir1+19:POS_Dir1+20)
       CALL Gbcs_Critical(.TRUE.,'File1 not a NOAA Level 1B file (.S)',&
            'check_overlap','extract_l1b_data.f90')
    ELSE
       POS_Start1 = POS_Dir1+21
    ENDIF

    IF( file2(POS_Dir2+19:POS_Dir2+20) .ne. '.S' )THEN
       print *,TRIM(file2)
       print *,file2(POS_Dir2+19:POS_Dir2+20)
       CALL Gbcs_Critical(.TRUE.,'File2 not a NOAA Level 1B file (.S)',&
            'check_overlap','extract_l1b_data.f90')
    ELSE
       POS_Start2 = POS_Dir2+21
    ENDIF

    IF( file1(POS_Dir1+25:POS_Dir1+26) .ne. '.E' )THEN
       print *,TRIM(file1)
       print *,file1(POS_Dir1+25:POS_Dir1+26)
       CALL Gbcs_Critical(.TRUE.,'File1 not a NOAA Level 1B file (.E)',&
            'check_overlap','extract_l1b_data.f90')
    ELSE
       POS_End1 = POS_Dir1+27
    ENDIF

    IF( file2(POS_Dir2+25:POS_Dir2+26) .ne. '.E' )THEN
       print *,TRIM(file2)
       print *,file2(POS_Dir2+25:POS_Dir2+26)
       CALL Gbcs_Critical(.TRUE.,'File2 not a NOAA Level 1B file (.E)',&
            'check_overlap','extract_l1b_data.f90')
    ELSE
       POS_End2 = POS_Dir2+27
    ENDIF

    !
    ! Get start/end times from filename - convert to julian day to get round day/year 
    ! boundaries
    ! Need end time of file1 and start time of file2
    !
    CALL Get_Jul_Day(file1,POS_Year1,POS_Start1,POS_End1,jday1_start,jday1_end)
    CALL Get_Jul_Day(file2,POS_Year2,POS_Start2,POS_End2,jday2_start,jday2_end)

    IF( jday1_end .ge. jday2_start )THEN
       check_overlap = .TRUE.
    ELSE
       check_overlap = .FALSE.
    ENDIF

  END FUNCTION check_overlap

  SUBROUTINE Get_Jul_Day(file,POS_Year,POS_Start,POS_End,jday_start,jday_end)

    CHARACTER(LEN=*), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: POS_Year
    INTEGER, INTENT(IN) :: POS_Start
    INTEGER, INTENT(IN) :: POS_End
    REAL(GbcsDble), INTENT(OUT) :: jday_start
    REAL(GbcsDble), INTENT(OUT) :: jday_end

    ! Local variables
    INTEGER :: IOS
    INTEGER :: year
    INTEGER :: dayno
    INTEGER :: month
    INTEGER :: day
    INTEGER :: hour
    INTEGER :: minute
    INTEGER :: hour_start
    INTEGER :: minute_start
    TYPE(DateTime) :: datestr

    READ(file(POS_Year:POS_Year+1),'(i2)',IOSTAT=IOS)year
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse year from filename',&
            'Get_Jul_Day','extract_l1b_data.f90')
    ENDIF
    READ(file(POS_Year+2:POS_Year+4),'(i3)',IOSTAT=IOS)dayno
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse dayno from filename',&
            'Get_Jul_Day','extract_l1b_data.f90')
    ENDIF
    CALL Day_Number_to_Date(year,dayno,month,day)
    READ(file(POS_Start:POS_Start+1),'(i2)',IOSTAT=IOS)hour
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse hour from filename',&
            'Get_Jul_Day','extract_l1b_data.f90')
    ENDIF
    READ(file(POS_Start+2:POS_Start+3),'(i2)',IOSTAT=IOS)minute
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse minute from filename',&
            'Get_Jul_Day','extract_l1b_data.f90')
    ENDIF
    datestr%year = year
    datestr%month = month
    datestr%day = day
    datestr%hour = hour
    datestr%minute = minute
    datestr%seconds = 0
    datestr%SEC1000 = 0
    jday_start = Date_to_JD(datestr)
    READ(file(POS_End:POS_End+1),'(i2)',IOSTAT=IOS)hour
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse hour from filename',&
            'Get_Jul_Day','extract_l1b_data.f90')
    ENDIF
    READ(file(POS_End+2:POS_End+3),'(i2)',IOSTAT=IOS)minute
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse minute from filename',&
            'Get_Jul_Day','extract_l1b_data.f90')
    ENDIF
    datestr%hour = hour
    datestr%minute = minute
    jday_end = Date_to_JD(datestr)

    !
    ! Check to see if we haven't gone over day boundary
    !
    IF( jday_end .lt. jday_start )THEN
       jday_end = jday_end+1.0D0
    ENDIF

  END SUBROUTINE Get_Jul_Day

  INTEGER FUNCTION get_instr(file)

    CHARACTER(LEN=*), INTENT(IN) :: file

    INTEGER :: POS_Dir
    CHARACTER(LEN=2) :: instr_str

    POS_Dir = INDEX(file,'/',.TRUE.)

    instr_str = file(POS_Dir+10:POS_Dir+11)

    SELECT CASE(instr_str)
    CASE('TN')
       get_instr = 1
    CASE('NA')
       get_instr = 6
    CASE('NC')
       get_instr = 7
    CASE('NE')
       get_instr = 8
    CASE('NF')
       get_instr = 9
    CASE('NG')
       get_instr = 10
    CASE('NH')
       get_instr = 11
    CASE('ND')
       get_instr = 12
    CASE('NJ')
       get_instr = 14
    CASE('NK')
       get_instr = 15
    CASE('NL')
       get_instr = 16
    CASE('NM')
       get_instr = 17
    CASE('NN')
       get_instr = 18
    CASE('NP')
       get_instr = 19
    CASE('M2')
       get_instr = -1
    END SELECT

  END FUNCTION get_instr

  SUBROUTINE read_all_data(nFile,file1,file2,file3,uuid_in,&
       AVHRR_Total,instr,avhrr_rad_coefs)

    INTEGER, INTENT(IN) :: nFile
    CHARACTER(LEN=*), INTENT(IN) :: file1
    CHARACTER(LEN=*), INTENT(IN) :: file2
    CHARACTER(LEN=*), INTENT(IN) :: file3
    CHARACTER(LEN=*), INTENT(IN) :: uuid_in
    TYPE(AVHRR_Data), INTENT(OUT) :: AVHRR_Total
    INTEGER, INTENT(OUT) :: instr
    TYPE(AVHRR_Radiative_Coefs), INTENT(OUT) :: avhrr_rad_coefs

    ! Local variables
    TYPE(AVHRR_Data) :: AVHRR

    instr = get_instr(file1)
    IF( nFile .eq. 1 )THEN
       CALL read_file(file1,AVHRR_Total,uuid_in,avhrr_rad_coefs=avhrr_rad_coefs)
    ELSE IF( nFile .eq. 2 )THEN
       IF( .not. check_overlap(file1,file2) )THEN
          CALL Gbcs_Critical(.TRUE.,'No overlap between file1 and file2',&
               'read_all_data','extract_l1b_data.f90')
       ENDIF
       CALL read_file(file1,AVHRR_Total,uuid_in,avhrr_rad_coefs=avhrr_rad_coefs)
       CALL read_file(file2,AVHRR,uuid_in)
       CALL merge_avhrr(AVHRR_Total,AVHRR)
       CALL Deallocate_OutData(AVHRR)
    ELSE
       IF( .not. check_overlap(file1,file2) )THEN
          CALL Gbcs_Critical(.TRUE.,'No overlap between file1 and file2',&
               'read_all_data','extract_l1b_data.f90')
       ENDIF
       CALL read_file(file1,AVHRR_Total,uuid_in,avhrr_rad_coefs=avhrr_rad_coefs)
       CALL read_file(file2,AVHRR,uuid_in)
       CALL merge_avhrr(AVHRR_Total,AVHRR)
       CALL Deallocate_OutData(AVHRR)
       IF( .not. check_overlap(file2,file3) )THEN
          CALL Gbcs_Critical(.TRUE.,'No overlap between file2 and file3',&
               'read_all_data','extract_l1b_data.f90')
       ENDIF
       CALL read_file(file3,AVHRR,uuid_in)
       CALL merge_avhrr(AVHRR_Total,AVHRR)
       CALL Deallocate_OutData(AVHRR)
    ENDIF

  END SUBROUTINE read_all_data

  SUBROUTINE read_file(infile,AVHRR,uuid_in,avhrr_rad_coefs)

    CHARACTER(LEN=*), INTENT(IN) :: infile
    TYPE(AVHRR_Data), INTENT(OUT) :: AVHRR
    CHARACTER(LEN=*), INTENT(IN) :: uuid_in
    TYPE(AVHRR_Radiative_Coefs), INTENT(OUT), OPTIONAL :: avhrr_rad_coefs

    ! Local variables
    INTEGER :: pid
    INTEGER :: stat
    INTEGER :: POS

    CHARACTER(LEN=256) :: inDirectory
    CHARACTER(LEN=256) :: infilename
    CHARACTER(LEN=256) :: temp_file
    CHARACTER(LEN=256) :: command_str

    TYPE(Imagery) :: IMG
    REAL :: coefs1(7)
    REAL :: coefs2(7)
    REAL :: coefs3(7)

    LOGICAL :: remove_temp_file

    !
    ! Check to see if we need to uncompress the data
    !
    remove_temp_file=.FALSE.
    IF( 0 .ne. INDEX(infile,'.gz') )THEN
       WRITE(command_str,'(''cp -f '',a,'' temp_file.'',a,''.gz'')')&
            TRIM(infile),TRIM(uuid_in)
       CALL SYSTEM(command_str,STATUS=stat)
       CALL Gbcs_Critical(stat.ne.0,'Cannot copy file to tempfile','Convert',&
            'extract_l1b_data.f90')
       WRITE(command_str,'(''gunzip -f temp_file.'',a,''.gz'')')&
            TRIM(uuid_in)
       CALL SYSTEM(command_str,STATUS=stat)
       CALL Gbcs_Critical(stat.ne.0,'Cannot gunzip tempfile','Convert',&
            'extract_l1b_data.f90')
       inDirectory = './'
       WRITE(inFilename,'(''temp_file.'',a)')TRIM(uuid_in)
       remove_temp_file = .TRUE.
    ELSE
       POS=INDEX(infile,'/',.TRUE.)
       IF( 0 .ne. POS )THEN
          inDirectory = infile(1:POS)
       ELSE
          inDirectory = './'
       ENDIF
       inFilename = infile(POS+1:LEN(infile))
    ENDIF

    IMG%DataDir = TRIM(inDirectory)
    IMG%DataFile = TRIM(inFilename)

    IMG%N_Chans = 2
    IMG%Gbcs_Map(1) = Gbcs_VIS_006
    IMG%Gbcs_Map(2) = Gbcs_VIS_008

    CALL Load_Imagery(IMG,outputData=AVHRR,use_new_calibration=.FALSE.)
    IF( PRESENT(avhrr_rad_coefs) )THEN
       CALL Get_Calib_Coefficients(AVHRR%AVHRR_No,coefs1,coefs2,coefs3)
       avhrr_rad_coefs%nuc = (/coefs1(3),coefs2(3),coefs3(3)/)
       avhrr_rad_coefs%aval = (/coefs1(4),coefs2(4),coefs3(4)/)
       avhrr_rad_coefs%bval = (/coefs1(5),coefs2(5),coefs3(5)/)
    ENDIF
    !
    ! Copy reflectances from IMG to AVHRR structure
    !
    AVHRR%array1 = IMG%Chan_Data(Gbcs_VIS_006)%d
    AVHRR%array2 = IMG%Chan_Data(Gbcs_VIS_008)%d

    IF( remove_temp_file )THEN
       WRITE(command_str,'(''rm -f '',a)')TRIM(inFilename)
       CALL SYSTEM(command_str,STATUS=stat)
       CALL Gbcs_Critical(stat.ne.0,'Cannot remove tempfile','Convert',&
            'extract_l1b_data.f90')
    ENDIF

  END SUBROUTINE read_file
  
  LOGICAL FUNCTION isNaN_Dble( inval )

    REAL(GbcsDBle), INTENT(IN) :: inval

    IF( inval .ne. inval )THEN
       isNaN_Dble = .TRUE.
    ELSE
       isNaN_Dble = .FALSE.
    ENDIF

  END FUNCTION isNaN_Dble

  LOGICAL FUNCTION isNaN_Real( inval )

    REAL(GbcsReal), INTENT(IN) :: inval

    IF( inval .ne. inval )THEN
       isNaN_Real = .TRUE.
    ELSE
       isNaN_Real = .FALSE.
    ENDIF

  END FUNCTION isNaN_Real

  SUBROUTINE Check_NetCDF(status)

    INTEGER, INTENT(IN) :: status

    ! Local variable
    CHARACTER(LEN=256) :: strerr

    IF( 0 .ne. status )THEN
       WRITE(strerr,'(''ERROR: stat='',i5,'' error='',a)')&
            status,TRIM(NF90_STRERROR(status))
       WRITE(*,'(a)')TRIM(strerr)
       STOP 1
    ENDIF

  END SUBROUTINE Check_NetCDF

  SUBROUTINE Output_Data(ofile,AVHRR,instr,avhrr_rad_coefs)

    CHARACTER(LEN=*), INTENT(IN) :: ofile
    TYPE(AVHRR_Data), INTENT(IN) :: AVHRR
    INTEGER, INTENT(IN) :: instr
    TYPE(AVHRR_Radiative_Coefs), INTENT(IN) :: avhrr_rad_coefs

    ! Local variables
    INTEGER :: ncid
    INTEGER :: varid
    INTEGER :: status

    INTEGER :: dim_nscans
    INTEGER :: dim_nelem
    INTEGER :: dim_ncal
    INTEGER :: dim_nfilters
    INTEGER :: dims(2)    

    INTEGER :: varid_nuc
    INTEGER :: varid_aval
    INTEGER :: varid_bval
    INTEGER :: varid_year
    INTEGER :: varid_month
    INTEGER :: varid_day
    INTEGER :: varid_hours
    INTEGER :: varid_time
    INTEGER :: varid_lat
    INTEGER :: varid_lon
    INTEGER :: varid_satZA
    INTEGER :: varid_solZA
    INTEGER :: varid_ch1
    INTEGER :: varid_ch2
    INTEGER :: varid_ch3
    INTEGER :: varid_ch4
    INTEGER :: varid_ch5
    INTEGER :: varid_prt1
    INTEGER :: varid_prt2
    INTEGER :: varid_prt3
    INTEGER :: varid_prt4
    INTEGER :: varid_counts1
    INTEGER :: varid_counts2
    INTEGER :: varid_counts3
    INTEGER :: varid_counts4
    INTEGER :: varid_counts5
    INTEGER :: varid_bb3
    INTEGER :: varid_bb4
    INTEGER :: varid_bb5
    INTEGER :: varid_sp3
    INTEGER :: varid_sp4
    INTEGER :: varid_sp5
    INTEGER :: varid_bbFilter3
    INTEGER :: varid_bbFilter4
    INTEGER :: varid_bbFilter5
    INTEGER :: varid_spFilter3
    INTEGER :: varid_spFilter4
    INTEGER :: varid_spFilter5
    INTEGER :: varid_patch
    INTEGER :: varid_solar_3B
    INTEGER :: varid_solar_4
    INTEGER :: varid_solar_5

    INTEGER :: I
    INTEGER :: stat
    INTEGER, ALLOCATABLE :: Flag(:)

    status = NF90_CREATE(ofile,0,ncid)
    CALL Check_NetCDF(status)

    status = NF90_DEF_DIM(ncid,'nscans',AVHRR%arraySize,dim_nscans)
    CALL Check_NetCDF(status)

    status = NF90_DEF_DIM(ncid,'nelem',AVHRR%nelem,dim_nelem)
    CALL Check_NetCDF(status)

    status = NF90_DEF_DIM(ncid,'ncal',10,dim_ncal)
    CALL Check_NetCDF(status)

    status = NF90_DEF_DIM(ncid,'nfilters',3,dim_nfilters)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'nuc',NF90_REAL,&
         dim_nfilters,varid_nuc)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'aval',NF90_REAL,&
         dim_nfilters,varid_aval)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'bval',NF90_REAL,&
         dim_nfilters,varid_bval)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Year',NF90_INT,&
         dim_nscans,varid_year)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Month',NF90_INT,&
         dim_nscans,varid_month)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Day',NF90_INT,&
         dim_nscans,varid_day)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Hours',NF90_REAL,&
         dim_nscans,varid_hours)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Time',NF90_DOUBLE,&
         dim_nscans,varid_time)
    CALL Check_NetCDF(status)

    dims=(/dim_nelem,dim_nscans/)
    status = NF90_DEF_VAR(ncid,'latitude',NF90_REAL,dims,varid_lat)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'longitude',NF90_REAL,dims,varid_lon)
    CALL Check_NetCDF(status)

    dims=(/dim_nelem,dim_nscans/)
    status = NF90_DEF_VAR(ncid,'satZA',NF90_REAL,dims,varid_satza)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'solZA',NF90_REAL,dims,varid_solza)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'ch1',NF90_REAL,dims,varid_ch1)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'ch2',NF90_REAL,dims,varid_ch2)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'ch3',NF90_REAL,dims,varid_ch3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'ch4',NF90_REAL,dims,varid_ch4)
    CALL Check_NetCDF(status)

    IF( ALLOCATED(AVHRR%array5) )THEN
       status = NF90_DEF_VAR(ncid,'ch5',NF90_REAL,dims,varid_ch5)
       CALL Check_NetCDF(status)
    ENDIF

    status = NF90_DEF_VAR(ncid,'prt1',NF90_REAL,dim_nscans,varid_prt1)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'prt2',NF90_REAL,dim_nscans,varid_prt2)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'prt3',NF90_REAL,dim_nscans,varid_prt3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'prt4',NF90_REAL,dim_nscans,varid_prt4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Counts1',NF90_REAL,dims,varid_counts1)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Counts2',NF90_REAL,dims,varid_counts2)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Counts3',NF90_REAL,dims,varid_counts3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Counts4',NF90_REAL,dims,varid_counts4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Counts5',NF90_REAL,dims,varid_counts5)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'BB3',NF90_REAL,dim_nscans,varid_bb3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'BB4',NF90_REAL,dim_nscans,varid_bb4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'BB5',NF90_REAL,dim_nscans,varid_bb5)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Sp3',NF90_REAL,dim_nscans,varid_sp3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Sp4',NF90_REAL,dim_nscans,varid_sp4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Sp5',NF90_REAL,dim_nscans,varid_sp5)
    CALL Check_NetCDF(status)

    dims=(/dim_ncal,dim_nscans/)
    status = NF90_DEF_VAR(ncid,'BB3Filter',NF90_REAL,dims,varid_bbFilter3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'BB4Filter',NF90_REAL,dims,varid_bbFilter4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'BB5Filter',NF90_REAL,dims,varid_bbFilter5)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Sp3Filter',NF90_REAL,dims,varid_spFilter3)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Sp4Filter',NF90_REAL,dims,varid_spFilter4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Sp5Filter',NF90_REAL,dims,varid_spFilter5)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Patch',NF90_REAL,dim_nscans,varid_patch)
    CALL Check_NetCDF(status)    

    status = NF90_DEF_VAR(ncid,'Solar_Contamination_3B',NF90_INT,&
         dim_nscans,varid_solar_3B)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Solar_Contamination_4',NF90_INT,&
         dim_nscans,varid_solar_4)
    CALL Check_NetCDF(status)

    status = NF90_DEF_VAR(ncid,'Solar_Contamination_5',NF90_INT,&
         dim_nscans,varid_solar_5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_ATT(ncid,NF90_GLOBAL,'Instrument_No',instr)
    CALL Check_NetCDF(status)

    status = NF90_ENDDEF(ncid)
    CALL Check_NetCDF(status)
    
    status = NF90_PUT_VAR(ncid,varid_lat,AVHRR%Lat)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_nuc,avhrr_rad_coefs%nuc)
    CALL Check_NetCDF(status)    

    status = NF90_PUT_VAR(ncid,varid_aval,avhrr_rad_coefs%aval)
    CALL Check_NetCDF(status)    

    status = NF90_PUT_VAR(ncid,varid_bval,avhrr_rad_coefs%bval)
    CALL Check_NetCDF(status)    

    status = NF90_PUT_VAR(ncid,varid_year,AVHRR%Year)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_month,AVHRR%Month)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_day,AVHRR%Day)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_hours,AVHRR%hours)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_time,AVHRR%time)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_lon,AVHRR%Lon)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_satza,AVHRR%satZA)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_solza,AVHRR%solZA)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_counts1,AVHRR%Counts1)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_counts2,AVHRR%Counts2)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_counts3,AVHRR%Counts3)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_counts4,AVHRR%Counts4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_counts5,AVHRR%Counts5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_ch1,AVHRR%array1)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_ch2,AVHRR%array2)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_ch3,AVHRR%array3B)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_ch4,AVHRR%array4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_ch5,AVHRR%array5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_prt1,AVHRR%prt1)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_prt2,AVHRR%prt2)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_prt3,AVHRR%prt3)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_prt4,AVHRR%prt4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_bb3,AVHRR%bb3)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_bb4,AVHRR%bb4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_bb5,AVHRR%bb5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_sp3,AVHRR%sp3)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_sp4,AVHRR%sp4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_sp5,AVHRR%sp5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_bbFilter3,AVHRR%bbodyFilter3)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_bbFilter4,AVHRR%bbodyFilter4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_bbFilter5,AVHRR%bbodyFilter5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_spFilter3,AVHRR%spaceFilter3)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_spFilter4,AVHRR%spaceFilter4)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_spFilter5,AVHRR%spaceFilter5)
    CALL Check_NetCDF(status)

    status = NF90_PUT_VAR(ncid,varid_patch,AVHRR%Patch)
    CALL Check_NetCDF(status)

    ALLOCATE(Flag(AVHRR%arraySize),STAT=stat)
    IF( 0 .ne. stat )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot allocate Flag','Output_Data',&
            'extract_l1b_data.f90')
    ENDIF

    DO I=1,AVHRR%arraySize
       IF( AVHRR%orig_solar_contamination_3B(I) )THEN
          Flag(I) = 1
       ELSE
          Flag(I) = 0
       ENDIF
    END DO
    status = NF90_PUT_VAR(ncid,varid_solar_3B,Flag)
    CALL Check_NetCDF(status)

    DO I=1,AVHRR%arraySize
       IF( AVHRR%orig_solar_contamination_4(I) )THEN
          Flag(I) = 1
       ELSE
          Flag(I) = 0
       ENDIF
    END DO
    status = NF90_PUT_VAR(ncid,varid_solar_4,Flag)
    CALL Check_NetCDF(status)

    DO I=1,AVHRR%arraySize
       IF( AVHRR%orig_solar_contamination_5(I) )THEN
          Flag(I) = 1
       ELSE
          Flag(I) = 0
       ENDIF
    END DO
    status = NF90_PUT_VAR(ncid,varid_solar_5,Flag)
    CALL Check_NetCDF(status)

    DEALLOCATE(Flag)

    status = NF90_CLOSE(ncid)
    CALL Check_NetCDF(status)

  END SUBROUTINE Output_Data

END MODULE Extract_L1B_Data_module

PROGRAM Extract_L1b_Data
  
  USE GbcsErrorHandler
  USE NOAA_LoadAVHRRLevel1B 
  USE Extract_L1B_Data_Module

  INTEGER :: nFiles
  INTEGER :: nArgs
  INTEGER :: stat
  CHARACTER(LEN=256) :: file1
  CHARACTER(LEN=256) :: file2
  CHARACTER(LEN=256) :: file3
  CHARACTER(LEN=256) :: ofile
  CHARACTER(LEN=256) :: uuid_in
  TYPE(AVHRR_Data) :: AVHRR
  INTEGER :: instr
  TYPE(AVHRR_Radiative_Coefs) :: avhrr_rad_coefs

  nArgs = COMMAND_ARGUMENT_COUNT()
  IF( 2 .ge. nArgs .or. 5 .lt. nArgs )THEN
     CALL Gbcs_Critical(.TRUE.,'USAGE: ./extract_l1b_data.exe uuid file1 (file2) (file3) outputFile',&
          'Main','extract_l1b_data.f90')
  ENDIF

  CALL GET_COMMAND_ARGUMENT(1,uuid_in,STATUS=stat)
  IF( 0 .ne. stat )THEN
     CALL Gbcs_Critical(.TRUE.,'Cannot get first command line argument',&
          'Main','extract_l1b_data.f90')
  ENDIF

  CALL GET_COMMAND_ARGUMENT(2,file1,STATUS=stat)
  IF( 0 .ne. stat )THEN
     CALL Gbcs_Critical(.TRUE.,'Cannot get second command line argument',&
          'Main','extract_l1b_data.f90')
  ENDIF
  nFiles = nFiles + 1

  IF( nArgs .gt. 3 )THEN
     CALL GET_COMMAND_ARGUMENT(3,file2,STATUS=stat)
     IF( 0 .ne. stat )THEN
        CALL Gbcs_Critical(.TRUE.,'Cannot get third command line argument',&
             'Main','extract_l1b_data.f90')
     ENDIF
     nFiles = nFiles + 1
     IF( nfiles .gt. 4 )THEN
        CALL GET_COMMAND_ARGUMENT(4,file3,STATUS=stat)
        IF( 0 .ne. stat )THEN
           CALL Gbcs_Critical(.TRUE.,'Cannot get fourth command line argument',&
                'Main','extract_l1b_data.f90')
        ENDIF
        nFiles = nFiles + 1
        CALL GET_COMMAND_ARGUMENT(5,ofile,STATUS=stat)
        IF( 0 .ne. stat )THEN
           CALL Gbcs_Critical(.TRUE.,'Cannot get fifth command line argument',&
                'Main','extract_l1b_data.f90')
        ENDIF
     ELSE
        CALL GET_COMMAND_ARGUMENT(4,ofile,STATUS=stat)
        IF( 0 .ne. stat )THEN
           CALL Gbcs_Critical(.TRUE.,'Cannot get fourth command line argument',&
                'Main','extract_l1b_data.f90')
        ENDIF
     ENDIF
  ELSE
     CALL GET_COMMAND_ARGUMENT(3,ofile,STATUS=stat)
     IF( 0 .ne. stat )THEN
        CALL Gbcs_Critical(.TRUE.,'Cannot get third command line argument',&
             'Main','extract_l1b_data.f90')
     ENDIF     
  ENDIF

  IF( 0 .ne. INDEX(ofile,'NSS.') .or. 0 .eq. INDEX(ofile,'.nc') )THEN
     CALL Gbcs_Critical(.TRUE.,&
          'output file (last argument) either a Level1B name or no .nc',&
          'Main','extract_l1b_data.f90')
  ENDIF
  
  print *,'nfiles:',nfiles
  CALL read_all_data(nfiles,file1,file2,file3,uuid_in,&
       AVHRR,instr,avhrr_rad_coefs)

  !
  ! Write to NetCDF file
  !
  CALL output_data(ofile,AVHRR,instr,avhrr_rad_coefs)

  !
  ! Deallocate structure
  !
  CALL Deallocate_OutData(AVHRR)

END PROGRAM Extract_L1b_Data
