MODULE Util

  USE GbcsKinds
  USE GbcsTypes
  USE GbcsErrorHandler

  IMPLICIT NONE

CONTAINS

  LOGICAL FUNCTION Get_New_Filename(level1b_filename,filename,dir)&
       RESULT(remove_data)

    CHARACTER(LEN=*), INTENT(IN) :: level1b_filename
    CHARACTER(LEN=*), INTENT(OUT) :: filename
    CHARACTER(LEN=*), INTENT(OUT) :: dir

    ! Local variables
    INTEGER :: I
    INTEGER :: IOS
    INTEGER :: POS
    LOGICAL :: File_Exists
    CHARACTER(LEN=512) :: command

    remove_data = .FALSE.
    print *,'Level 1B:',TRIM(level1b_filename)
    dir = get_directory(level1b_filename)
    POS = INDEX(level1b_filename,'.gz')
    IF( 0 .ne. POS )THEN
       INQUIRE(FILE='tmp',EXIST=File_Exists)
       IF( .not. File_Exists )THEN
          CALL SYSTEM('/bin/mkdir -p tmp',STATUS=IOS)
          IF( 0 .ne. IOS )THEN
             CALL Gbcs_Critical(.TRUE.,'Cannot make tmp sudirectory',&
                  'Get_New_Filename','add_cnts_to_matches')
          ENDIF
       ENDIF
       WRITE(filename,'(a,''.'',a)')'avhrr_file',TRIM(get_unique_string())
       WRITE(command,'(''/bin/cp -f '',a,''/'',a,'' tmp/'',a,''.gz'')',&
            IOSTAT=IOS)&
            TRIM(dir),TRIM(level1b_filename),TRIM(filename)
       IF( 0 .ne. IOS )THEN
          CALL Gbcs_Critical(.TRUE.,'Cannot write new unique filename',&
               'Get_New_Filename','add_cnts_to_matches')
       ENDIF
       CALL SYSTEM(TRIM(command),STATUS=IOS)
       IF( 0 .ne. IOS )THEN
          CALL Gbcs_Critical(.TRUE.,'Cannot copy to new unique filename',&
               'Get_New_Filename','add_cnts_to_matches')
       ENDIF
       WRITE(command,'(''/bin/gunzip -f tmp/'',a,''.gz'')')TRIM(filename)
       CALL SYSTEM(TRIM(command),STATUS=IOS)
       IF( 0 .ne. IOS )THEN
          CALL Gbcs_Critical(.TRUE.,'Cannot gunzip temporary file',&
               'Get_New_Filename','add_cnts_to_matches')
       ENDIF
       dir = 'tmp/'
       remove_data = .TRUE.
    ELSE
       filename = TRIM(level1b_filename)
       remove_data = .FALSE.
    ENDIF

  END FUNCTION Get_New_Filename

  FUNCTION Get_Directory(filename)
    CHARACTER(LEN=256) :: Get_Directory
    CHARACTER(LEN=*), INTENT(IN) :: filename

    ! Local variables
    INTEGER :: IOS
    INTEGER :: year
    INTEGER :: dayno
    INTEGER :: month
    INTEGER :: day
    CHARACTER(LEN=2) :: sat_id
    CHARACTER(LEN=80) :: sat_name

    !
    ! Parse dayno/year from filename
    !
    READ(filename,'(13x,i2,i3)',IOSTAT=IOS)year,dayno
    IF( 0 .ne. IOS )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot parse year/dayno from filename',&
            'Get_Directory','Add_Cnts_to_<atches.f90')
    ENDIF
    IF( year .lt. 50 )THEN
       year = year + 2000
    ELSE
       year = year + 1900
    ENDIF
    month = Get_Month(year,dayno,day)

    !
    ! Get AVHRR type
    !
    sat_id = filename(10:11)
    SELECT CASE(sat_id)
       CASE('TN')
          sat_name = 'AVHRRTN_G'
       CASE('NA')
          sat_name = 'AVHRR06_G'
       CASE('NC')
          sat_name = 'AVHRR07_G'
       CASE('NE')
          sat_name = 'AVHRR08_G'
       CASE('NF')
          sat_name = 'AVHRR09_G'
       CASE('NG')
          sat_name = 'AVHRR10_G'
       CASE('NH')
          sat_name = 'AVHRR11_G'
       CASE('ND')
          sat_name = 'AVHRR12_G'
       CASE('NJ')
          sat_name = 'AVHRR14_G'
       CASE('NK')
          sat_name = 'AVHRR15_G'
       CASE('NL')
          sat_name = 'AVHRR16_G'
       CASE('NM')
          sat_name = 'AVHRR17_G'
       CASE('NN')
          sat_name = 'AVHRR18_G'
       CASE('NP')
          sat_name = 'AVHRR19_G'
       CASE('M2')
          sat_name = 'AVHRRMTA_G'
       CASE('M1')
          sat_name = 'AVHRRMTB_G'
       CASE DEFAULT
          CALL Gbcs_Critical(.TRUE.,'Cannot match satellite ID',&
               'Get_Directory','add_cnts_to_matches.f90')
    END SELECT

    !
    ! Get directory to file - assume at CEMS
    !
    WRITE(Get_Directory,&
         '(''/group_workspaces/cems2/esacci_sst/input/avhrr/l1b/'',a,&
         &''/v1/'',i4.4,''/'',i2.2,''/'',i2.2,''/'')')TRIM(sat_name),&
         year,month,day

  END FUNCTION Get_Directory
  
  FUNCTION GET_UNIQUE_STRING()
    CHARACTER(LEN=30) :: GET_UNIQUE_STRING
    
    INTEGER :: pid
    INTEGER :: clock(8)
    INTEGER :: count
    
    CALL DATE_AND_TIME(VALUES=clock)
    count = clock(8)+clock(7)*1000+clock(6)*60000
    pid = GETPID()
    
    WRITE(GET_UNIQUE_STRING,'(i9.9,''.'',i6.6)')count,pid
    
  END FUNCTION GET_UNIQUE_STRING

  SUBROUTINE Remove_File(dir,filename)

    CHARACTER(LEN=*), INTENT(IN) :: dir
    CHARACTER(LEN=*), INTENT(IN) :: filename
    
    ! Local variables
    INTEGER :: STAT = 0
    CHARACTER(LEN=512) :: command

    command = 'rm -f '//TRIM(dir)//'/'//TRIM(filename)
    CALL SYSTEM(command,STATUS=STAT)
    IF( 0 .ne. STAT )THEN
       CALL Gbcs_Critical(.TRUE.,'Cannot remove temporary file',&
            'Remove_file','Add_Cnts_to_Matches.f90')
    ENDIF

  END SUBROUTINE Remove_File

  INTEGER FUNCTION Get_Month(year,dayno,day)

    INTEGER, INTENT(IN) :: year
    INTEGER, INTENT(IN) :: dayno
    INTEGER, INTENT(OUT) :: day

    ! Local variables
    INTEGER :: I
    INTEGER, PARAMETER :: Day_Num(13) = &
         (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
    INTEGER, PARAMETER :: Day_Num_leap(13) = &
         (/0,31,60,91,121,152,182,213,244,274,305,335,366/)

    IF( Is_Leap(year) )THEN
       Leap_Loop: DO I=13,1,-1
          IF( dayno .gt. Day_Num_Leap(I) )THEN
             Get_Month=I
             EXIT Leap_Loop
          ENDIF
       END DO Leap_Loop
       day = dayno-Day_Num_Leap(Get_Month)
    ELSE
       Loop: DO I=13,1,-1
          IF( dayno .gt. Day_Num(I) )THEN
             Get_Month=I
             EXIT Loop
          ENDIF
       END DO Loop
       day = dayno-Day_Num(Get_Month)
    ENDIF
    RETURN

  END FUNCTION Get_Month

  LOGICAL FUNCTION Is_Leap(Year)

    INTEGER, INTENT(IN) :: Year
    IF(MOD(Year,100).NE.0.AND.MOD(Year,4).EQ.0) THEN
       Is_Leap = .True.
    ELSE IF(MOD(Year,400).EQ.0) THEN
       Is_Leap = .True.
    ELSE
       Is_Leap = .False.
    ENDIF

  END FUNCTION Is_Leap

END MODULE Util
