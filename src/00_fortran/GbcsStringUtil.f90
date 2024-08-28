!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsStringUtil
!
! PURPOSE:
!       Funtions for generating string representations of various data types
!
! NOTES:
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsStringUtil
!
! MODULES:
!       
!
! CONTAINS:
!       Upper_Case
!       Lower_Case
!       Compress_Blanks
!       Count_Tokens
!       Extract_Token
!       Split
!       STR
!
! DERIVED TYPES:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 21/07/2008
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
!M-
!----------------------------------------------------------------------------------
MODULE GbcsStringUtil
  ! ------------
  ! Modules used
  ! ------------

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE

  INTERFACE STR
    MODULE PROCEDURE VarStr, FixedStr
  END INTERFACE STR
  
  PRIVATE :: STRLEN, VarStr, FixedStr
  ! -----------------
  ! Module parameters
  ! -----------------
  
  CHARACTER(LEN=26), PARAMETER :: ascii_lowercase = 'abcdefghijklmnopqrstuvwxyz'
  CHARACTER(LEN=26), PARAMETER :: ascii_uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  CHARACTER(LEN=10), PARAMETER :: digits          = '0123456789'
  CHARACTER(LEN=*),  PARAMETER :: alpha_numeric   = ascii_lowercase // ascii_uppercase // digits
  
  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Misc/GbcsStringUtil.f90'

CONTAINS


!------------------------------------------------------------------------------
!F+
! NAME:
!       Upper_Case
!
! PURPOSE:
!       Convert string to upper case
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Upper_Case( string )
!
! INPUT ARGUMENTS:
!       string
!
! OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       string converted to upper case
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       Assumes ASCII character set is being used.
!
! PROCEDURE:
!
! CREATION HISTORY:
!       22/03/11  OE  Creation
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Upper_Case(string)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: string
    
    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=LEN(string))   :: Upper_Case
    
    ! ----------------
    ! Local parameters
    ! ----------------
    INTEGER,   PARAMETER :: offset = IACHAR('A') - IACHAR('a')
    INTEGER,   PARAMETER :: c1 = IACHAR('a')
    INTEGER,   PARAMETER :: c2 = IACHAR('z')
  
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i, ch
    
    DO i=1,LEN(string)
      ch = IACHAR(string(i:i))
      IF (ch >= c1 .AND. ch <=c2) ch = ch + offset
      Upper_Case(i:i) = ACHAR(ch)
    END DO
    
  END FUNCTION Upper_Case


!------------------------------------------------------------------------------
!F+
! NAME:
!       Lower_Case
!
! PURPOSE:
!       Convert string to lower case
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Lower_Case( string )
!
! INPUT ARGUMENTS:
!       string
!
! OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       string converted to lower case
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       Assumes ASCII character set is being used.
!
! PROCEDURE:
!
! CREATION HISTORY:
!       22/03/11  OE  Creation
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Lower_Case(string)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: string
    
    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=LEN(string))   :: Lower_Case
    
    ! ----------------
    ! Local parameters
    ! ----------------
    INTEGER,   PARAMETER :: offset = IACHAR('a') - IACHAR('A')
    INTEGER,   PARAMETER :: c1 = IACHAR('A')
    INTEGER,   PARAMETER :: c2 = IACHAR('Z')
  
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i, ch
    
    DO i=1,LEN(string)
      ch = IACHAR(string(i:i))
      IF (ch >= c1 .AND. ch <=c2) ch = ch + offset
      Lower_Case(i:i) = ACHAR(ch)
    END DO
    
  END FUNCTION Lower_Case


!------------------------------------------------------------------------------
!S+
! NAME:
!       Compress_Blanks
!
! PURPOSE:
!       Compress white space in string to a single character between words.
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Compress_Blanks( string )
!
! INPUT ARGUMENTS:
!       string
!
! OUTPUT ARGUMENTS
!
! CALLS:
!       None
!
! SIDE EFFECTS:
!       Argument string is modified
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!
! CREATION HISTORY:
!       22/03/11  OE  Creation
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Compress_Blanks(string)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(INOUT) :: string
      
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i1, i2
    LOGICAL :: flag

    i2   = 1
    flag = .False.
    DO i1=1, LEN(string)
      IF (string(i1:i1) /= ' ') THEN
        string(i2:i2) = string(i1:i1)
        i2=i2+1
        flag = .True.
      ELSE IF (flag) THEN
        string(i2:i2) = ' '
        i2=i2+1
        flag = .False.
      END IF
    END DO
    IF (i2 < LEN(string)) string(i2:) = ''
  END SUBROUTINE Compress_Blanks


!------------------------------------------------------------------------------
!F+
! NAME:
!       Count_Tokens
!
! PURPOSE:
!       Count the number of tokens/words in a string. Assumes that tokens are
!       separated by a single space character.
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Count_Tokens( string )
!
! INPUT ARGUMENTS:
!       string
!
! OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       number of tokens in input string
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
!       22/03/11  OE  Creation
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Count_Tokens(string)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: string
      
    ! ---------
    ! Function result
    ! ---------
    INTEGER :: Count_Tokens
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i1, i2

    Count_Tokens = 0
    i1 = 1
    DO
      i2 = INDEX(string(i1:), ' ')
      IF (i2 <= 1) EXIT
      Count_Tokens = Count_Tokens + 1
      i1 = i1 + i2
    END DO
  END FUNCTION Count_Tokens


!------------------------------------------------------------------------------
!F+
! NAME:
!       Extract_Token
!
! PURPOSE:
!       Extract the requested token from the input string. Assumes tokens are
!       separated by a single space character.
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = Extract_Token( string, ind )
!
! INPUT ARGUMENTS:
!       string
!       ind
!
! OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       token at index ind of string.
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
!       22/03/11  OE  Creation
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Extract_Token(string, ind)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: string
    INTEGER,          INTENT(IN) :: ind
      
    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=LEN(string))   :: Extract_Token
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i1, i2
    INTEGER :: Count_Tokens

    Count_Tokens = 0
    i1 = 1
    DO
      i2 = INDEX(string(i1:), ' ')
      IF (i2 <= 1) EXIT
      Count_Tokens = Count_Tokens + 1
      IF (Count_Tokens == ind) EXIT
      i1 = i1 + i2
    END DO
    
    Extract_Token = string(i1:i1+i2-2)
  END FUNCTION Extract_Token


!------------------------------------------------------------------------------
!S+
! NAME:
!       Split
!
! PURPOSE:
!       Split a string into tokens/words. Similar to Extract_Token, but as a
!       subroutine which returns all tokens
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Split( string, tokens, [ntoken] )
!
! INPUT ARGUMENTS:
!       string
!
! OUTPUT ARGUMENTS
!       tokens - array to hold string words
!
! OPTIONAL OUTPUT ARGUMENTS
!       ntoken - number of tokens found
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
!       22/03/11  OE  Creation
!
!S-
!------------------------------------------------------------------------------
  SUBROUTINE Split(string, tokens, ntoken)
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*),               INTENT(IN)  :: string
    CHARACTER(LEN=*), DIMENSION(:), INTENT(OUT) :: tokens
    INTEGER,          OPTIONAL,     INTENT(OUT) :: ntoken
      
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: i1, i2
    INTEGER :: max_tokens, num_tokens
    LOGICAL :: flag

    i1   = 1
    flag = .False.
    
    max_tokens = SIZE(tokens)
    num_tokens = 0
    tokens(:) = ' '
    
    DO i2=1, LEN(string)
      IF (string(i2:i2) /= ' ') THEN
        IF (.NOT. flag) THEN
          i1   = i2
          flag = .TRUE.
          num_tokens = num_tokens + 1
        END IF
      ELSE IF (flag) THEN
        IF (num_tokens <= max_tokens) tokens(num_tokens) = string(i1:i2-1)
        flag = .False.
      END IF
    END DO
    
    IF (PRESENT(ntoken)) ntoken = num_tokens
    
  END SUBROUTINE Split


!------------------------------------------------------------------------------
!F+
! NAME:
!       STR
!
! PURPOSE:
!       Convert integer to string
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       result = STR( value, length )
!
! INPUT ARGUMENTS:
!       value
!
! OPTIONAL INPUT ARGUMENTS:
!       length
!
! OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       string
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
!       21/07/08  OE  Creation
!F-
!------------------------------------------------------------------------------
  PURE FUNCTION STRLEN( value ) &
           RESULT( length )
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: value
    INTEGER             :: length
    INTEGER             :: temp
    length = 1
    IF ( value < 0 ) THEN
      length = length + 1
    END IF
    
    temp = ABS(value)
    DO WHILE (temp >= 10)
      temp = temp / 10
      length = length + 1
    END DO
    
  END FUNCTION STRLEN


  PURE FUNCTION VarStr( value ) &
        RESULT( string )
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: value
    CHARACTER(STRLEN(value)) :: string

    INTEGER :: temp, i
    
    INTEGER,  PARAMETER :: zchar = ICHAR('0')
    
    temp = ABS(value)
    DO i=1,LEN(string)
      string = CHAR( MOD(temp, 10) + zchar ) // string
      temp = temp / 10
    END DO

    IF (value < 0 ) THEN
      string = '-' // string(2:)
    END IF
    
  END FUNCTION VarStr


  PURE FUNCTION FixedStr( value, slen ) &
        RESULT( string )
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: value
    INTEGER, INTENT(IN) :: slen
    CHARACTER(slen) :: string

    INTEGER :: temp, i
    
    INTEGER,  PARAMETER :: zchar = ICHAR('0')
    
    temp = ABS(value)
    DO i=1,slen
      string = CHAR( MOD(temp, 10) + zchar ) // string
      temp = temp / 10
    END DO

    IF (value < 0 ) THEN
      string = '-' // string(2:)
    END IF
    
  END FUNCTION FixedStr


!------------------------------------------------------------------------------
!F+
! NAME:
!       Count
!
! PURPOSE:
!       Count the number of non-overlapping occurrences of substring sub in
!       in the string
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       n = Count(string, sub)
!
! INPUT ARGUMENTS:
!       string
!       sub
!
! FUNCTION RESULT:
!       Number of occurrences of substring sub in string
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
!       Written by:   Owen Embury 03/09/2012
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Count_Substr( string, sub ) RESULT(count)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: string
    CHARACTER(LEN=*), INTENT(IN) :: sub
    
    ! ---------
    ! Function result
    ! ---------
    INTEGER :: Count
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: start, offset
    
    Count = 0
    start = 1
    
    DO
      offset = INDEX(string(start:), sub)
      IF (offset==0) EXIT
      start = start + offset
      count = count + 1
    END DO
    
   END FUNCTION Count_Substr


!------------------------------------------------------------------------------
!F+
! NAME:
!       Replace
!
! PURPOSE:
!       Return a copy of the string with all occurrences of substring old
!       replaced with substring new.
!
! CATEGORY:
!       String Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       newstr = Replace(string, old, new)
!
! INPUT ARGUMENTS:
!       string
!       old
!       new
!
! FUNCTION RESULT:
!       New string with all occurrences of old replaced with new
!
! CALLS:
!       Count
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
!       Written by:   Owen Embury 03/09/2012
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Replace( string, old, new )
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: string
    CHARACTER(LEN=*), INTENT(IN) :: old
    CHARACTER(LEN=*), INTENT(IN) :: new
    
    ! ---------
    ! Function result
    ! ---------
    ! -- This does not work in gfortran v4.4
    ! CHARACTER(LEN=LEN(string)+Count(string,old)*(LEN(new)-LEN(old))) :: Replace
    CHARACTER(LEN=LEN(string)+80) :: Replace
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: s1, s2
    INTEGER :: offset
    
    s1 = 1
    s2 = 1
    
    DO
      offset = INDEX(string(s1:), old) - 1
      IF (offset >= 0) THEN
        replace(s2:s2+offset+LEN(new)) = string(s1:s1+offset-1) // new
        s1 = s1 + offset + LEN(old)
        s2 = s2 + offset + LEN(new)
      ELSE
        replace(s2:) = string(s1:)
        EXIT
      END IF
    END DO
    
  END FUNCTION Replace
END MODULE GbcsStringUtil
