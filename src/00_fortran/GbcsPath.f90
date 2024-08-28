!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsPath
!
! PURPOSE:
!       Manipulate pathnames
!       
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsPath
!
! PUBLIC DATA:
!       None
!
! MODULES:
!       None
!
! CONTAINS:
!       Path_Join
!       Basename
!       Dirname
!       File_Exists
!
! DERIVED TYPES:
!       None
!
! NOTES:
!
! CREATION HISTORY:
!       Written by:   Owen Embury 11/10/2011
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
!
!M-
!------------------------------------------------------------------------------
MODULE GbcsPath
  ! ------------
  ! Modules used
  ! ------------

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: Path_Join, Basename, Dirname, File_Exists
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Misc/GbcsPath.f90'

CONTAINS
!------------------------------------------------------------------------------
!F+
! NAME:
!       Path_Join
!
! PURPOSE:
!       Join two path segments together - e.g. path + '/' + filename
!       If the first path is empty, just returns second part
!       If second part is absolute (starts with '/') then ignores first
!       
! CATEGORY:
!       File Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       fullpath = Path_Join(path1, path2)
!
! INPUT ARGUMENTS:
!       path1, path2: CHARACTER(*)
!
! FUNCTION RESULT:
!       fullpath: two input paths joined using the path separator '/'
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
!       Written by:   Owen Embury 11/10/2011
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
   PURE FUNCTION Path_Join(path1, path2) RESULT(path)
    IMPLICIT NONE
  
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: path1
    CHARACTER(LEN=*), INTENT(IN) :: path2

    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=Path_Join_Calc_Length(path1,path2)) :: path
    
    ! ---------------
    ! Local variables
    ! ---------------
    INTEGER :: length
    
    length = LEN_TRIM(path1)

    IF (length==0 .OR. INDEX(path2,'/')==1) THEN
      path = path2
      RETURN
    END IF

    IF (length == INDEX(path1,'/',.TRUE.)) THEN
      path = TRIM(path1) // TRIM(path2)
    ELSE
      path = TRIM(path1) // "/" // TRIM(path2)
    END IF
  
  END FUNCTION Path_Join

  ELEMENTAL FUNCTION Path_Join_Calc_Length(path1, path2) RESULT(length)
    IMPLICIT NONE
  
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: path1
    CHARACTER(LEN=*), INTENT(IN) :: path2

    ! ---------
    ! Function result
    ! ---------
    INTEGER :: length
    
    length = LEN_TRIM(path1)
    
    IF (length==0 .OR. INDEX(path2,'/')==1) THEN
      length = LEN_TRIM(path2)
      RETURN
    END IF
    
    IF (length /= INDEX(path1,'/',.TRUE.)) THEN
      length = length + 1
    END IF
    
    length = length + LEN_TRIM(path2)
    
  END FUNCTION Path_Join_Calc_Length


!------------------------------------------------------------------------------
!F+
! NAME:
!       Basename
!
! PURPOSE:
!       Strip the directory from a path returning just the file name
!       
! CATEGORY:
!       File Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       name = Basename(path)
!
! INPUT ARGUMENTS:
!       path: CHARACTER(*)
!
! FUNCTION RESULT:
!       name: the file name, i.e. text after the final path separator '/'
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
!       Written by:   Owen Embury 27/08/2012
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
   PURE FUNCTION Basename(path) RESULT(name)
    IMPLICIT NONE
  
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: path

    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=LEN_TRIM(path)-INDEX(path,'/',.TRUE.)) :: name
    
    name = path(1+INDEX(path,'/',.TRUE.):LEN_TRIM(path))
    
  END FUNCTION Basename
  
  
!------------------------------------------------------------------------------
!F+
! NAME:
!       Dirname
!
! PURPOSE:
!       Strip the non-directory suffix from a path returning just the directory
!       
! CATEGORY:
!       File Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       dir = Dirname(path)
!
! INPUT ARGUMENTS:
!       path: CHARACTER(*)
!
! FUNCTION RESULT:
!       dir: the directory, i.e. text before the final path separator '/'
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
!       Written by:   Owen Embury 27/08/2012
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
   PURE FUNCTION Dirname(path) RESULT(dir)
    IMPLICIT NONE
  
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER(LEN=*), INTENT(IN) :: path

    ! ---------
    ! Function result
    ! ---------
    CHARACTER(LEN=INDEX(path,'/',.TRUE.)-1) :: dir
    
    dir = path(1:INDEX(path,'/',.TRUE.)-1)
    
  END FUNCTION Dirname


!------------------------------------------------------------------------------
!F+
! NAME:
!       File_Exists
!
! PURPOSE:
!       Check if the specified file exists
!
! CATEGORY:
!       File Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       stat = File_Exists( Filename )
!
! INPUT ARGUMENTS:
!       Filename:   CHARACTER(*)
!
! FUNCTION RESULT:
!       .TRUE. if the file exists
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
!       This function is just a wrapper around the INQUIRE intrinsic.
!
! CREATION HISTORY:
!       16/09/13  OE  Moved function to GbcsPath module
!       Written by:   Owen Embury 09/07/2007
!                     IAES, University of Edinburgh
!
!F-
!------------------------------------------------------------------------------
  FUNCTION File_Exists( Filename )
    IMPLICIT NONE
    
    ! ---------
    ! Arguments
    ! ---------
    CHARACTER( * ), INTENT(IN) :: Filename
    
    ! ---------
    ! Function result
    ! ---------
    LOGICAL                    :: File_Exists

    ! ----------------
    ! Local parameters
    ! ----------------
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'File_Exists'
    
    INQUIRE(FILE=Filename, EXIST=File_Exists)
    
  END FUNCTION File_Exists  
END MODULE GbcsPath
