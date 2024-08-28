!------------------------------------------------------------------------------
!M+
! NAME:
!       GbcsBaseTypes
!
! PURPOSE:
!       Contains the definition of basic GBCS data types
!
! CATEGORY:
!       
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE GbcsBaseTypes
!
! PUBLIC DATA:
!
! MODULES:
!
! CONTAINS:
!
! DERIVED TYPES:
!       Data1D_I8       - Allocatable 1-D array of bytes
!       Data2D_I8       - Allocatable 2-D array of bytes
!       Data3D_I8       - Allocatable 3-D array of bytes
!       Data1D_I16      - Allocatable 1-D array of 2 byte integers
!       Data2D_I16      - Allocatable 2-D array of 2 byte integers
!       Data3D_I16      - Allocatable 3-D array of 2 byte integers
!       Data1D_I32      - Allocatable 1-D array of 4 byte integers
!       Data2D_I32      - Allocatable 2-D array of 4 byte integers
!       Data3D_I32      - Allocatable 3-D array of 4 byte integers
!       Data1D_R32      - Allocatable 1-D array of 4 byte reals
!       Data2D_R32      - Allocatable 2-D array of 4 byte reals
!       Data3D_R32      - Allocatable 3-D array of 4 byte reals
!
!       LineElem        - Line/element pair of integers 
!       LatLon          - Latitude/longitude pair of reals
!       RADec           - Right ascension/declination pair of reals
!
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

MODULE GbcsBaseTypes

  ! ------------
  ! Modules used
  ! ------------
  USE GbcsKinds

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
  IMPLICIT NONE


  TYPE Data1D_I8
    INTEGER(KIND=GbcsInt1), DIMENSION(:), ALLOCATABLE :: d              ! Allocatable 1-D array of 1 byte integers
  END TYPE Data1D_I8

  TYPE Data2D_I8
    INTEGER(KIND=GbcsInt1), DIMENSION(:,:), ALLOCATABLE :: d            ! Allocatable 2-D array of 1 byte integers
  END TYPE Data2D_I8

  TYPE Data3D_I8
    INTEGER(KIND=GbcsInt1), DIMENSION(:,:,:), ALLOCATABLE :: d          ! Allocatable 3-D array of 1 byte integers
  END TYPE Data3D_I8

  TYPE Data1D_I16
    INTEGER(KIND=GbcsInt2), DIMENSION(:), ALLOCATABLE :: d              ! Allocatable 1-D array of 2 byte integers
  END TYPE Data1D_I16

  TYPE Data2D_I16
    INTEGER(KIND=GbcsInt2), DIMENSION(:,:), ALLOCATABLE :: d            ! Allocatable 2-D array of 2 byte integers
  END TYPE Data2D_I16

  TYPE Data3D_I16
    INTEGER(KIND=GbcsInt2), DIMENSION(:,:,:), ALLOCATABLE :: d          ! Allocatable 3-D array of 2 byte integers
  END TYPE Data3D_I16

  TYPE Data1D_I32
    INTEGER, DIMENSION(:), ALLOCATABLE :: d                             ! Allocatable 1-D array of 4 byte integers
  END TYPE Data1D_I32

  TYPE Data2D_I32
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: d                           ! Allocatable 2-D array of 4 byte integers
  END TYPE Data2D_I32

  TYPE Data3D_I32
    INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: d                         ! Allocatable 3-D array of 4 byte integers
  END TYPE Data3D_I32

  TYPE Data1D_R32
    REAL(KIND=GbcsReal), DIMENSION(:), ALLOCATABLE :: d                 ! Allocatable 1-D array of 32 bit reals
  END TYPE Data1D_R32

  TYPE Data2D_R32
    REAL(KIND=GbcsReal), DIMENSION(:,:), ALLOCATABLE :: d               ! Allocatable 2-D array of 32 bit reals
  END TYPE Data2D_R32

  TYPE Data3D_R32
    REAL(KIND=GbcsReal), DIMENSION(:,:,:), ALLOCATABLE :: d             ! Allocatable 3-D array of 32 bit reals
  END TYPE Data3D_R32


  TYPE LineElem
    INTEGER :: line                                                     ! Line number
    INTEGER :: elem                                                     ! Element in line
  END TYPE LineElem

  TYPE WeightMP
    INTEGER             :: elem                                         ! Element in line
    INTEGER             :: line                                         ! Line number
    REAL(KIND=GbcsReal) :: weight                                       ! Weight
  END TYPE WeightMP

  TYPE LatLon
    REAL(KIND=GbcsReal) :: lat                                          ! Latitude
    REAL(KIND=GbcsReal) :: lon                                          ! Longitude
  END TYPE LatLon

  TYPE RADec
    REAL(KIND=GbcsReal) :: RA                                           ! Right ascension
    REAL(KIND=GbcsReal) :: Dec                                          ! Declination
  END TYPE RADec

  TYPE Model_Map_2D
    INTEGER :: LatLo                                                    ! Low latitude of nearest neighbours
    INTEGER :: LatHi                                                    ! High latitude of nearest neihbours
    INTEGER :: LonLo                                                    ! Low longitude of nearest neighbours
    INTEGER :: LonHi                                                    ! High longitude of nearest neighbours
    REAL(Kind=GbcsReal) :: LatWgt                                       ! Latitude weighting to pixel location 
    REAL(Kind=GbcsReal) :: LonWgt                                       ! Longitude weighting to pixel location
  END TYPE Model_Map_2D


  ! -----------------
  ! Module parameters
  ! -----------------
 
  ! -- Module name for error messages
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_NAME = &
    'GbcsMod_Globals/GbcsBaseTypes.f90'

END MODULE GbcsBaseTypes
