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



!+ This module contains Matrix operations

MODULE GbcsMatrixOps

!
! Description:
!
! Method:
!
! Owner: Manager of TRICS Project
!
! History:
! Version  Date       Comment
! -------  ----       -------
! 0.0   04/03/2005    Creation                                                   CPO
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

  CHARACTER(LEN=50), PARAMETER, PRIVATE :: Module_Name = 'GbcsMatrixOps.mod'

CONTAINS

  ! Return determinant of (NxN) matrix A and store

  FUNCTION Determ( A , N , det_error )

  !
  ! Description:
  !
  ! Method:
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

    IMPLICIT NONE

    REAL(KIND=GbcsDble) :: Determ

    INTEGER                             :: N
    REAL(KIND=GbcsDble), DIMENSION(:,:) :: A
    INTEGER                             :: det_error

    REAL(KIND=GbcsDble), DIMENSION(N,N) :: AA
    INTEGER, DIMENSION(N) :: INDX
    REAL(KIND=GbcsDble) :: D

    INTEGER :: ii

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Determ'


      det_error = 0

      AA = A

      CALL LUdcmp( AA , N , INDX , D , det_error )

      IF ( det_error == 0 ) THEN

        DO ii = 1,N

          D = D * AA(ii,ii)

        END DO

        Determ = D

      END IF

    RETURN

  END FUNCTION Determ


  PURE SUBROUTINE Invert2x2(A, B, Det, inv_error)

    IMPLICIT NONE

    REAL(KIND=GbcsDble), DIMENSION(2,2), INTENT (IN   ) :: A
    REAL(KIND=GbcsDble), DIMENSION(2,2), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER,                             INTENT (  OUT) :: inv_error

    REAL(KIND=GbcsDble) :: inv_Det

      inv_error = 0

      Det = A(1,1)*A(2,2) - A(2,1)*A(1,2)
      IF (Det == 0) THEN
        inv_error = 1
        RETURN
      END IF

      inv_det = 1.0 / Det

      B(1,1) =  A(2,2) * inv_det
      B(1,2) = -A(1,2) * inv_det
      B(2,1) = -A(2,1) * inv_det
      B(2,2) =  A(1,1) * inv_det

    RETURN

  END SUBROUTINE Invert2x2


  PURE SUBROUTINE Invert3x3(A, B, Det, inv_error)

    IMPLICIT NONE

    REAL(KIND=GbcsDble), DIMENSION(3,3), INTENT (IN   ) :: A
    REAL(KIND=GbcsDble), DIMENSION(3,3), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER,                             INTENT (  OUT) :: inv_error

    REAL(KIND=GbcsDble) :: inv_Det
    REAL(KIND=GbcsDble), DIMENSION(6) :: temp

      inv_error = 0

  ! -- Note Assumes matrix is symmetric

      temp(1) = A(2,2)*A(3,3) - A(2,3)*A(3,2)
      temp(2) = A(2,3)*A(3,1) - A(2,1)*A(3,3)
      temp(3) = A(2,1)*A(3,2) - A(2,2)*A(3,1)
      temp(4) = A(1,1)*A(3,3) - A(1,3)*A(3,1)
      temp(5) = A(1,2)*A(3,1) - A(1,1)*A(3,2)
      temp(6) = A(1,1)*A(2,2) - A(1,2)*A(2,1)

      Det =  A(1,1)*temp(1) + A(1,2)*temp(2) + A(1,3)*temp(3)

      IF (Det == 0) THEN
        inv_error = 1
        RETURN
      END IF

      inv_det = 1.0 / Det

      B(1,1) = temp(1) * inv_det
      B(2,1) = temp(2) * inv_det
      B(3,1) = temp(3) * inv_det

      B(1,2) = temp(2) * inv_det
      B(2,2) = temp(4) * inv_det
      B(3,2) = temp(5) * inv_det

      B(1,3) = temp(3) * inv_det
      B(2,3) = temp(5) * inv_det
      B(3,3) = temp(6) * inv_det

    RETURN

  END SUBROUTINE Invert3x3

  PURE SUBROUTINE Invert6x6(A, B, Det, inv_error)

    IMPLICIT NONE


    REAL(KIND=GbcsDble), DIMENSION(6,6), INTENT (IN   ) :: A
    REAL(KIND=GbcsDble), DIMENSION(6,6), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER,                             INTENT (  OUT) :: inv_error

    REAL(KIND=GbcsDble), DIMENSION(3,3) :: P, Q, R, inv_p, alpha, beta, gamma
    REAL(KIND=GbcsDble)                 :: det_P, det_beta

      Q = A(1:3,4:6)

      inv_error = 0

      P = A(1:3,1:3)
      CALL Invert3x3(P, inv_P, det_P, inv_error)

      alpha = MATMUL(inv_P, Q)
      R = A(4:6,4:6)-MATMUL(TRANSPOSE(Q), alpha)
      CALL Invert3x3(R, beta, det_beta, inv_error)

      gamma = MATMUL(beta, TRANSPOSE(alpha))

      B(1:3,1:3) = inv_P + MATMUL(alpha, gamma)
      B(1:3,4:6) = -TRANSPOSE(gamma)
      B(4:6,1:3) = -gamma
      B(4:6,4:6) = beta

      Det = det_P * det_beta

    RETURN

  END SUBROUTINE Invert6x6


  PURE SUBROUTINE Invert4x4(A, B, Det, inv_error)

    IMPLICIT NONE


    REAL(KIND=GbcsDble), DIMENSION(4,4), INTENT (IN   ) :: A
    REAL(KIND=GbcsDble), DIMENSION(4,4), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER,                             INTENT (  OUT) :: inv_error

    REAL(KIND=GbcsDble), DIMENSION(2,2) :: P, Q, R, inv_p, alpha, beta, gamma
    REAL(KIND=GbcsDble)                 :: det_P, det_beta

      inv_error = 0

      P = A(1:2,1:2)
      CALL Invert2x2(P, inv_P, det_P, inv_error)

      Q = A(1:2,3:4)

      alpha = MATMUL(inv_P, Q)
      R = A(3:4,3:4)-MATMUL(TRANSPOSE(Q), alpha)
      CALL Invert2x2(R, beta, det_beta, inv_error)

      gamma = MATMUL(beta, TRANSPOSE(alpha))

      B(1:2,1:2) = inv_P + MATMUL(alpha, gamma)
      B(1:2,3:4) = -TRANSPOSE(gamma)
      B(3:4,1:2) = -gamma
      B(3:4,3:4) = beta

      Det = det_P * det_beta
    
    RETURN

  END SUBROUTINE Invert4x4

  PURE SUBROUTINE Invert_Diag( A , N , B , Det , inv_error )

    IMPLICIT NONE

    REAL(KIND=GbcsDble), DIMENSION(N,N), INTENT (IN   ) :: A
    INTEGER,                             INTENT (IN   ) :: N
    REAL(KIND=GbcsDble), DIMENSION(N,N), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER                            , INTENT (  OUT) :: inv_error

    INTEGER :: ii

      inv_error = 0

      B(:,:) = 0.0_GbcsDble
      Det    = 1.0_GbcsDble 
      DO ii = 1,N
        B(ii,ii) = 1.0_GbcsDble / A(ii,ii)
        Det      = Det          * A(ii,ii)
      END DO

    RETURN

  END SUBROUTINE Invert_Diag
  
  ! Find inverse of (NxN) matrix A and store in B

  PURE SUBROUTINE InvertNxN( A , N , B , Det , inv_error )

  !
  ! Description:
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   24/01/2005    Creation                                                   CPO
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

    IMPLICIT NONE


    REAL(KIND=GbcsDble), DIMENSION(N,N), INTENT (IN   ) :: A
    INTEGER,                             INTENT (IN   ) :: N
    REAL(KIND=GbcsDble), DIMENSION(N,N), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER                            , INTENT (  OUT) :: inv_error

    INTEGER :: ii,jj
    INTEGER :: INDX(N)
    REAL(KIND=GbcsDble), DIMENSION(N) :: COL
    REAL(KIND=GbcsDble), DIMENSION(N,N) :: AA
    REAL(KIND=GbcsDble) :: D

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'InvertNxN'


      inv_error = 0

      AA = A

      B = 0.0_GbcsDble
      DO ii = 1,N
        B(ii,ii) = 1.0_GbcsDble
      END DO

      CALL LUdcmp( AA , N , INDX , D , inv_error )

      IF ( inv_error == 0 ) THEN

        Det = D

        DO jj = 1,N

          COL = B(:,jj)
          CALL LUbksb(AA,N,INDX,COL)
          B(:,jj) = COL     
          
          Det = Det * AA(jj,jj)

        END DO

      END IF

    RETURN

  END SUBROUTINE InvertNxN

  ! Find inverse of (NxN) matrix A and store in B

  PURE SUBROUTINE Invert( A , N , B , Det , inv_error )

  !
  ! Description:
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   24/01/2005    Creation                                                   CPO
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

    IMPLICIT NONE


    REAL(KIND=GbcsDble), DIMENSION(N,N), INTENT (IN   ) :: A
    INTEGER,                             INTENT (IN   ) :: N
    REAL(KIND=GbcsDble), DIMENSION(N,N), INTENT (  OUT) :: B
    REAL(KIND=GbcsDble),                 INTENT (  OUT) :: Det
    INTEGER                            , INTENT (  OUT) :: inv_error


    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'Invert'

    SELECT CASE (N)
      CASE (2)
        CALL Invert2x2(A, B, Det, inv_error)
      CASE (3)
        CALL Invert3x3(A, B, Det, inv_error)
      CASE (4)
        CALL Invert4x4(A, B, Det, inv_error)
      CASE (6)
        CALL Invert6x6(A, B, Det, inv_error)
      CASE DEFAULT
        CALL InvertNxN(A, N, B, Det, inv_error)
    END SELECT

  END SUBROUTINE Invert
  ! Calculates the LU decomposition of a Rank 2 n-dimensional matrix

  PURE SUBROUTINE LUdcmp( A , N , INDX , D , lud_error )

  !
  ! Description:
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   24/01/2005    Creation                                                   CPO
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

    IMPLICIT NONE

    REAL(KIND=GbcsDble), DIMENSION(:,:), INTENT(INOUT) :: A
    INTEGER,                             INTENT(IN)    :: N
    INTEGER,             DIMENSION(:),   INTENT(OUT)   :: INDX
    REAL(KIND=GbcsDble),                 INTENT(OUT)   :: D
    INTEGER,                             INTENT(OUT)   :: lud_error


    INTEGER :: ii,jj,kk
    INTEGER, PARAMETER :: NMAX=500
    REAL(KIND=GbcsDble), PARAMETER :: TINY=1.0e-200_GbcsDble
    INTEGER :: MAX_II
    REAL(KIND=GbcsDble) :: MAX_ELEM
    REAL(KIND=GbcsDble) :: DUMMY
    REAL(KIND=GbcsDble) :: TOTAL
    REAL(KIND=GbcsDble) :: SCALE_FACTOR(NMAX)

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'LUdcmp'

      lud_error = 0

      D = 1.0_GbcsDble

      DO ii = 1,N

        MAX_ELEM = 0.0_GbcsDble

        DO jj = 1,N

          IF ( DABS( A(ii,jj) ) > MAX_ELEM ) MAX_ELEM = DABS( A(ii,jj) )

        END DO

        IF ( MAX_ELEM == 0.0_GbcsDble ) THEN
          lud_error = 1
          RETURN
        END IF

        SCALE_FACTOR(ii) = 1.0_GbcsDble / MAX_ELEM

      END DO

      DO jj = 1,N

        DO ii = 1,jj-1

          TOTAL = A(ii,jj)

          DO kk = 1,ii-1

            TOTAL = TOTAL - ( A(ii,kk) * A(kk,jj) )

          END DO

          A(ii,jj) = TOTAL

        END DO

        MAX_ELEM = 0.0_GbcsDble

        DO ii = jj,N

          TOTAL = A(ii,jj)

          DO kk = 1,jj-1

            TOTAL = TOTAL - ( A(ii,kk) * A(kk,jj) )

          END DO

          A(ii,jj) = TOTAL

          DUMMY = SCALE_FACTOR(ii) * DABS( TOTAL )

          IF ( DUMMY >= MAX_ELEM ) THEN

            MAX_II = ii
            MAX_ELEM = DUMMY

          END IF

        END DO

        IF ( jj /= MAX_II ) THEN

          DO kk = 1,N

            DUMMY = A(MAX_II,kk)
            A(MAX_II,kk) = A(jj,kk)
            A(jj,kk) = DUMMY

          END DO

          D = -D
          SCALE_FACTOR(MAX_II) = SCALE_FACTOR(jj)

        END IF

        INDX(jj) = MAX_II

        IF ( A(jj,jj) == 0.0_GbcsDble ) A(jj,jj) = TINY

        IF ( jj /= N ) THEN

          DUMMY = 1.0_GbcsDble / A(jj,jj)

          DO ii = jj+1,N

            A(ii,jj) = A(ii,jj) * DUMMY

          END DO

        END IF

      END DO

    RETURN

  END SUBROUTINE LUdcmp


  ! Solves A.X=B for X using the LU back-substitution

  PURE SUBROUTINE LUbksb( A , N , INDX , B )

  !
  ! Description:
  !
  ! Method:
  !
  ! Owner: Manager of TRICS Project
  !
  ! History:
  ! Version  Date       Comment
  ! -------  ----       -------
  ! 0.0   24/01/2005    Creation                                                   CPO
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

    IMPLICIT NONE

    REAL(KIND=GbcsDble), DIMENSION(:,:),INTENT(IN)    :: A
    INTEGER,                            INTENT(IN)    :: N
    INTEGER,             DIMENSION(:),  INTENT(IN)    :: INDX
    REAL(KIND=GbcsDble), DIMENSION(:),  INTENT(INOUT) :: B

    INTEGER :: ii,jj,kk
    INTEGER :: NON_VANISH
    REAL(KIND=GbcsDble) :: TOTAL

    CHARACTER(LEN=50), PARAMETER :: Routine_Name = 'LUbksb'


      NON_VANISH = 0

      DO ii = 1,N

        kk = INDX(ii)

        TOTAL = B(kk)

        B(kk) = B(ii)

        IF ( NON_VANISH /= 0 ) THEN

          DO jj = NON_VANISH,ii-1

            TOTAL = TOTAL - ( A(ii,jj) * B(jj) )

          END DO

        ELSE IF ( TOTAL /= 0.0_GbcsDble ) THEN

          NON_VANISH = ii

        END IF

        B(ii) = TOTAL

      END DO

      DO ii = N,1,-1

        TOTAL = B(ii)

        DO jj = ii+1,N

          TOTAL = TOTAL - ( A(ii,jj) * B(jj) )

        END DO

        B(ii) = TOTAL / A(ii,ii)

      END DO

    RETURN

  END SUBROUTINE LUbksb


!------------------------------------------------------------------------------
!F+
! NAME:
!       Calc_S_Matrix
!
! PURPOSE:
!       Calculates the covariance matrix: S = transpose(H).B.H
!       Where B is a symmetric matrix
!
! CATEGORY:
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       s = Calc_S_Matrix( H, B )
!
! INPUT ARGUMENTS:
!       H: n x m matrix
!       B: n x n symmetrix matrix
!
! FUNCTION RESULT:
!       s: m x m covariance matrix
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
!       Written by:   Owen Embury 28/08/2012
!                     IAES, University of Edinburgh
!F-
!------------------------------------------------------------------------------
  PURE FUNCTION Calc_S_Matrix(h,b) RESULT(s)
    IMPLICIT NONE
    ! ---------
    ! Arguments
    ! ---------
    REAL(KIND=GbcsDble), DIMENSION(:,:), INTENT(IN) :: H
    REAL(KIND=GbcsDble), DIMENSION(:,:), INTENT(IN) :: B
    
    ! ---------
    ! Function result
    ! ---------
    REAL(KIND=GbcsDble), DIMENSION(SIZE(H,2),SIZE(H,2)) :: S

    ! ---------------
    ! Local variables
    ! ---------------
    REAL(KIND=GbcsDble), DIMENSION(SIZE(H,1)) :: vec
    INTEGER :: i,j
      
    DO i=1,SIZE(H,2)
      DO j=1,SIZE(H,1)
        vec(j) = DOT_PRODUCT(H(:,i), B(:,j))
      END DO
      DO j=i,SIZE(H,2)
        S(j,i) = DOT_PRODUCT(vec,H(:,j))
      END DO
      S(i,i+1:) = S(i+1:,i)
    END DO
    
  END FUNCTION Calc_S_Matrix

END MODULE GbcsMatrixOps
