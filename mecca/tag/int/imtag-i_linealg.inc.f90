  ! - some linear algebra routines --------------------------------------------

    SUBROUTINE inverse(M,n)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n
      REAL(dp), INTENT(INOUT) :: M(n,n)

      INTEGER  :: indx(n), i
      REAL(dp) :: E(n,n), d

      E(:,:)=0.0_dp
      DO i = 1, n                    ! setting up identity matrix
         E(i,i)=1.0_dp
      ENDDO

      CALL ludcmp(M,indx,d,n)        ! decomposing matrix just once
      DO i = 1, n                    ! finding inverse by columns
         CALL lubksb(M,indx,E(:,i),n)
!                           E(1,i)
      ENDDO

      M = E

    END SUBROUTINE inverse

  ! ---------------------------------------------------------------------------

    SUBROUTINE ludcmp(a,indx,d,n)
  
      IMPLICIT NONE
  
      INTEGER, INTENT(IN) :: n
      REAL(dp), INTENT(INOUT) :: a(n,n)
  
      REAL(dp), INTENT(OUT)   :: d
      INTEGER, INTENT(OUT)    :: indx(n)
  
      REAL(dp), PARAMETER :: TINY = 1.0E-20_dp      ! a small number.
  
    ! Given a matrix a(1:n,1:n), with physical dimension np by np, this routine replaces it by
    ! the LU decomposition of a rowwise permutation of itself. a and n are input. a is output,
    ! arranged as in equation (2.3.14) above; indx(1:n) is an output vector that records the
    ! row permutation effected by the partial pivoting; d is output as +-1 depending on whether
    ! the number of row interchanges was even or odd, respectively. This routine is used in
    ! combination with lubksb to solve linear equations or invert a matrix.
  
      INTEGER  :: i, imax, j, k
      REAL(dp) :: aamax, dum, sum, &
                  vv(n) ! vv stores the implicit scaling of each row.
  
      d=1.0_dp                 ! No row interchanges yet.
  
      DO i=1,n                 ! Loop over rows to get the implicit scaling information
        aamax=0.0_dp
        DO j=1,n
          IF (ABS(a(i,j)) .GT. aamax) aamax=ABS(a(i,j))
        ENDDO
      IF (aamax .EQ.0.0_dp ) pause 'singular matrix in ludcmp' ! No nonzero largest element.
      vv(i)=1.0_dp /aamax              ! Save the scaling.
      ENDDO
  
      DO j=1,n                 ! This is the loop over columns of Crout's method.
        DO i=1,j-1             ! This is equation (2.3.12) except for i = j.
          sum=a(i,j)
          DO k=1,i-1
            sum=sum-a(i,k)*a(k,j)
          ENDDO
          a(i,j)=sum
        ENDDO
        aamax=0.0_dp ! Initialize for the search for largest pivot element.
        DO i=j,n               ! This is i = j of equation (2.3.12) and i = j+1: ::N
          sum=a(i,j)              ! of equation (2.3.13).
          DO k=1,j-1
            sum=sum-a(i,k)*a(k,j)
          ENDDO
          a(i,j)=sum
          dum=vv(i)*ABS(sum)       ! Figure of merit for the pivot.
          IF (dum .GE. aamax) THEN ! Is it better than the best so far?
            imax=i
            aamax=dum
          ENDIF
        ENDDO
        IF (j .NE. imax) THEN     ! DO we need to interchange rows?
          DO k=1,n             ! Yes, DO so...
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
          ENDDO
          d=-d                    ! ...and change the parity of d.
          vv(imax)=vv(j)          ! Also interchange the scale factor.
        ENDIF
        indx(j)=imax
        IF (a(j,j) .EQ.0.0_dp ) a(j,j)=TINY
    ! IF the pivot element is zero the matrix is singular 
    ! (at least to the precision of the algorithm).
    ! For some applications on singular matrices, 
    ! it is desirable to substitute TINY for zero.
        IF (j .NE. n) THEN        ! Now, finally, divide by the pivot element.
          dum=1.0_dp /a(j,j)
          DO i=j+1,n
            a(i,j)=a(i,j)*dum
          ENDDO
        ENDIF
      ENDDO                    ! Go back for the next column in the reduction.
      return
    END SUBROUTINE ludcmp
  
    ! --------------------------------------------------------------------------
  
    SUBROUTINE lubksb(a,indx,b,n)
    
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n
      REAL(dp), INTENT(IN) :: a(n,n)
      INTEGER,  INTENT(IN) :: indx(n)
      REAL(dp), INTENT(INOUT) :: b(n)
  
    ! Solves the set of n linear equations A * X = B. Here a is input, not as the matrix A but
    ! rather as its LU decomposition, determined by the routine ludcmp. indx is input as the
    ! permutation vector returned by ludcmp. b(1:n) is input as the right-hand side vector B,
    ! and returns with the solution vector X. a, n, np, and indx are not modifed by this routine
    ! and can be left in place for successive calls with different right-hand sides b. This 
    ! routine takes into account the possibility that b will begin with many zero elements, 
    ! so it is efficient for use in matrix inversion.
    
      INTEGER i,ii,j,ll
      REAL sum
      ii=0  ! When ii is set to a positive value, it will become the index
            ! of the first nonvanishing element of b. We now DO
            ! the forward substitution, equation (2.3.6). The only new
            ! wrinkle is to unscramble the permutation as we go.
      DO i=1, n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        IF (ii .NE. 0) THEN
          DO j = ii, i-1
            sum=sum-a(i,j)*b(j)
          ENDDO 
        ELSE IF (sum .NE.0.0_dp ) THEN
          ii=i ! A nonzero element was encountered, so from now on we will
               ! have to DO the sums in the loop above
        ENDIF
        b(i)=sum
      ENDDO
  
      DO i = n, 1, -1     ! Now we do the backsubstitution, equation (2.3.7).
        sum=b(i)
        DO j = i+1, n
          sum=sum-a(i,j)*b(j)
        ENDDO
        b(i)=sum/a(i,i)  ! Store a component of the solution vector X.
      ENDDO
    
      return             ! All done!
    
    END SUBROUTINE lubksb

  ! ----------------------------------------------------------------------------