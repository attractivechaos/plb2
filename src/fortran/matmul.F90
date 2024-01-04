MODULE kinds
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: sp, dp
    INTEGER, PARAMETER :: sp = KIND(1.0)
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(2*PRECISION(1.0_sp))
END MODULE kinds

MODULE mat_ops
    USE kinds, ONLY: dp
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: mat_gen, mat_mul
CONTAINS
    SUBROUTINE mat_gen(n, mat)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        REAL(KIND=dp), DIMENSION(n,n), INTENT(OUT) :: mat
        REAL(KIND=dp) :: tmp
        INTEGER :: i, j
        tmp = 1.0_dp / (n**2)
        DO j = 1, n
            DO i = 1, n
                mat(i,j) = tmp * (i-j) * (i+j-2)
            END DO
        END DO
    END SUBROUTINE mat_gen

    SUBROUTINE mat_mul(n, p, mata, m, matb, matc)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n, p, m
        REAL(KIND=dp), DIMENSION(n, p), INTENT(IN) :: mata
        REAL(KIND=dp), DIMENSION(p, m), INTENT(IN) :: matb
        REAL(KIND=dp), DIMENSION(n, m), INTENT(OUT) :: matc
        INTEGER :: i, j, k
        matc(:,:) = 0.0_dp
        DO j = 1, m
            DO k = 1, p
                DO i = 1, n
                    matc(i,j) = matc(i,j) + mata(i,k)*matb(k,j)
                END DO
            END DO
        END DO
    END SUBROUTINE mat_mul
                    

END MODULE

PROGRAM matmul
    USE kinds, ONLY: dp
    USE mat_ops, ONLY: mat_gen, mat_mul
    IMPLICIT NONE
    INTEGER :: n
    REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: a, b, c
    CHARACTER(len=32) :: arg
    INTEGER :: stat
    n = 1500
    arg = ""
    CALL GET_COMMAND_ARGUMENT(1, arg)
    IF (LEN_TRIM(arg) > 0) THEN
        READ(arg, FMT=*, IOSTAT=stat) n
        IF (stat > 0) CALL EXIT(1)
    END IF
    ALLOCATE(a(n,n))
    ALLOCATE(b(n,n))
    ALLOCATE(c(n,n))
    CALL mat_gen(n, a)
    CALL mat_gen(n, b)
    CALL mat_mul(n, n, a, n, b, c)

    WRITE(*,*) c(n/2+1,n/2+1)

    DEALLOCATE(a)
    DEALLOCATE(b)
    DEALLOCATE(c)

END PROGRAM matmul
