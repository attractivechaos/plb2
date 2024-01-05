MODULE mat_ops
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: mat_gen, mat_mul
CONTAINS
    SUBROUTINE mat_gen(n, mat)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n
        REAL(KIND=REAL64), DIMENSION(n,n), INTENT(OUT) :: mat
        REAL(KIND=REAL64) :: tmp
        INTEGER :: i, j
        tmp = 1.0_REAL64 / (n**2)
        DO j = 1, n
            DO i = 1, n
                mat(i,j) = tmp * (i-j) * (i+j-2)
            END DO
        END DO
    END SUBROUTINE mat_gen

    SUBROUTINE mat_mul(n, p, mata, m, matb, matc)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: n, p, m
        REAL(KIND=REAL64), DIMENSION(n, p), INTENT(IN) :: mata
        REAL(KIND=REAL64), DIMENSION(p, m), INTENT(IN) :: matb
        REAL(KIND=REAL64), DIMENSION(n, m), INTENT(OUT) :: matc
        INTEGER :: i, j, k
        matc(:,:) = 0.0_REAL64
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
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE mat_ops, ONLY: mat_gen, mat_mul
    IMPLICIT NONE
    INTEGER :: n
    REAL(KIND=REAL64), DIMENSION(:,:), ALLOCATABLE :: a, b, c
    CHARACTER(len=32) :: arg
    INTEGER :: stat
    n = 1500
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
