MODULE nqueen_ops
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: nq_solve
CONTAINS
    INTEGER FUNCTION nq_solve(n) RESULT(m)
        USE, INTRINSIC :: ISO_FORTRAN_ENV
        INTEGER, INTENT(IN) :: n
        INTEGER(KIND=INT32) :: y, y0, z
        INTEGER(KIND=INT32), DIMENSION(0:n-1) :: a, l, c, r
        INTEGER :: k, i
        m = 0
        y0 = ISHFT(1, n) - 1
        a = -1
        l = 0
        c = 0
        r = 0
        k = 0
        DO WHILE (k >= 0)
            y = IAND(y0, IOR(r(k), IOR(l(k), c(k))))
            IF (ISHFT(IEOR(y, y0), -(a(k)+1)) /= 0) THEN
                i = a(k) + 1
                DO WHILE (i<n)
                    IF (IAND(y, ISHFT(1,i)) == 0) EXIT
                    i = i+1
                END DO
                IF (k < n-1) THEN
                    z = ISHFT(1,i)
                    a(k) = i
                    k = k+1
                    l(k) = ISHFT(IOR(l(k-1), z), 1)
                    c(k) =       IOR(c(k-1), z)
                    r(k) = ISHFT(IOR(r(k-1), z), -1)
                ELSE
                    m = m + 1
                    k = k-1
                END IF
            ELSE
                a(k) = -1
                k = k-1
            END IF
        END DO
        RETURN 
    END FUNCTION nq_solve
END MODULE nqueen_ops

PROGRAM nqueen
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE nqueen_ops, ONLY: nq_solve
    IMPLICIT NONE
    INTEGER, PARAMETER :: nq_max = 30
    INTEGER :: n, m
    CHARACTER(len=32) :: arg
    INTEGER :: stat
    n = 15
    CALL GET_COMMAND_ARGUMENT(1, arg)
    IF (LEN_TRIM(arg) > 0) THEN
        READ(arg, FMT=*, IOSTAT=stat) n
        IF (stat > 0) CALL EXIT(1)
    END IF
    IF (n > nq_max) CALL EXIT(1)
    m = nq_solve(n)
    WRITE(*,*) m
END PROGRAM nqueen
