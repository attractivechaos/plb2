MODULE sudokus
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: sdaux_t, nsudokus, sudokulen, &
              hard20, sd_genmat, sd_solve
    INTEGER, PARAMETER :: nsudokus = 20
    INTEGER, PARAMETER :: sudokulen = 9*9
    CHARACTER(LEN=sudokulen), DIMENSION(nsudokus) :: hard20 = [ &
        "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9", &
        ".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...", &
        ".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..", &
        "........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........", &
        "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8", &
        "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1", &
        ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....", &
        "12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8", &
        "..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.", &
        "1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.", &
        "..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..", &
        "....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7", &
        "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........", &
        "7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........", &
        "3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....", &
        "........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3", &
        ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...", &
        ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..", &
        "1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1", &
        ".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67....."]
    TYPE sdaux_t
        INTEGER(KIND=INT16), DIMENSION(0:323,0:8) :: r
        INTEGER(KIND=INT16), DIMENSION(0:728,0:3) :: c
    END TYPE sdaux_t
CONTAINS
    TYPE(sdaux_t) FUNCTION sd_genmat() RESULT(a)
        IMPLICIT NONE
        INTEGER :: i,j,k,r,c,c2,r2;
        INTEGER(KIND=INT16), DIMENSION(0:323) :: nr
        i = 0
        r = 0
        DO i = 0, 8
            DO j = 0, 8
                DO k = 0, 8
                    a%c(r,0) = 9*i+j
                    a%c(r,1) = (i/3*3 + j/3) *9+k+81
                    a%c(r,2) = 9 * i + k + 162
                    a%c(r,3) = 9 * j + k + 243
                    r = r + 1
                END DO
            END DO
        END DO
        nr(:) = 0
        DO r = 0, 728
            DO c2 = 0, 3
                k = a%c(r,c2)
                a%r(k,nr(k)) = r
                nr(k) = nr(k) + 1
            END DO
        END DO
        RETURN
    END FUNCTION sd_genmat

    SUBROUTINE sd_update_for(aux, sr, sc, r, cand)
        IMPLICIT NONE
        TYPE(sdaux_t), INTENT(IN) :: aux
        INTEGER(KIND=INT16), DIMENSION(0:728), INTENT(INOUT) :: sr
        INTEGER(KIND=INT16), DIMENSION(0:323), INTENT(INOUT) :: sc
        INTEGER(KIND=INT16), INTENT(IN) :: r
        INTEGER, INTENT(OUT) :: cand
        INTEGER :: c, c2, min, min_c, r2, rr, cc, cc2
        min = 10
        min_c = 0
        DO c2 = 0, 3
            sc(aux%c(r, c2)) = IOR(sc(aux%c(r, c2)), ISHFT(1_INT16, 7))
        END DO
        DO c2 = 0, 3
            c = aux%c(r,c2)
            DO r2 = 0, 8
                rr = aux%r(c,r2)
                IF (sr(rr) /= 0) THEN
                    sr(rr) = sr(rr) + 1
                    CYCLE
                ELSE
                    sr(rr) = sr(rr) + 1
                END IF
                DO cc2 = 0, 3
                    cc = aux%c(rr,cc2)
                    sc(cc) = sc(cc) - 1
                    IF (sc(cc) < min) THEN
                        min = sc(cc)
                        min_c = cc
                    END IF
                END DO
            END DO
        END DO
        cand = IOR(ISHFT(min, 16), min_c)
    END SUBROUTINE sd_update_for

    SUBROUTINE sd_update_rev(aux, sr, sc, r)
        IMPLICIT NONE
        TYPE(sdaux_t), INTENT(IN) :: aux
        INTEGER(KIND=INT16), DIMENSION(0:728), INTENT(INOUT) :: sr
        INTEGER(KIND=INT16), DIMENSION(0:323), INTENT(INOUT) :: sc
        INTEGER(KIND=INT16), INTENT(IN) :: r
        INTEGER :: c2, r2, cc2, c, rr
        DO c2 = 0, 3
            sc(aux%c(r, c2)) = IAND(sc(aux%c(r, c2)), Z'7f')
        END DO
        DO c2 = 0, 3
            c = aux%c(r, c2)
            DO r2 = 0, 8
                rr = aux%r(c, r2)
                sr(rr) = sr(rr) -1
                IF (sr(rr) /= 0) CYCLE
                sc(aux%c(rr, 0)) = sc(aux%c(rr, 0)) + 1
                sc(aux%c(rr, 1)) = sc(aux%c(rr, 1)) + 1
                sc(aux%c(rr, 2)) = sc(aux%c(rr, 2)) + 1
                sc(aux%c(rr, 3)) = sc(aux%c(rr, 3)) + 1
            END DO
        END DO
    END SUBROUTINE sd_update_rev

    SUBROUTINE sd_solve(aux, s) 
        IMPLICIT NONE
        TYPE(sdaux_t), INTENT(IN) :: aux
        CHARACTER(LEN=sudokulen), INTENT(IN) :: s
        INTEGER(KIND=INT16), DIMENSION(0:728) :: sr
        INTEGER(KIND=INT16), DIMENSION(0:80) :: cr
        INTEGER(KIND=INT16), DIMENSION(0:323) :: sc
        INTEGER(KIND=INT16), DIMENSION(0:80) :: cc
        CHARACTER(LEN=sudokulen) :: outstr
        INTEGER :: a, i, j, r, r2, c, dir, cand, n, hints, stat, myi
        INTEGER :: min
        n = 0
        hints = 0
        sr(:) = 0
        sc(:) = IOR(ISHFT(0, 7), 9)
        outstr = s
        DO i = 0, sudokulen-1
            READ(s(i+1:i+1), FMT=*, IOSTAT=stat) a
            IF (stat == 0) THEN
                CALL sd_update_for(aux, sr, sc, INT(i*9+a-1, INT16), cand)
                hints = hints + 1
            END IF
            cr(i) = -1
            cc(i) = -1
        END DO
        dir = 1
        cand = IOR(ISHFT(10,16), 0)
        i = 0
        DO
            DO WHILE (i >= 0 .AND. i < sudokulen - hints)
                IF (dir == 1) THEN
                    min = ISHFT(cand, -16)
                    cc(i) = IAND(cand, Z'FFFF')
                    IF (min > 1) THEN
                        DO c = 0, 323
                            IF (sc(c) < min) THEN
                                min = sc(c)
                                cc(i) = c
                                IF (min <= 1) EXIT
                            END IF
                        END DO
                    END IF
                    IF (min == 0 .OR. min == 10) THEN
                        cr(i) = -1
                        dir = -1
                        i = i -1
                    END IF
                END IF
                c = cc(i)
                IF (dir == -1 .AND. cr(i) >= 0) THEN
                    CALL sd_update_rev(aux, sr, sc, INT(aux%r(c, cr(i)), INT16))
                END IF
                r2 = cr(i) + 1
                DO WHILE (r2 < 9)
                    IF (sr(aux%r(c, r2)) == 0) EXIT
                    r2 = r2 + 1
                END DO
                IF (r2 < 9) THEN
                    CALL sd_update_for(aux, sr, sc, INT(aux%r(c,r2), INT16), cand)
                    cr(i) = r2
                    dir = 1
                    i = i + 1
                ELSE
                    cr(i) = -1
                    dir = -1
                    i = i - 1
                END IF
            END DO
            IF (i < 0) EXIT
            DO j = 0, i - 1
                r = aux%r(cc(j), cr(j))
                WRITE(outstr(r/9+1:r/9+1), FMT='(I1)') MOD(r, 9) + 1
            END DO
            WRITE(*, FMT='(A)') outstr
            n = n-1
            i = i-1
            dir = -1
        END DO
    END SUBROUTINE sd_solve
END MODULE sudokus
    
PROGRAM sudoku
    USE sudokus, ONLY: sdaux_t, nsudokus, sudokulen, &
                       hard20, sd_genmat, sd_solve
    IMPLICIT NONE
    INTEGER :: n
    CHARACTER(len=32) :: arg
    INTEGER :: stat
    INTEGER :: i, j
    TYPE(sdaux_t) :: a
    CHARACTER(LEN=sudokulen) :: buf
    n = 200
    CALL GET_COMMAND_ARGUMENT(1, arg)
    IF (LEN_TRIM(arg) > 0) THEN
        READ(arg, FMT=*, IOSTAT=stat) n
        IF (stat > 0) CALL EXIT(1)
    END IF
    a = sd_genmat();
    DO i = 1, n
        DO j = 1, nsudokus
            buf = hard20(j)
            CALL sd_solve(a, buf)
            WRITE(*, FMT='(A1)', ADVANCE='no') NEW_LINE('A')
        END DO
    END DO
END PROGRAM sudoku
