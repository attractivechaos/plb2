import std/[strutils, cmdline]

type
    Sdaux_t = object
            r: array[324, array[9, uint16]]
            c: array[729, array[4, uint16]]


proc `|=`[T](a: var T, b: T) {.inline.} =
    a = a or b


proc `&=`[T](a: var T, b: T) {. inline .} =
    a = a and b


proc initSdaux_t: Sdaux_t =
    var r = 0
    var nr: array[324, int8]
    for i in 0 ..< 9:
        for j in 0 ..< 9:
            for k in 0 ..< 9:
                result.c[r][0] = uint16(9 * i + j)
                result.c[r][1] = uint16((i div 3 * 3 + j div 3) * 9 + k + 81)
                result.c[r][2] = uint16(9 * i + k + 162)
                result.c[r][3] = uint16(9 * j + k + 243)
                inc r
    
    for c in 0 ..< 324:
        nr[c] = 0
    var k2: int
    for r2 in 0 ..< 729:
        for c2 in 0 ..< 4:
            k2 = result.c[r2][c2].int
            result.r[k2][nr[k2]] = r2.uint16
            inc nr[k2]
           

proc sdUpdateFor(aux: Sdaux_t, sr: var array[729, int8], sc: var array[324, uint8], r: uint16): int =
    var
        min = 10
        min_c = 0
    for c2 in 0 ..< 4:
        sc[aux.c[r][c2]] |= 1.uint8 shl 7
    for c2 in 0 ..< 4:
        var c = aux.c[r][c2]
        for r2 in 0 ..< 9:
            var rr = aux.r[c][r2]
            if sr[rr] != 0:
                inc sr[rr]
                continue
            else:
                inc sr[rr]
            for cc2 in 0 ..< 4:
                var cc = aux.c[rr][cc2]
                dec sc[cc] 
                if sc[cc] < min.uint8:
                    min = sc[cc].int
                    min_c = cc.int

    return (min shl 16) or min_c 


proc sdUpdateRev(aux: Sdaux_t, sr: var array[729, int8], sc: var array[324, uint8], r: uint16) =
    for c2 in 0 ..< 4:
        sc[aux.c[r][c2]] &= 0x7f
    for c2 in 0 ..< 4:
        var c = aux.c[r][c2]
        for r2 in 0 ..< 9:
            var rr = aux.r[c][r2]
            dec sr[rr]
            if sr[rr] != 0:
                continue
            var p = aux.c[rr]
            inc sc[p[0]]
            inc sc[p[1]]
            inc sc[p[2]]
            inc sc[p[3]]


proc sdSolve(aux: Sdaux_t, line: string): int =
    var 
        sr: array[729, int8] 
        cr: array[81, int8]
        sc: array[324, uint8] 
        cc: array[81, int16]
        output: string = newString(line.len)
        hints: int
        n: int
    
    for c in 0 ..< 324:
        sc[c] = (0 shl 7) or 9
    for i in 0 ..< 81:
        var a: int
        if line[i] >= '1' and line[i] <= '9':
            a = line[i].ord - '1'.ord
        else:
            a = -1
        
        if a >= 0:
            discard sdUpdateFor(aux, sr, sc, (i * 9 + a).uint16)
            inc hints
        cr[i] = -1
        cc[i] = -1
        output[i] = line[i]
    
    var
        dir = 1
        cand = 10 shl 16 or 0 # I don't think the or 0 does anything
        i: int
    
    while true:
        while i >= 0 and i < 81 - hints:
            if dir == 1:
                var min = cand shr 16
                cc[i] = (cand and 0xffff).int16 
                if min > 1:
                    for c in 0 ..< 324:
                        if sc[c] < min.uint8:
                            min = sc[c].int
                            cc[i] = c.int16
                            if min <= 1:
                                break
                if min == 0 or min == 10:
                    cr[i] = -1
                    dec i
                    dir = -1
            var c = cc[i]
            if dir == -1 and cr[i] >= 0:
                sdUpdateRev(aux, sr, sc, aux.r[c][cr[i]])
            var r2 = cr[i] + 1
            while r2 < 9:
                if sr[aux.r[c][r2]] == 0:
                    break
                inc r2
            if r2 < 9:
                cand = sdUpdateFor(aux, sr, sc, aux.r[c][r2])
                cr[i] = r2.int8
                inc i
                dir = 1
            else:
                cr[i] = -1
                dec i
                dir = -1
        if i < 0:
            break
        for j in 0 ..< i:
            var r2 = aux.r[cc[j]][cr[j]]
            output[r2 div 9] = char(r2 mod 9 + '1'.ord)
        echo output
        inc n
        dec i
        dir = -1
        
    return n

            
const hard20 = """
..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9
.......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...
.2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..
........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........
12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8
1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1
.......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....
12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8
..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.
1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.
..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..
....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7
4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........
7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........
3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....
........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3
.......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...
.......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..
1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1
.....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67.....
"""


if isMainModule:
    const lines = hard20.split("\n")
    var n = 200
    if paramCount() >= 1:
        n = paramStr(1).parseInt
    var a = initSdaux_t()  # Good
    for i in 0 ..< n:
        for j in 0 ..< 20:
            discard sdSolve(a, lines[j])

