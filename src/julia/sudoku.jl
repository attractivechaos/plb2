# adapted from plb/sudoku/sudoku_v1.jl
# the old version is incompatible with the latest Julia

function sd_genmat()
    C = Array{UInt16}(undef, 729, 4)
    R = Array{UInt16}(undef, 324, 9)
    r = 1
    @inbounds for i in 0:8, j in 0:8, k in 0:8
        C[r, 1] = 9 * i + j + 1
        C[r, 2] = (div(i, 3) * 3 + div(j, 3)) * 9 + k + 82
        C[r, 3] = 9 * i + k + 163
        C[r, 4] = 9 * j + k + 244
        r += 1
    end
    nr = ones(UInt8, 324)
    @inbounds for r in 1:729, c in 1:4
        k = C[r, c]
        R[k, nr[k]] = r
        nr[k] += 1
    end
    return R, C
end

@inline function sd_update(R, C, sr, sc, r)
    m = 10
    m_c = 0
    @inbounds for c2 in 1:4
        sc[C[r, c2]] += 128
    end
    @inbounds for c2 in 1:4
        c = C[r, c2]
        for r2 in 1:9
            rr = R[c, r2] #10
            t = sr[rr] #11
            sr[rr] += 1
            t != 0 && continue
            for cc2 in 1:4
                cc = C[rr, cc2] #15
                if (sc[cc] -= 1) < m #16
                    m = sc[cc]
                    m_c = cc - 1
                end
            end
        end
    end
    return m << 16 | m_c
end

@inline function revert(R, C, sr, sc, r)
    @inbounds for c2 in 1:4
        sc[C[r, c2]] -= 128
    end
    @inbounds for c2 in 1:4
        c = C[r, c2]
        for r2 in 1:9
            rr = R[c, r2]
            (sr[rr] -= 1) != 0 && continue #9
            for i in 1:4
                sc[C[rr, i]] += 1 #11
            end
        end
    end
end

function sd_solve(R, C, _s)
    hints = 0
    out = zeros(Int, 81)
    sr = zeros(Int, 729)
    sc = Array{Int}(undef, 324)
    fill!(sc, 9)
    cr = zeros(Int, 81)
    cc = zeros(Int, 81)
    cu = codeunits(_s)  # iterator over bytes in string
    @inbounds for i in 1:81
        a = (cu[i] >= UInt8('1')) && (cu[i] <= UInt8('9')) ? Int8(cu[i] - UInt8('1')) : Int8(-1)
        if a >= 0
            sd_update(R, C, sr, sc, (i - 1) * 9 + a + 1)
            hints += 1
        end
        out[i] = a + 1
    end
    i, d, cand = 1, 1, 10 << 16 | 0
    @inbounds while true
        while i >= 1 && i < 82 - hints
            if d == 1
                m, cc[i] = cand >> 16, (cand & 0xffff) + 1
                if m > 1
                    for c in 1:324
                        if sc[c] < m
                            m, cc[i] = sc[c], c
                            m < 2 && break
                        end
                    end
                end
                if m == 0 || m == 10
                    cr[i] = d = 0
                    i -= 1
                end
            end
            c = cc[i]
            r2 = cr[i] + 1
            d == 0 && cr[i] >= 1 && revert(R, C, sr, sc, R[c, r2 - 1])
            for rr in r2:9
                sr[R[c, rr]] == 0 && break
                r2 += 1
            end
            if r2 < 10
                cand = sd_update(R, C, sr, sc, R[c, r2])
                cr[i], d = r2, 1
                i += 1
            else
                cr[i] = d = 0
                i -= 1
            end
        end
        i < 1 && break
        for j in 1:(i - 1)
            r = R[cc[j], cr[j]] - 1
            out[div(r, 9) + 1] = r % 9 + 1
        end
        println(join(out, ""))
        i -= 1
        d = 0
    end
    return out
end

const hard20 = [
    "..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9",
    ".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...",
    ".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..",
    "........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........",
    "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8",
    "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1",
    ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....",
    "12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8",
    "..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.",
    "1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.",
    "..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..",
    "....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7",
    "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........",
    "7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........",
    "3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....",
    "........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3",
    ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...",
    ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..",
    "1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1",
    ".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67.....",
];

function main(n)
    R, C = sd_genmat()
    for i in 1:n
        for str in hard20
            r = sd_solve(R, C, str)
            println()
        end
    end
end

main(200)
