import std;

struct Interval(S, T)
{
    T data;
    S st;
    S en;
    S max;
}

alias interval = Interval!(size_t, size_t);

nothrow pure @safe
byte interIndex(ref interval[] a)
{
    auto n = a.length;
    if (n == 0)
        return -1;
    size_t last_i;
    long last;
    foreach (i; iota(0, n, 2))
    {
        last_i = i;
        a[i].max = a[i].en;
        last = a[i].max;
    }
    byte k = 1;
    while (1L << k <= n)
    {
        size_t x = 1L << (k - 1);
        size_t i0 = (x << 1) - 1;
        size_t step = x << 2;
        foreach (i; iota(i0, n, step))
        {
            long el = a[i - x].max;
            long er = i + x < n ? a[i + x].max : last;
            long e = a[i].en;
            if (e < el)
                e = el;
            if (e < er)
                e = er;
            a[i].max = e;
        }
        last_i = (last_i >> k & 1) != 0 ? last_i - x : last_i + x;
        if (last_i < n && a[last_i].max > last)
            last = a[last_i].max;
        k++;
    }
    return k--;
}

struct StackCell
{
    int x;
    byte k;
    byte w;
}

pure nothrow @safe
void interOverlap(const interval[] a, const byte max_level, const long st, const long en, RefAppender!(interval[]) b)
{
    b.clear();
    auto n = a.length;
    StackCell[64] stack;
    int t = 0;
    stack[t++] = StackCell((1 << max_level) - 1, max_level, 0);
    while (t)
    {
        auto s = stack[--t];
        if (s.k <= 3)
        {
            size_t i0 = s.x >> s.k << s.k;
            size_t i1 = i0 + (1L << (s.k + 1)) - 1;
            i1 = min(i1, n);
            size_t i = i0;
            while (i < i1 && a[i].st < en)
            {
                if (st < a[i].en) //b ~= a[i];
                    b.put(a[i]);
                i++;
            }
        }
        else if (s.w == 0)
        {
            int y = s.x - (1 << (s.k - 1));
            stack[t++] = StackCell(s.x, s.k, 1);
            if ((y >= n) || (a[y].max > st))
                stack[t++] = StackCell(y, cast(byte)(s.k - 1), 0);
        }
        else if (s.x < n && a[s.x].st < en)
        {
            if (st < a[s.x].en) //b ~= a[s.x];
                b.put(a[s.x]);
            stack[t++] = StackCell(s.x + (1 << (s.k - 1)), cast(byte)(s.k - 1), 0);
        }
    }
}

uint splitMix32(ref uint x)
{
    x += 0x9e3779b9;
    uint z = x;
    z = (z ^ (z >> 16)) * 0x21f0aaad;
    z = (z ^ (z >> 15)) * 0x735a2d97;
    return z ^ (z >> 15);
}

interval[] gen_intv(long n, ref uint x, const byte bit_st, const byte bit_len)
{
    interval[] res = new interval[](n);
    long mask_st = (1 << bit_st) - 1;
    long mask_len = (1 << bit_len) - 1;
    foreach (i, ref el; res)
    {
        long st = cast(long) splitMix32(x) & mask_st;
        long en = st + (cast(long) splitMix32(x) & mask_len);
        el.st = st;
        el.en = en;
        el.max = i;
    }
    return res;
}

void main(in string[] args)
{
    const byte bit_st = 28;
    const byte bit_len = 14;
    uint x = 11;
    const long n = 1_000_000L;
    auto a1 = gen_intv(n, x, bit_st, bit_len);
    const a2 = gen_intv(n, x, bit_st, bit_len);
    a1.sort!"a.st < b.st";
    auto max_level = interIndex(a1);
    interval[] b;
    auto app = appender(&b);
    long tot_cov, cov_st, cov_en, cov;
    long st0, en0, st1, en1;
    foreach (a2j; a2)
    {
        st0 = a2j.st;
        en0 = a2j.en;
        interOverlap(a1, max_level, st0, en0, app);
        if (b.length == 0)
            continue;
        cov_st = b[0].st > st0 ? b[0].st : st0;
        cov_en = b[0].en < en0 ? b[0].en : en0;
        cov = 0;
        foreach (bi; b)
        {
            st1 = bi.st > st0 ? bi.st : st0;
            en1 = bi.en < en0 ? bi.en : en0;
            if (st1 > cov_en)
            {
                cov += cov_en - cov_st;
                cov_st = st1;
                cov_en = en1;
            }
            else
            {
                cov_en = cov_en > en1 ? cov_en : en1;
            }
        }
        cov += cov_en - cov_st;
        tot_cov += cov;
    }
    writeln(tot_cov);
}
