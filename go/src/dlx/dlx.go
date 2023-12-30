// Package dlx is an implementation of Knuth's Dancing Links for algorithm X
// to solve a generalized cover problem.
//
// See:
//     arXiv:cs/0011047
//     https://en.wikipedia.org/wiki/Dancing_Links
// An alternative implementation can be found within:
//     https://rosettacode.org/wiki/Sudoku#Go
package dlx

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"strings"
)

// x is Knuth's data object.
type x struct {
	left, right *x      // row links
	up, down    *x      // column links
	col         *column // column list header
}

// column is Knuth's column object.
type column struct {
	x
	size int // number of 1's in column
	id   int // XXX name string?
}

// Matrix represents the matrix for a generalized cover problem.
type Matrix struct {
	root    *x       // up, down, col fields unused
	headers []column // column headers
	sol     []*x     // solution so far
	cells   []x      // pre-allocated cells
	stats   []stat

	maxCols int      // maximum number of columns seen in any row constraint
}

type stat struct {
	nodes   int
	updates int
}

// New returns a new DLX Matrix with the specified number of columns
func New(primaryCols, secondaryCols int) *Matrix {
	return NewWithHint(primaryCols, secondaryCols, 0, 0)
}

// NewWithHint is like New but provides an allocation hint for the
// estimated number of cells and estimated maximum number of rows in
// solutions.
func NewWithHint(primaryCols, secondaryCols, estCells, estSolutionRows int) *Matrix {
	n := primaryCols + secondaryCols
	m := &Matrix{
		headers: make([]column, n),
		sol:     make([]*x, 0, estSolutionRows),
		cells:   make([]x, estCells+1), // +1 to use as the root
		stats:   make([]stat, 0, estSolutionRows),
	}
	m.root = &m.cells[0]
	m.cells = m.cells[1:]
	m.root.left = &m.headers[primaryCols-1].x
	m.root.left.right = m.root
	prev := m.root
	for i := 0; i < n; i++ {
		c := &m.headers[i]
		c.id = i
		c.col = c
		c.up = &c.x
		c.down = &c.x
		if i < primaryCols {
			c.left = prev
			prev.right = &c.x
			prev = &c.x
		} else {
			c.left = &c.x
			c.right = &c.x
		}
	}
	return m
}

// AddRow adds a new constraint row to the matrix.
// 'cols' indicates which column indices should have a 1 for this row.
func (m *Matrix) AddRow(cols []int) {
	if len(cols) == 0 {
		return
	}
	if len(cols) > m.maxCols {
		m.maxCols = len(cols)
	}
	var buf []x
	if len(cols) <= len(m.cells) {
		buf = m.cells[:len(cols)]
		m.cells = m.cells[len(cols):]
	} else {
		buf = make([]x, len(cols))
	}
	//sort.Ints(cols) // not strictly required
	prev := &buf[len(cols)-1]
	for i, id := range cols {
		c := &m.headers[id]
		c.size++
		x := &buf[i]
		x.col = c
		x.up = c.up
		x.down = &c.x
		x.left = prev
		x.up.down = x
		x.down.up = x
		prev.right = x
		prev = x
	}
}

// SearchFunc is the type of the function called for each solution
// found by Matrix.Search.
//
// The pseudo error value Stop may be returned to indication the search
// should terminate without error.
type SearchFunc func(*Matrix) error

// Stop is used as a return value from SearchFuncs to indicate that
// the search should terminate instead of continuing to search for
// alternative solutions.
// It is not returned as an error by any function.
var Stop = errors.New("terminate search")

func (m *Matrix) callFn(fn SearchFunc) error {
	return fn(m)
}

// SolutionString returns a text representation of
// the solution using the provided column names.
func (m *Matrix) SolutionString(names []string) string {
	var buf strings.Builder
	_ = m.SolutionWrite(&buf, names)
	return buf.String()
}

// SolutionWrite writes a text representation of the
// solution to `w` using the provided column names.
func (m *Matrix) SolutionWrite(w io.Writer, names []string) error {
	bw := bufio.NewWriter(w)
	for _, x := range m.sol {
		n := names[x.col.id]
		fmt.Fprint(bw, n)
		for j := x.right; j != x; j = j.right {
			n = names[j.col.id]
			fmt.Fprint(bw, " ", n)
		}
		fmt.Fprintln(bw)
	}
	return bw.Flush()
}

// SolutionIDs writes the column IDs of the solution
// to `buf` and returns the extended slice.
func (m *Matrix) SolutionIDs(buf [][]int) [][]int {
	if cap(buf) < len(m.sol) {
		new := make([][]int, len(buf), len(m.sol))
		copy(new, buf)
		buf = new
	}
	solIDs := buf[:len(m.sol)]
	for i, x := range m.sol {
		n := 1
		min := x
		for j := x.right; j != x; j = j.right {
			n++
			if j.col.id < min.col.id {
				min = j
			}
		}
		ids := solIDs[i]
		if cap(ids) < n {
			ids = make([]int, 1, m.maxCols)
		} else {
			ids = ids[:1]
		}
		ids[0] = min.col.id
		for j := min.right; j != min; j = j.right {
			ids = append(ids, j.col.id)
		}
		//sort.Ints(ids) // not strictly required
		solIDs[i] = ids
	}
	return solIDs
}

// ProfileString returns profiling output as a string.
func (m *Matrix) ProfileString() string {
	var buf strings.Builder
	_ = m.ProfileWrite(&buf)
	return buf.String()
}

// ProfileWrite writes profiling output to `w`.
func (m *Matrix) ProfileWrite(w io.Writer) error {
	bw := bufio.NewWriter(w)
	var total stat
	for _, s := range m.stats {
		total.nodes += s.nodes
		total.updates += s.updates
	}
	fmt.Fprintln(bw, "Level        Nodes            Updates     Updates per node")
	for i, s := range m.stats {
		pn := float64(s.nodes) / float64(total.nodes) * 100.0
		pu := float64(s.updates) / float64(total.updates) * 100.0
		per := float64(s.updates) / float64(s.nodes)
		fmt.Fprintf(bw, "%5d %8d  (%2.0f%%) %10d  (%2.0f%%) %14.1f\n",
			i, s.nodes, pn, s.updates, pu, per)
	}
	per := float64(total.updates) / float64(total.nodes)
	fmt.Fprintf(bw, "Total %8d (100%%) %10d (100%%) %14.1f\n",
		total.nodes, total.updates, per)
	return bw.Flush()
}

// Search runs Knuth's algorithm X on `m`
// and for each solution found calls `fn`.
func (m *Matrix) Search(fn SearchFunc) error {
	if len(m.sol) > 0 {
		return errors.New("recursive call to Matrix.Search")
	}
	err := m.search(fn)
	if err == Stop {
		return nil
	}
	return err
}

func (m *Matrix) search(fn SearchFunc) error {
	k := len(m.sol)
	j := m.root.right
	if j == m.root {
		return m.callFn(fn)
	}
	c := j.col
	if true { // Knuth's "S heuristic"
		for j = j.right; j != m.root; j = j.right {
			if j.col.size < c.size {
				c = j.col
			}
		}
	}
	if c.size < 1 {
		return nil
	}
	if len(m.stats) <= k {
		m.stats = append(m.stats, stat{})
	}
	s := &m.stats[k]
	s.nodes += c.size

	cover(c, s)
	m.sol = append(m.sol, nil)
	for r := c.down; r != &c.x; r = r.down {
		m.sol[k] = r
		for j = r.right; j != r; j = j.right {
			cover(j.col, s)
		}
		if err := m.search(fn); err != nil {
			return err
		}
		for j = r.left; j != r; j = j.left {
			uncover(j.col)
		}
	}

	m.sol = m.sol[:k]
	uncover(c)
	return nil
}

func cover(c *column, s *stat) {
	c.right.left, c.left.right = c.left, c.right
	s.updates++
	for i := c.down; i != &c.x; i = i.down {
		for j := i.right; j != i; j = j.right {
			j.down.up, j.up.down = j.up, j.down
			j.col.size--
			s.updates++
		}
	}
}

func uncover(c *column) {
	for i := c.up; i != &c.x; i = i.up {
		for j := i.left; j != i; j = j.left {
			j.col.size++
			j.down.up, j.up.down = j, j
		}
	}
	c.right.left, c.left.right = &c.x, &c.x
}
