// Finds overlaps between two arrays of 1,000,000 intervals with implicit interval trees.
// The algorithm involves frequent array access in a pattern similar to binary searches.
//
// Implementation mimics the C implementation in src/c/bedcov.c
// Links:
// * https://en.wikipedia.org/wiki/Interval_tree
// * https://academic.oup.com/bioinformatics/article/37/9/1315/5910546?login=false
package main

import (
	"fmt"
	"math/bits"
)

// SplitMix32 generates pseudo-random 32 bits numbers.
type SplitMix32 struct {
	state uint32
}

// random yields the next pseudo-random `uint32` number.
func (s *SplitMix32) random() uint32 {
	s.state += 0x9e3779b9
	z := s.state
	z = (z ^ (z >> 16)) * 0x21f0aaad
	z = (z ^ (z >> 15)) * 0x735a2d97
	return z ^ (z >> 15)
}

// RangeType used to represent an interval.
type RangeType uint64

// DataType with the information stored in the interval.
type DataType int64

// Interval representation.
type Interval struct {
	start RangeType
	end   RangeType
	max   RangeType
	data  DataType
}

// GenerateIntervals randomly, with the start using up to startBits, and the length of the interval defined by
// up to lenBits.
func GenerateIntervals(num int, rng *SplitMix32, startBits int, lenBits int) (intervals []Interval) {
	intervals = make([]Interval, num)
	startMask := (RangeType(1) << startBits) - 1
	lenMask := (RangeType(1) << lenBits) - 1
	for ii := 0; ii < num; ii++ {
		start := RangeType(rng.random()) & startMask
		end := start + (RangeType(rng.random()) & lenMask)
		intervals[ii] = Interval{start, end, 0, DataType(ii)}
	}
	return
}

// InplaceRadixSort intervals by its start value, using radix sort with 2 buckets (per bit).
//
// It uses 1-bit radix, and sort in-place, splitting one bit at a time. O(log2(maxStart)*n)
func InplaceRadixSort(intervals []Interval) {
	maxStart := intervals[0].start
	for _, interval := range intervals {
		maxStart = max(maxStart, interval.start)
	}
	maxBit := 64 - bits.LeadingZeros64(uint64(maxStart)) - 1 // Highest bit used.
	if maxBit < 0 {
		return
	}
	radixSortRecursive(intervals, maxBit)
}

// radixSortRecursive sorts the slice by the given bit, and then recursively sorts
// the two parts (separated by the given bit value) by the next bit down to bit 0.
func radixSortRecursive(intervals []Interval, bit int) {
	n := len(intervals)
	if n <= 1 {
		return
	}
	mask := RangeType(1) << bit

	// Split slice by given bit: start values with bit set to 0 goes to the first part, bit 1 goes to the second.
	indexBit0, indexBit1 := 0, len(intervals)-1
	for {
		// Find first interval with start bit set to 1 in the bottom part of the slice.
		for (intervals[indexBit0].start&mask == 0) && indexBit0 < indexBit1 {
			indexBit0++
		}
		// Find last interval with start bit set to 0 in the top part of the slice.
		for (intervals[indexBit1].start&mask > 0) && indexBit1 > indexBit0 {
			indexBit1--
		}
		if indexBit0 >= indexBit1 {
			if intervals[indexBit0].start&mask == 0 {
				indexBit0++ // Make sure indexBit0 points to one-past the last element with bit==0
			}
			// We are done splitting by given bit.
			break
		}
		// Swap elements pointed by indexBit0 and indexBit1, since they are out of order.
		intervals[indexBit0], intervals[indexBit1] = intervals[indexBit1], intervals[indexBit0]
		indexBit0++
		indexBit1--
	}
	if bit == 0 {
		// Last bit to sort.
		return
	}
	// Recursively sort the two splits on the next lower bits.
	bit--
	if indexBit0 > 0 {
		radixSortRecursive(intervals[:indexBit0], bit)
	}
	if indexBit0 < n {
		radixSortRecursive(intervals[indexBit0:], bit)
	}
}

/*
ImplicitIntervalTree representation, stored as a slice.

Suppose there are N=2^(K+1)-1 sorted numbers in an array a[]. They
implicitly form a complete binary tree of height K+1. We consider leaves to
be at level 0.

Example: N=7, K=2

	    011
	  /     \
	001     101
	/ \     / \

000 010 100 110

	The binary tree has the following properties:

	1. The lowest k-1 bits of nodes at level k are all 1. The k-th bit is 0.
	   The first node at level k is indexed by 2^k-1. The root of the tree is
	   indexed by 2^K-1.

	2. For a node x at level k, its left child is x-2^(k-1) and the right child
	   is x+2^(k-1).

	3. For a node x at level k, it is a left child if its (k+1)-th bit is 0. Its
	   parent node is x+2^k. Similarly, if the (k+1)-th bit is 1, x is a right
	   child and its parent is x-2^k.

	4. For a node x at level k, there are 2^(k+1)-1 nodes in the subtree
	   descending from x, including x. The left-most leaf is x&~(2^k-1) (masking
	   the lowest k bits to 0).

	When numbers can't fill a complete binary tree, the parent of a node may not
	be present in the array. The implementation here still mimics a complete
	tree, though getting the special casing right is a little complex. There may
	be alternative solutions.

	As a sorted array can be considered as a binary search tree, we can
	implement an interval tree on top of the idea. We only need to record, for
	each node, the maximum value in the subtree descending from the node.
*/
type ImplicitIntervalTree struct {
	intervals []Interval
	depth     int
	stack     [64]StackCell
}

// MakeImplicitIntervalTree takes over the intervals slice and index them in an implicit interval tree for fast access.
//
// Note: Since these are integers, one could use a radix sort. Not done yet because sort uses only ~10% of the time.
func MakeImplicitIntervalTree(intervals []Interval) ImplicitIntervalTree {
	// Sort by interval start.
	//timer := time.Now()
	InplaceRadixSort(intervals)
	// slices.SortFunc(intervals, func(i, j Interval) int { return int(i.start - j.start) })
	//fmt.Printf("Sort time: %.3f\n", float64(time.Since(timer).Microseconds())/1e6)
	n := len(intervals)
	if n == 0 {
		return ImplicitIntervalTree{}
	}
	lastIdx := 0
	last := RangeType(0)
	for i := 0; i < n; i += 2 {
		lastIdx = i
		intervals[i].max = intervals[i].end
		last = intervals[i].max
	}
	k := 1
	for 1<<k <= n {
		x := 1 << (k - 1)
		i0 := (x << 1) - 1
		step := x << 2
		for i := i0; i < n; i += step {
			el := intervals[i-x].max
			er := last
			if i+x < n {
				er = intervals[i+x].max
			}
			e := intervals[i].end
			if e < el {
				e = el
			}
			if e < er {
				e = er
			}
			intervals[i].max = e
		}
		lastIdx = lastIdx + x
		if lastIdx>>k&1 != 0 {
			lastIdx = lastIdx - x
		}
		if lastIdx < n && intervals[lastIdx].max > last {
			last = intervals[lastIdx].max
		}
		k += 1
	}
	return ImplicitIntervalTree{intervals: intervals, depth: k - 1}
}

type StackCell struct {
	x int
	k int8
	w int8
}

func (tree *ImplicitIntervalTree) FindOverlaps(start, end RangeType, overlapsBuffer []Interval) []Interval {
	maxDepth := tree.depth
	intervals := tree.intervals

	n := len(intervals)
	overlaps := overlapsBuffer[:0]
	tree.stack[0] = StackCell{(1 << maxDepth) - 1, int8(maxDepth), 0}
	t := 1
	for t > 0 {
		t -= 1
		z := tree.stack[t]
		if z.k <= 3 {
			i0 := int(z.x) >> z.k << z.k
			i1 := i0 + ((1 << int(z.k+1)) - 1)
			if i1 >= n {
				i1 = n
			}
			i := i0
			for i < i1 && intervals[i].start < end {
				if start < intervals[i].end {
					overlaps = append(overlaps, intervals[i])
				}
				i += 1
			}
		} else if z.w == 0 {
			y := int(z.x) - 1<<int(z.k-1)
			tree.stack[t] = StackCell{z.x, z.k, 1}
			t += 1
			if y >= n || intervals[y].max > start {
				tree.stack[t] = StackCell{y, z.k - 1, 0}
				t += 1
			}
		} else if int(z.x) < n && intervals[z.x].start < end {
			if start < intervals[z.x].end {
				overlaps = append(overlaps, intervals[z.x])
			}
			tree.stack[t] = StackCell{z.x + (1 << (z.k - 1)), z.k - 1, 0}
			t += 1
		}
	}
	return overlaps
}

const (

	// Parameters of random interval generation.

	NumIntervals       = 1000000
	IntervalStartBits  = 28
	IntervalLengthBits = 14
)

func main() {
	var rng = &SplitMix32{state: 11} // Fixed seed, to get deterministic (always the same) result.
	tree := MakeImplicitIntervalTree(GenerateIntervals(NumIntervals, rng, IntervalStartBits, IntervalLengthBits))
	intervalsToCheck := GenerateIntervals(NumIntervals, rng, IntervalStartBits, IntervalLengthBits)

	totalCoverage := RangeType(0)
	var overlaps = make([]Interval, 0, 1<<IntervalLengthBits) // Reused to avoid extra allocations.
	for _, interval := range intervalsToCheck {
		currentStart, currentEnd := interval.start, interval.end
		overlaps = tree.FindOverlaps(currentStart, currentEnd, overlaps)
		if len(overlaps) == 0 {
			continue
		}

		coverage := RangeType(0) // Accumulates coverage of `interval` in tree.
		coveredStart := currentStart
		if overlaps[0].start > currentStart {
			coveredStart = overlaps[0].start
		}
		coveredEnd := currentEnd
		if overlaps[0].end < currentEnd {
			coveredEnd = overlaps[0].end
		}
		for _, overlap := range overlaps {
			newStart := max(currentStart, overlap.start)
			newEnd := min(currentEnd, overlap.end)
			if newStart > coveredEnd {
				coverage += coveredEnd - coveredStart
				coveredStart = newStart
				coveredEnd = newEnd
			} else {
				if coveredEnd < newEnd {
					coveredEnd = newEnd
				}
			}
		}
		coverage += coveredEnd - coveredStart
		totalCoverage += coverage
	}
	fmt.Println(totalCoverage)
}
