//////////////////////////////////////////////////////////////////////
/////
///              Problem 2
///

// http://projecteuler.net/index.php?section=problems&id=2
//
// Each new term in the Fibonacci sequence is generated
// by adding the previous two terms.
// By starting with 1 and 2, the first 10 terms will be:
//
// 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
//
// Find the sum of all the even-valued terms in the sequence
// which do not exceed four million.

// http://odz.sakura.ne.jp/projecteuler/index.php?Problem%202
// フィボナッチ数列の項は前の2つの項の和である。
// 最初の2項を 1, 2 とすれば、最初の10項は以下の通りである。
//
//  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
//
// 数列の項が400万を超えない範囲で、偶数の項の総和を求めよ。
//

// package main
//
// import "fmt"
//
// func main() {
//   upper := 4000000
//   sum := 0
//   a, b := 1, 2
//   for {
//     if b < upper {
//       if (b % 2) == 0 {
//         sum += b
//       }
//       a, b = b, (a + b)
//     } else {
//       break
//     }
//   }
//   fmt.Println("total:", sum)
// }

package main

import "fmt"

func fibonacci(limit int, c chan int) {
	x, y := 1, 2
	for {
		if x < limit {
			if (x % 2) == 0 {
				c <- x
			}
			x, y = y, x+y
		} else {
			close(c)
			break
		}
	}
}

func main() {
	c := make(chan int, 4000000)
	go fibonacci(cap(c), c)
	sum := 0
	for i := range c {
		sum += i
	}
	fmt.Println(sum)
}
