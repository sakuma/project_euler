// #  http://odz.sakura.ne.jp/projecteuler/index.php?Problem%201
// #
// #  10未満の自然数のうち、3 もしくは 5 の倍数になっているものは
// #  3, 5, 6, 9 の4つがあり、これらの合計は 23 になる。
// #
// #  同じようにして、1,000 未満の 3 か 5 の倍数になっている数字の合計を求めよ。
package main

import (
	"fmt"
	"math"
)

func main() {
	var sum float64 = 0
	upper := 1000.0
	for i := 1.0; i < upper; i++ {
		if math.Mod(i, 3) == 0 {
			sum += i
		} else if math.Mod(i, 5) == 0 {
			sum += i
		}
	}

	fmt.Println("total: ", sum)
}

// total:  233168
