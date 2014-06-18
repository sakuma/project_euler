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
