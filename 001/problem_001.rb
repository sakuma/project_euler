#  http://odz.sakura.ne.jp/projecteuler/index.php?Problem%201
#
#  10未満の自然数のうち、3 もしくは 5 の倍数になっているものは
#  3, 5, 6, 9 の4つがあり、これらの合計は 23 になる。
#
#  同じようにして、1,000 未満の 3 か 5 の倍数になっている数字の合計を求めよ。
total = 0
# upper = 10
upper = 1_000
(1...upper).each do |i|
  if (i%3).zero?
    total += i
  elsif (i%5).zero?
    total += i
  end
end
puts total

# total:  233168
