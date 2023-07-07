state
  b: Array Int Int;
  sum: Int;
  x: Int;
  sum0: Int;

init :=
  x = 0 && b[x] >= 0 && sum = sum0

step transfer(from: Int, to: Int, v: Int) when 0 <= v && v <= b[from] :=
  b' = b[from := b[from] - v][to := b[from := b[from] - v][to] + v] &&
  # How to automate this?
  sum' = (((sum - b[to]) - b[from]) + b'[to]) + b'[from] &&
  x' = x &&
  sum0' = sum0

req x_nonneg :=
  0 <= b[x]

req sum_constant :=
  sum0 = sum
