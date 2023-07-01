state
  x: Int;
  y: Int;

init := x = 0 && y = 0

step inc_x(a: Int)
  when 0 <= a && a <= 1 :=
    x' = x + a && y' = y


step inc_y(a: Int)
  when 0 <= a && a <= 1 :=
       x' = x && y' = y + a

step inc_both_nondet() :=
    x' - x = y' - y && x' > x && y' > y

req x_nonneg := 0 <= x
