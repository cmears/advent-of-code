import Control.Monad

-- my input: 172851-675869

passwords = do
  a <- [1..6]
  b <- [a..8]
  c <- [b..9]
  d <- [c..9]
  e <- [d..9]
  f <- [e..9]
  let n = 10^5*a + 10^4*b + 10^3*c + 10^2*d + 10^1*e + 10^0*f
  guard (172851 <= n && n <= 675869)
  guard $ or [           a == b && b /= c
             , a /= b && b == c && c /= d
             , b /= c && c == d && d /= e
             , c /= d && d == e && e /= f
             , d /= e && e == f
             ]
  guard (a <= b && b <= c && c <= d && d <= e && e <= f)
  pure n

main = print (length passwords)
