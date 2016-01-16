a = 9
puts "a: 9 == #{a}"

b = 3
b += a
puts "b: 12 == #{b}"

c = 4
c *= 3
puts "c: 12 == #{c}"

d = 49
d /= 7
puts "d: 7 == #{d}"

e = 5
e **= 2
puts "e: 25 == #{e}"

f = 100
f <<= 1
puts "f: 200 == #{f}"

g = 100
g >>= 1
puts "g: 50 == #{g}"

h = 7
h ^= 3
puts "h: 4 == #{h}"

i = 30
i %= 4
puts "i: 2 == #{i}"

j = 28
j -= 4
puts "j: 24 == #{j}"

k = 3
k |= 9
puts "k: 11 == #{k}"

l = false
l ||= 24
puts "l: 24 == #{l}"

m = true
m &&= 24
puts "m: 24 == #{m}"
