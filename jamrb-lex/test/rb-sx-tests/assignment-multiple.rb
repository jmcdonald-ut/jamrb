foo, baz, bar = [1, 2, 3] # foo = 1, baz = 2, bar = 3
arr = 1, 2, 3, 4, 5, 6

a, b, *c, d = [1, 2, 3, 4, 5, 6]

# Decomposition
jon, (alexander, mcdonald), jam = 1, [2, 3, 5, 6], 4
billy, (bob, sam), *joel = 1, [2, 3, 5, 6], 4
walt, (mickey, *mouse), *frank = 1, [2, 3, 4, 5, 6, 7], 9, 9, 10
oh, (so, it), *has = 1, [2, 3]

# Splat!
e = *[1, 2, 3] # ironically sets `e` to [1, 2, 3]
