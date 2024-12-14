# Part 2
Using `_print_maps` I found 2 recurring patterns:
```
start 76 every 103: kind of like a raindeer
start 14 every 101: a pillar in the middle
```

The cycles make sense as they are the diminsions of the map.

So my best guess is when they converge.

To work that out, I notice that first sequence advances 2 faster than the second.

So we need to advance the second one first to be ahead:

```
start 76 cycle 103
start 115 cycle 101
```

Then subtract the distance `115 - 76 = 39`. This is not even so we'll advance another cycle.

```
start 76 cycle 103
start 216 cycle 101
```
Then subtract the distance `216 - 76 = 140`, so it'll take further 70 cycles.

So convergence happens at `216 + 101 x 70 = 7286`
