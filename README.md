# riemann

Toying with [Neo-Riemannian music theory](https://en.wikipedia.org/wiki/Neo-Riemannian_theory).

Motivating question: what would happen if you took some chord-progression in tonnetz (e.g.
[this](https://www.youtube.com/watch?v=NQ7LkWCzKxI)), and rotated it?

Preliminary results: Nothing *too* magical, but maybe interesting:

```
## circle-esque:
          Dm  Gm  C   F   Bb  Em  A   D
Rotate1   Dm  F#m F#  Bb  D   C#m C#  F
Rotate2   Dm  Fm  B   D   F   C#m G   Bb
Swap1     Dm  Am  Eb  Bb  F   C#m G   D
Swap2     Dm  Bbm F#  D   Bb  C#m A   F

## north korea:
          Dm  D   Gm  F   A   Dm
Rotate1   Dm  F   F#m Bb  C#  Dm
Rotate2   Dm  Bb  Fm  D   G   Dm
Swap1     Dm  D   Am  Bb  G   Dm
Swap2     Dm  F   Bbm D   A   Dm

## neapolitan:
          Dm  Eb  A
Rotate1   Dm  F#  C#
Rotate2   Dm  G#  G
Swap1     Dm  C   G
Swap2     Dm  F#  A

## WC2 Human 1:
          Dm  E   Eb  D   Dm  Gm  A   Dm
Rotate1   Dm  E   F#  F   Dm  F#m C#  Dm
Rotate2   Dm  A   G#  Bb  Dm  Fm  G   Dm
Swap1     Dm  C#  C   D   Dm  Am  G   Dm
Swap2     Dm  E   F#  F   Dm  Bbm A   Dm
```
