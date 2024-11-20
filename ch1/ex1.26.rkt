#lang sicp

; by expanding out the square function you are now creating
; n^2 calls to expmod for that remainder code, which combined
; with the overall log(n) nature of the function,
; will result in linear (n) overall complexity

; Θ(log2^n) = Θ(n.log2) = Θ(n)