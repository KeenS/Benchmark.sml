fun fib n = if n <= 1 then 1 else (fib (n - 1)) + (fib (n - 2))
val _ = Benchmark.benchset "fib" 1 [
    ("24" , (fn _ => fib 24)),
    ("32" , (fn _ => fib 32)),
    ("40" , (fn _ => fib 40))
]
