* Routes

Checks how big the impact in total response time of looking up a route in a
a list of N routes is.

On my machine:


    benchmarking 10
    time                 13.13 ms   (12.76 ms .. 13.57 ms)
                         0.993 R²   (0.986 R² .. 0.997 R²)
    mean                 13.55 ms   (13.27 ms .. 13.92 ms)
    std dev              807.2 μs   (633.6 μs .. 996.4 μs)
    variance introduced by outliers: 26% (moderately inflated)

    benchmarking 50
    time                 14.56 ms   (14.26 ms .. 15.01 ms)
                         0.994 R²   (0.986 R² .. 0.998 R²)
    mean                 14.62 ms   (14.27 ms .. 14.93 ms)
    std dev              868.8 μs   (609.8 μs .. 1.306 ms)
    variance introduced by outliers: 27% (moderately inflated)

    benchmarking 250
    time                 18.69 ms   (17.93 ms .. 19.49 ms)
                         0.984 R²   (0.970 R² .. 0.993 R²)
    mean                 18.65 ms   (17.99 ms .. 19.27 ms)
    std dev              1.590 ms   (1.214 ms .. 2.122 ms)
    variance introduced by outliers: 39% (moderately inflated)
