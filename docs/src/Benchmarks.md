## Benchmarks

The following benchmarks were conducted on SBCL / Linux / Core-i7 3.4GHz and Clozure / Darwin / Core 2 Duo 1.8GHz. On SBCL the new work-stealing model with lockless queues is enabled (the default on SBCL). Clozure uses the central queue model.

As with all sequence-related functions in lparallel, pmap and preduce acheive speedup by operating on chunks of sequences rather than on individual elements. psort uses a typical quicksort algorithm, which is naturally parallelizable. pfib (Fibonacci) and pmatrix-mul (matrix multiplication) use the fine-grained parallelism feature defpun.

All arrays are of type (simple-array single-float (*)). The rightmost column is time in microseconds.

SBCL / Linux / 4 cores

```
size     10 | op SIN      | MAP                 3
size     10 | op SIN      | MAP                 2
size     10 | op SIN      | MAP                 3
size     10 | op SIN      | MAP                 3
size     10 | op SIN      | PMAP               12
size     10 | op SIN      | PMAP               24
size     10 | op SIN      | PMAP               22
size     10 | op SIN      | PMAP               12

size     50 | op SIN      | MAP                 6
size     50 | op SIN      | MAP                 6
size     50 | op SIN      | MAP                 6
size     50 | op SIN      | MAP                 6
size     50 | op SIN      | PMAP               22
size     50 | op SIN      | PMAP               23
size     50 | op SIN      | PMAP               53
size     50 | op SIN      | PMAP               50

size    100 | op SIN      | MAP                10
size    100 | op SIN      | MAP                11
size    100 | op SIN      | MAP                11
size    100 | op SIN      | MAP                10
size    100 | op SIN      | PMAP               27
size    100 | op SIN      | PMAP               18
size    100 | op SIN      | PMAP               17
size    100 | op SIN      | PMAP               17

size    500 | op SIN      | MAP                59
size    500 | op SIN      | MAP                47
size    500 | op SIN      | MAP                51
size    500 | op SIN      | MAP                47
size    500 | op SIN      | PMAP               26
size    500 | op SIN      | PMAP               23
size    500 | op SIN      | PMAP               27
size    500 | op SIN      | PMAP               32

size   1000 | op SIN      | MAP                89
size   1000 | op SIN      | MAP                90
size   1000 | op SIN      | MAP                89
size   1000 | op SIN      | MAP                90
size   1000 | op SIN      | PMAP               31
size   1000 | op SIN      | PMAP               31
size   1000 | op SIN      | PMAP               36
size   1000 | op SIN      | PMAP               38

size   5000 | op SIN      | MAP               442
size   5000 | op SIN      | MAP               442
size   5000 | op SIN      | MAP               460
size   5000 | op SIN      | MAP               456
size   5000 | op SIN      | PMAP              128
size   5000 | op SIN      | PMAP              135
size   5000 | op SIN      | PMAP              134
size   5000 | op SIN      | PMAP              151

size  10000 | op SIN      | MAP               837
size  10000 | op SIN      | MAP               889
size  10000 | op SIN      | MAP               841
size  10000 | op SIN      | MAP               878
size  10000 | op SIN      | PMAP              196
size  10000 | op SIN      | PMAP              196
size  10000 | op SIN      | PMAP              198
size  10000 | op SIN      | PMAP              197

size  50000 | op SIN      | MAP              4224
size  50000 | op SIN      | MAP              4210
size  50000 | op SIN      | MAP              4228
size  50000 | op SIN      | MAP              4215
size  50000 | op SIN      | PMAP              947
size  50000 | op SIN      | PMAP              987
size  50000 | op SIN      | PMAP             1061
size  50000 | op SIN      | PMAP              953

size 100000 | op SIN      | MAP              9002
size 100000 | op SIN      | MAP              9025
size 100000 | op SIN      | MAP              9007
size 100000 | op SIN      | MAP              9116
size 100000 | op SIN      | PMAP             1976
size 100000 | op SIN      | PMAP             1989
size 100000 | op SIN      | PMAP             1970
size 100000 | op SIN      | PMAP             2005

size 500000 | op SIN      | MAP             44873
size 500000 | op SIN      | MAP             42728
size 500000 | op SIN      | MAP             45324
size 500000 | op SIN      | MAP             43025
size 500000 | op SIN      | PMAP             9092
size 500000 | op SIN      | PMAP             9131
size 500000 | op SIN      | PMAP             9055
size 500000 | op SIN      | PMAP             9021

size     10 | op +        | REDUCE              0
size     10 | op +        | REDUCE              0
size     10 | op +        | REDUCE              0
size     10 | op +        | REDUCE              1
size     10 | op +        | PREDUCE             5
size     10 | op +        | PREDUCE            14
size     10 | op +        | PREDUCE             6
size     10 | op +        | PREDUCE             5

size     50 | op +        | REDUCE              2
size     50 | op +        | REDUCE              3
size     50 | op +        | REDUCE              2
size     50 | op +        | REDUCE              1
size     50 | op +        | PREDUCE            16
size     50 | op +        | PREDUCE            10
size     50 | op +        | PREDUCE            10
size     50 | op +        | PREDUCE             9

size    100 | op +        | REDUCE              2
size    100 | op +        | REDUCE              3
size    100 | op +        | REDUCE              2
size    100 | op +        | REDUCE              3
size    100 | op +        | PREDUCE            11
size    100 | op +        | PREDUCE            11
size    100 | op +        | PREDUCE            10
size    100 | op +        | PREDUCE            11

size    500 | op +        | REDUCE             11
size    500 | op +        | REDUCE             14
size    500 | op +        | REDUCE             12
size    500 | op +        | REDUCE             13
size    500 | op +        | PREDUCE            12
size    500 | op +        | PREDUCE            11
size    500 | op +        | PREDUCE            11
size    500 | op +        | PREDUCE            11

size   1000 | op +        | REDUCE             24
size   1000 | op +        | REDUCE             24
size   1000 | op +        | REDUCE             23
size   1000 | op +        | REDUCE             24
size   1000 | op +        | PREDUCE            15
size   1000 | op +        | PREDUCE            21
size   1000 | op +        | PREDUCE            16
size   1000 | op +        | PREDUCE            15

size   5000 | op +        | REDUCE            115
size   5000 | op +        | REDUCE            119
size   5000 | op +        | REDUCE            116
size   5000 | op +        | REDUCE            116
size   5000 | op +        | PREDUCE            56
size   5000 | op +        | PREDUCE            57
size   5000 | op +        | PREDUCE            57
size   5000 | op +        | PREDUCE            61

size  10000 | op +        | REDUCE            233
size  10000 | op +        | REDUCE            232
size  10000 | op +        | REDUCE            233
size  10000 | op +        | REDUCE            233
size  10000 | op +        | PREDUCE           105
size  10000 | op +        | PREDUCE           111
size  10000 | op +        | PREDUCE           105
size  10000 | op +        | PREDUCE           102

size  50000 | op +        | REDUCE            990
size  50000 | op +        | REDUCE            993
size  50000 | op +        | REDUCE           1018
size  50000 | op +        | REDUCE            996
size  50000 | op +        | PREDUCE           501
size  50000 | op +        | PREDUCE           508
size  50000 | op +        | PREDUCE           486
size  50000 | op +        | PREDUCE           488

size 100000 | op +        | REDUCE           2025
size 100000 | op +        | REDUCE           2000
size 100000 | op +        | REDUCE           2044
size 100000 | op +        | REDUCE           1997
size 100000 | op +        | PREDUCE           975
size 100000 | op +        | PREDUCE           965
size 100000 | op +        | PREDUCE           941
size 100000 | op +        | PREDUCE           965

size 500000 | op +        | REDUCE          10076
size 500000 | op +        | REDUCE          10094
size 500000 | op +        | REDUCE          11394
size 500000 | op +        | REDUCE          10147
size 500000 | op +        | PREDUCE          4250
size 500000 | op +        | PREDUCE          2834
size 500000 | op +        | PREDUCE          2829
size 500000 | op +        | PREDUCE          5336

size     10 | op <        | SORT                2
size     10 | op <        | SORT                3
size     10 | op <        | SORT                2
size     10 | op <        | SORT                2
size     10 | op <        | PSORT               4
size     10 | op <        | PSORT               4
size     10 | op <        | PSORT               4
size     10 | op <        | PSORT               5

size     50 | op <        | SORT               16
size     50 | op <        | SORT               17
size     50 | op <        | SORT               17
size     50 | op <        | SORT               17
size     50 | op <        | PSORT              14
size     50 | op <        | PSORT              13
size     50 | op <        | PSORT              13
size     50 | op <        | PSORT              11

size    100 | op <        | SORT               41
size    100 | op <        | SORT               42
size    100 | op <        | SORT               40
size    100 | op <        | SORT               40
size    100 | op <        | PSORT              25
size    100 | op <        | PSORT              21
size    100 | op <        | PSORT              28
size    100 | op <        | PSORT              23

size    500 | op <        | SORT              284
size    500 | op <        | SORT              284
size    500 | op <        | SORT              295
size    500 | op <        | SORT              302
size    500 | op <        | PSORT              97
size    500 | op <        | PSORT              96
size    500 | op <        | PSORT              94
size    500 | op <        | PSORT             121

size   1000 | op <        | SORT              684
size   1000 | op <        | SORT              685
size   1000 | op <        | SORT              681
size   1000 | op <        | SORT              685
size   1000 | op <        | PSORT             274
size   1000 | op <        | PSORT             277
size   1000 | op <        | PSORT             288
size   1000 | op <        | PSORT             318

size   5000 | op <        | SORT             3952
size   5000 | op <        | SORT             3948
size   5000 | op <        | SORT             3949
size   5000 | op <        | SORT             4127
size   5000 | op <        | PSORT            1089
size   5000 | op <        | PSORT            1016
size   5000 | op <        | PSORT            2863
size   5000 | op <        | PSORT            1067

size  10000 | op <        | SORT             8605
size  10000 | op <        | SORT             8627
size  10000 | op <        | SORT             8618
size  10000 | op <        | SORT             8625
size  10000 | op <        | PSORT            2710
size  10000 | op <        | PSORT            2362
size  10000 | op <        | PSORT            2382
size  10000 | op <        | PSORT            2623

size  50000 | op <        | SORT            51234
size  50000 | op <        | SORT            52806
size  50000 | op <        | SORT            51456
size  50000 | op <        | SORT            52556
size  50000 | op <        | PSORT           14694
size  50000 | op <        | PSORT           21582
size  50000 | op <        | PSORT           19204
size  50000 | op <        | PSORT           13035

size 100000 | op <        | SORT           112340
size 100000 | op <        | SORT           110923
size 100000 | op <        | SORT           112489
size 100000 | op <        | SORT           110955
size 100000 | op <        | PSORT           41906
size 100000 | op <        | PSORT           44487
size 100000 | op <        | PSORT           44311
size 100000 | op <        | PSORT           42166

size 200000 | op <        | SORT           238384
size 200000 | op <        | SORT           238247
size 200000 | op <        | SORT           238630
size 200000 | op <        | SORT           238458
size 200000 | op <        | PSORT           59908
size 200000 | op <        | PSORT           59434
size 200000 | op <        | PSORT           63248
size 200000 | op <        | PSORT           61640

n      5 | FIB-LET                0
n      5 | FIB-LET                0
n      5 | FIB-LET                0
n      5 | FIB-LET                0
n      5 | FIB-PLET               3
n      5 | FIB-PLET               8
n      5 | FIB-PLET               1
n      5 | FIB-PLET               2
n      5 | FIB-PLET-IF            1
n      5 | FIB-PLET-IF            1
n      5 | FIB-PLET-IF            1
n      5 | FIB-PLET-IF            0

n     10 | FIB-LET                2
n     10 | FIB-LET                2
n     10 | FIB-LET                2
n     10 | FIB-LET                2
n     10 | FIB-PLET               2
n     10 | FIB-PLET               3
n     10 | FIB-PLET               4
n     10 | FIB-PLET               4
n     10 | FIB-PLET-IF            1
n     10 | FIB-PLET-IF            2
n     10 | FIB-PLET-IF            2
n     10 | FIB-PLET-IF            1

n     15 | FIB-LET               13
n     15 | FIB-LET               13
n     15 | FIB-LET               13
n     15 | FIB-LET               13
n     15 | FIB-PLET              10
n     15 | FIB-PLET              10
n     15 | FIB-PLET              10
n     15 | FIB-PLET              11
n     15 | FIB-PLET-IF           16
n     15 | FIB-PLET-IF           16
n     15 | FIB-PLET-IF           15
n     15 | FIB-PLET-IF           16

n     20 | FIB-LET              135
n     20 | FIB-LET              134
n     20 | FIB-LET              134
n     20 | FIB-LET              134
n     20 | FIB-PLET              43
n     20 | FIB-PLET              44
n     20 | FIB-PLET              48
n     20 | FIB-PLET              54
n     20 | FIB-PLET-IF           43
n     20 | FIB-PLET-IF           44
n     20 | FIB-PLET-IF           43
n     20 | FIB-PLET-IF           44

n     25 | FIB-LET             1493
n     25 | FIB-LET             1485
n     25 | FIB-LET             1499
n     25 | FIB-LET             1480
n     25 | FIB-PLET             442
n     25 | FIB-PLET             446
n     25 | FIB-PLET             444
n     25 | FIB-PLET             405
n     25 | FIB-PLET-IF          408
n     25 | FIB-PLET-IF          400
n     25 | FIB-PLET-IF          393
n     25 | FIB-PLET-IF          436

n     30 | FIB-LET            16044
n     30 | FIB-LET            16039
n     30 | FIB-LET            16039
n     30 | FIB-LET            16070
n     30 | FIB-PLET            4743
n     30 | FIB-PLET            4378
n     30 | FIB-PLET            4532
n     30 | FIB-PLET            5762
n     30 | FIB-PLET-IF         4372
n     30 | FIB-PLET-IF         4296
n     30 | FIB-PLET-IF         4356
n     30 | FIB-PLET-IF         4310

n     35 | FIB-LET           178913
n     35 | FIB-LET           180466
n     35 | FIB-LET           178761
n     35 | FIB-LET           178801
n     35 | FIB-PLET           46752
n     35 | FIB-PLET           48785
n     35 | FIB-PLET           49776
n     35 | FIB-PLET           46423
n     35 | FIB-PLET-IF        48273
n     35 | FIB-PLET-IF        48348
n     35 | FIB-PLET-IF        47367
n     35 | FIB-PLET-IF        47368

n      5 | MATRIX-MUL             4
n      5 | MATRIX-MUL             4
n      5 | MATRIX-MUL             4
n      5 | MATRIX-MUL             5
n      5 | PMATRIX-MUL            4
n      5 | PMATRIX-MUL            3
n      5 | PMATRIX-MUL            4
n      5 | PMATRIX-MUL            3

n     10 | MATRIX-MUL            21
n     10 | MATRIX-MUL            21
n     10 | MATRIX-MUL            22
n     10 | MATRIX-MUL            21
n     10 | PMATRIX-MUL            8
n     10 | PMATRIX-MUL           12
n     10 | PMATRIX-MUL            8
n     10 | PMATRIX-MUL           12

n     50 | MATRIX-MUL          2090
n     50 | MATRIX-MUL          2074
n     50 | MATRIX-MUL          2088
n     50 | MATRIX-MUL          2074
n     50 | PMATRIX-MUL          600
n     50 | PMATRIX-MUL          565
n     50 | PMATRIX-MUL          575
n     50 | PMATRIX-MUL          574

n    100 | MATRIX-MUL         16079
n    100 | MATRIX-MUL         16060
n    100 | MATRIX-MUL         15848
n    100 | MATRIX-MUL         15757
n    100 | PMATRIX-MUL         4280
n    100 | PMATRIX-MUL         4278
n    100 | PMATRIX-MUL         4278
n    100 | PMATRIX-MUL         4276

n    200 | MATRIX-MUL        124417
n    200 | MATRIX-MUL        123705
n    200 | MATRIX-MUL        126760
n    200 | MATRIX-MUL        124257
n    200 | PMATRIX-MUL        33465
n    200 | PMATRIX-MUL        33445
n    200 | PMATRIX-MUL        33444
n    200 | PMATRIX-MUL        33478
```

Clozure / Darwin / 2 cores

```
size     10 | op SIN      | MAP                19
size     10 | op SIN      | MAP                20
size     10 | op SIN      | MAP                19
size     10 | op SIN      | MAP                20
size     10 | op SIN      | PMAP              108
size     10 | op SIN      | PMAP              108
size     10 | op SIN      | PMAP               95
size     10 | op SIN      | PMAP               89

size     50 | op SIN      | MAP                29
size     50 | op SIN      | MAP                29
size     50 | op SIN      | MAP                29
size     50 | op SIN      | MAP                29
size     50 | op SIN      | PMAP               99
size     50 | op SIN      | PMAP              101
size     50 | op SIN      | PMAP               98
size     50 | op SIN      | PMAP              100

size    100 | op SIN      | MAP                42
size    100 | op SIN      | MAP                42
size    100 | op SIN      | MAP                42
size    100 | op SIN      | MAP                42
size    100 | op SIN      | PMAP              133
size    100 | op SIN      | PMAP              145
size    100 | op SIN      | PMAP              133
size    100 | op SIN      | PMAP              144

size    500 | op SIN      | MAP               143
size    500 | op SIN      | MAP               143
size    500 | op SIN      | MAP               143
size    500 | op SIN      | MAP               142
size    500 | op SIN      | PMAP              185
size    500 | op SIN      | PMAP              190
size    500 | op SIN      | PMAP              198
size    500 | op SIN      | PMAP              187

size   1000 | op SIN      | MAP               269
size   1000 | op SIN      | MAP               270
size   1000 | op SIN      | MAP               270
size   1000 | op SIN      | MAP               270
size   1000 | op SIN      | PMAP              255
size   1000 | op SIN      | PMAP              237
size   1000 | op SIN      | PMAP              254
size   1000 | op SIN      | PMAP              263

size   5000 | op SIN      | MAP              1480
size   5000 | op SIN      | MAP              1539
size   5000 | op SIN      | MAP              1553
size   5000 | op SIN      | MAP              1554
size   5000 | op SIN      | PMAP              786
size   5000 | op SIN      | PMAP              779
size   5000 | op SIN      | PMAP              781
size   5000 | op SIN      | PMAP              797

size  10000 | op SIN      | MAP              2741
size  10000 | op SIN      | MAP              2686
size  10000 | op SIN      | MAP              2761
size  10000 | op SIN      | MAP              2746
size  10000 | op SIN      | PMAP             1449
size  10000 | op SIN      | PMAP             1468
size  10000 | op SIN      | PMAP             1635
size  10000 | op SIN      | PMAP             1479

size  50000 | op SIN      | MAP             18660
size  50000 | op SIN      | MAP             13311
size  50000 | op SIN      | MAP             18647
size  50000 | op SIN      | MAP             13354
size  50000 | op SIN      | PMAP             7002
size  50000 | op SIN      | PMAP             6935
size  50000 | op SIN      | PMAP             9493
size  50000 | op SIN      | PMAP             6949

size 100000 | op SIN      | MAP             35478
size 100000 | op SIN      | MAP             35885
size 100000 | op SIN      | MAP             36514
size 100000 | op SIN      | MAP             34902
size 100000 | op SIN      | PMAP            13699
size 100000 | op SIN      | PMAP            13776
size 100000 | op SIN      | PMAP            13711
size 100000 | op SIN      | PMAP            16364

size 500000 | op SIN      | MAP            279260
size 500000 | op SIN      | MAP            162787
size 500000 | op SIN      | MAP            170616
size 500000 | op SIN      | MAP            191481
size 500000 | op SIN      | PMAP            68104
size 500000 | op SIN      | PMAP            73681
size 500000 | op SIN      | PMAP            68150
size 500000 | op SIN      | PMAP            73888

size     10 | op +        | REDUCE              1
size     10 | op +        | REDUCE              1
size     10 | op +        | REDUCE              1
size     10 | op +        | REDUCE              1
size     10 | op +        | PREDUCE            77
size     10 | op +        | PREDUCE            72
size     10 | op +        | PREDUCE            67
size     10 | op +        | PREDUCE            78

size     50 | op +        | REDUCE              4
size     50 | op +        | REDUCE              4
size     50 | op +        | REDUCE              4
size     50 | op +        | REDUCE              4
size     50 | op +        | PREDUCE            51
size     50 | op +        | PREDUCE            79
size     50 | op +        | PREDUCE            51
size     50 | op +        | PREDUCE            80

size    100 | op +        | REDUCE              7
size    100 | op +        | REDUCE              7
size    100 | op +        | REDUCE              8
size    100 | op +        | REDUCE              8
size    100 | op +        | PREDUCE            64
size    100 | op +        | PREDUCE            71
size    100 | op +        | PREDUCE            64
size    100 | op +        | PREDUCE            71

size    500 | op +        | REDUCE             37
size    500 | op +        | REDUCE             37
size    500 | op +        | REDUCE             37
size    500 | op +        | REDUCE             37
size    500 | op +        | PREDUCE           109
size    500 | op +        | PREDUCE            99
size    500 | op +        | PREDUCE            99
size    500 | op +        | PREDUCE            99

size   1000 | op +        | REDUCE             74
size   1000 | op +        | REDUCE             74
size   1000 | op +        | REDUCE             74
size   1000 | op +        | REDUCE             74
size   1000 | op +        | PREDUCE           128
size   1000 | op +        | PREDUCE           117
size   1000 | op +        | PREDUCE           118
size   1000 | op +        | PREDUCE           126

size   5000 | op +        | REDUCE            368
size   5000 | op +        | REDUCE            408
size   5000 | op +        | REDUCE            368
size   5000 | op +        | REDUCE            368
size   5000 | op +        | PREDUCE           340
size   5000 | op +        | PREDUCE           265
size   5000 | op +        | PREDUCE           247
size   5000 | op +        | PREDUCE           274

size  10000 | op +        | REDUCE            730
size  10000 | op +        | REDUCE            730
size  10000 | op +        | REDUCE            730
size  10000 | op +        | REDUCE            715
size  10000 | op +        | PREDUCE           447
size  10000 | op +        | PREDUCE           434
size  10000 | op +        | PREDUCE           447
size  10000 | op +        | PREDUCE           433

size  50000 | op +        | REDUCE           3674
size  50000 | op +        | REDUCE           3686
size  50000 | op +        | REDUCE           3674
size  50000 | op +        | REDUCE           3684
size  50000 | op +        | PREDUCE          1897
size  50000 | op +        | PREDUCE          1904
size  50000 | op +        | PREDUCE          1896
size  50000 | op +        | PREDUCE          1944

size 100000 | op +        | REDUCE           7368
size 100000 | op +        | REDUCE           7370
size 100000 | op +        | REDUCE           7370
size 100000 | op +        | REDUCE           7368
size 100000 | op +        | PREDUCE          3753
size 100000 | op +        | PREDUCE          3791
size 100000 | op +        | PREDUCE          3743
size 100000 | op +        | PREDUCE          3741

size 500000 | op +        | REDUCE          37027
size 500000 | op +        | REDUCE          36895
size 500000 | op +        | REDUCE          36891
size 500000 | op +        | REDUCE          36884
size 500000 | op +        | PREDUCE         18630
size 500000 | op +        | PREDUCE         18570
size 500000 | op +        | PREDUCE         18685
size 500000 | op +        | PREDUCE         18592

size     10 | op <        | SORT                3
size     10 | op <        | SORT                3
size     10 | op <        | SORT                3
size     10 | op <        | SORT                3
size     10 | op <        | PSORT              99
size     10 | op <        | PSORT              95
size     10 | op <        | PSORT             101
size     10 | op <        | PSORT              95

size     50 | op <        | SORT               24
size     50 | op <        | SORT               23
size     50 | op <        | SORT               23
size     50 | op <        | SORT               24
size     50 | op <        | PSORT             123
size     50 | op <        | PSORT             123
size     50 | op <        | PSORT             123
size     50 | op <        | PSORT             131

size    100 | op <        | SORT               61
size    100 | op <        | SORT               61
size    100 | op <        | SORT               61
size    100 | op <        | SORT               61
size    100 | op <        | PSORT             176
size    100 | op <        | PSORT             187
size    100 | op <        | PSORT             193
size    100 | op <        | PSORT             188

size    500 | op <        | SORT              452
size    500 | op <        | SORT              448
size    500 | op <        | SORT              449
size    500 | op <        | SORT              455
size    500 | op <        | PSORT             362
size    500 | op <        | PSORT             363
size    500 | op <        | PSORT             360
size    500 | op <        | PSORT             362

size   1000 | op <        | SORT             1031
size   1000 | op <        | SORT             1036
size   1000 | op <        | SORT             1032
size   1000 | op <        | SORT             1032
size   1000 | op <        | PSORT             643
size   1000 | op <        | PSORT             648
size   1000 | op <        | PSORT             647
size   1000 | op <        | PSORT             646

size   5000 | op <        | SORT             6928
size   5000 | op <        | SORT             6919
size   5000 | op <        | SORT             6919
size   5000 | op <        | SORT             6934
size   5000 | op <        | PSORT            4167
size   5000 | op <        | PSORT            4166
size   5000 | op <        | PSORT            4177
size   5000 | op <        | PSORT            4173

size  10000 | op <        | SORT            13865
size  10000 | op <        | SORT            13931
size  10000 | op <        | SORT            13860
size  10000 | op <        | SORT            13857
size  10000 | op <        | PSORT            8933
size  10000 | op <        | PSORT            8925
size  10000 | op <        | PSORT            8904
size  10000 | op <        | PSORT            8942

size  50000 | op <        | SORT            84698
size  50000 | op <        | SORT            83833
size  50000 | op <        | SORT            83882
size  50000 | op <        | SORT            83927
size  50000 | op <        | PSORT           54610
size  50000 | op <        | PSORT           54669
size  50000 | op <        | PSORT           54475
size  50000 | op <        | PSORT           54613

size 100000 | op <        | SORT           179542
size 100000 | op <        | SORT           179620
size 100000 | op <        | SORT           179544
size 100000 | op <        | SORT           179628
size 100000 | op <        | PSORT          127452
size 100000 | op <        | PSORT          127176
size 100000 | op <        | PSORT          127589
size 100000 | op <        | PSORT          126974

size 200000 | op <        | SORT           373940
size 200000 | op <        | SORT           373704
size 200000 | op <        | SORT           373814
size 200000 | op <        | SORT           373912
size 200000 | op <        | PSORT          198871
size 200000 | op <        | PSORT          199304
size 200000 | op <        | PSORT          198842
size 200000 | op <        | PSORT          198844

n      5 | FIB-LET                0
n      5 | FIB-LET                1
n      5 | FIB-LET                0
n      5 | FIB-LET                0
n      5 | FIB-PLET             104
n      5 | FIB-PLET             109
n      5 | FIB-PLET              97
n      5 | FIB-PLET             100
n      5 | FIB-PLET-IF            0
n      5 | FIB-PLET-IF            0
n      5 | FIB-PLET-IF            0
n      5 | FIB-PLET-IF            1

n     10 | FIB-LET                2
n     10 | FIB-LET                3
n     10 | FIB-LET                2
n     10 | FIB-LET                2
n     10 | FIB-PLET              97
n     10 | FIB-PLET              97
n     10 | FIB-PLET              98
n     10 | FIB-PLET              98
n     10 | FIB-PLET-IF            3
n     10 | FIB-PLET-IF            3
n     10 | FIB-PLET-IF            3
n     10 | FIB-PLET-IF            3

n     15 | FIB-LET               25
n     15 | FIB-LET               25
n     15 | FIB-LET               26
n     15 | FIB-LET               26
n     15 | FIB-PLET             259
n     15 | FIB-PLET             184
n     15 | FIB-PLET             220
n     15 | FIB-PLET              75
n     15 | FIB-PLET-IF           27
n     15 | FIB-PLET-IF           27
n     15 | FIB-PLET-IF           27
n     15 | FIB-PLET-IF           31

n     20 | FIB-LET              277
n     20 | FIB-LET              277
n     20 | FIB-LET              277
n     20 | FIB-LET              277
n     20 | FIB-PLET             569
n     20 | FIB-PLET             458
n     20 | FIB-PLET             894
n     20 | FIB-PLET             408
n     20 | FIB-PLET-IF          210
n     20 | FIB-PLET-IF          208
n     20 | FIB-PLET-IF          211
n     20 | FIB-PLET-IF          208

n     25 | FIB-LET             3061
n     25 | FIB-LET             3058
n     25 | FIB-LET             3059
n     25 | FIB-LET             3062
n     25 | FIB-PLET            2101
n     25 | FIB-PLET            2728
n     25 | FIB-PLET            2205
n     25 | FIB-PLET            1989
n     25 | FIB-PLET-IF         1760
n     25 | FIB-PLET-IF         1925
n     25 | FIB-PLET-IF         1832
n     25 | FIB-PLET-IF         1797

n     30 | FIB-LET            34010
n     30 | FIB-LET            33998
n     30 | FIB-LET            33929
n     30 | FIB-LET            34031
n     30 | FIB-PLET           19112
n     30 | FIB-PLET           19656
n     30 | FIB-PLET           19118
n     30 | FIB-PLET           22719
n     30 | FIB-PLET-IF        19280
n     30 | FIB-PLET-IF        22369
n     30 | FIB-PLET-IF        19268
n     30 | FIB-PLET-IF        18161

n     35 | FIB-LET           376509
n     35 | FIB-LET           375270
n     35 | FIB-LET           375297
n     35 | FIB-LET           375402
n     35 | FIB-PLET          207037
n     35 | FIB-PLET          209237
n     35 | FIB-PLET          195259
n     35 | FIB-PLET          204691
n     35 | FIB-PLET-IF       183718
n     35 | FIB-PLET-IF       192542
n     35 | FIB-PLET-IF       185317
n     35 | FIB-PLET-IF       185194

n      5 | MATRIX-MUL             8
n      5 | MATRIX-MUL             8
n      5 | MATRIX-MUL             8
n      5 | MATRIX-MUL             9
n      5 | PMATRIX-MUL           77
n      5 | PMATRIX-MUL           88
n      5 | PMATRIX-MUL           96
n      5 | PMATRIX-MUL           96

n     10 | MATRIX-MUL            50
n     10 | MATRIX-MUL            50
n     10 | MATRIX-MUL            50
n     10 | MATRIX-MUL            51
n     10 | PMATRIX-MUL          118
n     10 | PMATRIX-MUL           90
n     10 | PMATRIX-MUL          119
n     10 | PMATRIX-MUL          114

n     50 | MATRIX-MUL          4767
n     50 | MATRIX-MUL          4844
n     50 | MATRIX-MUL          4733
n     50 | MATRIX-MUL          4832
n     50 | PMATRIX-MUL         2570
n     50 | PMATRIX-MUL         2471
n     50 | PMATRIX-MUL         6237
n     50 | PMATRIX-MUL         2454

n    100 | MATRIX-MUL         39091
n    100 | MATRIX-MUL         36697
n    100 | MATRIX-MUL         36823
n    100 | MATRIX-MUL         36829
n    100 | PMATRIX-MUL        18725
n    100 | PMATRIX-MUL        21860
n    100 | PMATRIX-MUL        18701
n    100 | PMATRIX-MUL        18679

n    200 | MATRIX-MUL        286528
n    200 | MATRIX-MUL        288732
n    200 | MATRIX-MUL        286697
n    200 | MATRIX-MUL        288674
n    200 | PMATRIX-MUL       166732
n    200 | PMATRIX-MUL       150359
n    200 | PMATRIX-MUL       155815
n    200 | PMATRIX-MUL       154714
```
