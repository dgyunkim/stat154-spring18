Lab 01: Basics of Matrix Algebra in R
================
Donggyun Kim
1/19/2018

1) Basic Vector and Matrix manipulations in R
=============================================

A vector x

``` r
x <- 1:9
```

Use the vector x to make matrices

``` r
matrix(x, nrow = 3, ncol = 3) # column-major
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    4    7
    ## [2,]    2    5    8
    ## [3,]    3    6    9

``` r
matrix(x, nrow = 3, ncol = 3, byrow = 1) # row-major
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    2    3
    ## [2,]    4    5    6
    ## [3,]    7    8    9

Identity matrix 5x5

``` r
diag(5)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    0    0    0    0
    ## [2,]    0    1    0    0    0
    ## [3,]    0    0    1    0    0
    ## [4,]    0    0    0    1    0
    ## [5,]    0    0    0    0    1

Three vectors a1, a2, and a3

``` r
a1 <- c(2, 3, 6, 7, 10)
a2 <- c(1.88, 2.05, 1.70, 1.60, 1.78)
a3 <- c(80, 90, 70, 50, 75)
```

Use `cbind()` to form matrix 5x3

``` r
A <- cbind(a1, a2, a3)
rownames(A) <- 1:5
A
```

    ##   a1   a2 a3
    ## 1  2 1.88 80
    ## 2  3 2.05 90
    ## 3  6 1.70 70
    ## 4  7 1.60 50
    ## 5 10 1.78 75

Three vectors b1, b2, and b3

``` r
b1 <- c(1, 4, 5, 8, 9)
b2 <- c(1.22, 1.05, 3.60, 0.40, 2.54)
b3 <- c(20, 40, 30, 80, 100)
```

Use `rbind()` to form matrix 3x5

``` r
B <- rbind(b1, b2, b3)
colnames(B) <- 1:5
B
```

    ##        1     2    3    4      5
    ## b1  1.00  4.00  5.0  8.0   9.00
    ## b2  1.22  1.05  3.6  0.4   2.54
    ## b3 20.00 40.00 30.0 80.0 100.00

Use `%*%` to compute matrix products

``` r
A %*% B
```

    ##          1        2        3        4        5
    ## 1 1604.294 3209.974 2416.768 6416.752 8022.775
    ## 2 1805.501 3614.153 2722.380 7224.820 9032.207
    ## 3 1408.074 2825.785 2136.120 5648.680 7058.318
    ## 4 1008.952 2029.680 1540.760 4056.640 5067.064
    ## 5 1512.172 3041.869 2306.408 6080.712 7594.521

``` r
B %*% A
```

    ##         a1       a2      a3
    ## b1  190.00  47.4000  1865.0
    ## b2   55.39  15.7273   654.6
    ## b3 1900.00 476.6000 18800.0

``` r
t(A) %*% t(B)
```

    ##        b1       b2      b3
    ## a1  190.0  55.3900  1900.0
    ## a2   47.4  15.7273   476.6
    ## a3 1865.0 654.6000 18800.0

``` r
t(B) %*% t(A)
```

    ##          1        2        3        4        5
    ## 1 1604.294 1805.501 1408.074 1008.952 1512.172
    ## 2 3209.974 3614.153 2825.785 2029.680 3041.869
    ## 3 2416.768 2722.380 2136.120 1540.760 2306.408
    ## 4 6416.752 7224.820 5648.680 4056.640 6080.712
    ## 5 8022.775 9032.207 7058.318 5067.064 7594.521

Use the data frame "iris" to compute a linear combination

``` r
library(dplyr) # load a package
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
dat <- select(iris, 1:4) # need first 4 columns of data frame "iris"

mat <- as.matrix(dat) # convert data frame into matrix

rownames(mat) <- 1:nrow(mat)

head(mat, n = 10)
```

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width
    ## 1           5.1         3.5          1.4         0.2
    ## 2           4.9         3.0          1.4         0.2
    ## 3           4.7         3.2          1.3         0.2
    ## 4           4.6         3.1          1.5         0.2
    ## 5           5.0         3.6          1.4         0.2
    ## 6           5.4         3.9          1.7         0.4
    ## 7           4.6         3.4          1.4         0.3
    ## 8           5.0         3.4          1.5         0.2
    ## 9           4.4         2.9          1.4         0.2
    ## 10          4.9         3.1          1.5         0.1

``` r
v <- 1:4 # create numeric vector to compute a linear combination

A <- mat %*% v

col_name <- paste(v, colnames(mat), sep = "*")

colnames(A) <- paste(col_name, collapse = " + ")

head(A, n = 10)
```

    ##    1*Sepal.Length + 2*Sepal.Width + 3*Petal.Length + 4*Petal.Width
    ## 1                                                             17.1
    ## 2                                                             15.9
    ## 3                                                             15.8
    ## 4                                                             16.1
    ## 5                                                             17.2
    ## 6                                                             19.9
    ## 7                                                             16.8
    ## 8                                                             17.1
    ## 9                                                             15.2
    ## 10                                                            16.0

Use `t()` and `%*%` to write a function `vnorm()` that computes l2 norm

``` r
vnorm <- function(x) {
  as.vector(sqrt(t(x) %*% x)) # convert a matrix into a vector
}

v <- 1:5
vnorm(v)
```

    ## [1] 7.416198

Use `vnorm()` to create a unit vectro u

``` r
v
```

    ## [1] 1 2 3 4 5

``` r
u <- v / vnorm(v)
u
```

    ## [1] 0.1348400 0.2696799 0.4045199 0.5393599 0.6741999

``` r
vnorm(u)
```

    ## [1] 1

Write a function `is_square()` to test whether a matrix is a square matrix

``` r
is_square <- function(X) {
  if (!is.matrix(X)) {
    stop("input must be a matrix")
  }
  
  if (ncol(X) == nrow(X)) {
    TRUE
  } else {
    FALSE
  }
}

I <- diag(3)
I
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    0    0
    ## [2,]    0    1    0
    ## [3,]    0    0    1

``` r
is_square(I)
```

    ## [1] TRUE

``` r
A <- matrix(1:6, ncol = 2, nrow = 3)
A
```

    ##      [,1] [,2]
    ## [1,]    1    4
    ## [2,]    2    5
    ## [3,]    3    6

``` r
is_square(A)
```

    ## [1] FALSE

Write a function `mtrace()` to compute the trace of a square matrix

``` r
mtrace <- function(X) {
  if(!is_square(X)) {
    stop("The matrix must be a square matrix")
  }
  
  sum(diag(X))
}

A <- matrix(1:9, ncol = 3, nrow = 3)
A
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    4    7
    ## [2,]    2    5    8
    ## [3,]    3    6    9

``` r
mtrace(A)
```

    ## [1] 15

Given two square matrices A and B

``` r
A <- matrix(1:9, ncol = 3, nrow = 3)
B <- diag(3)
A
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    4    7
    ## [2,]    2    5    8
    ## [3,]    3    6    9

``` r
B
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    0    0
    ## [2,]    0    1    0
    ## [3,]    0    0    1

``` r
C <- A + B
D <- 5 * A

mtrace(C) == mtrace(A) + mtrace(B)
```

    ## [1] TRUE

``` r
mtrace(D) == 5 * mtrace(A)
```

    ## [1] TRUE

Trace of products

``` r
X <- matrix(1:6, nrow = 2, ncol = 3)
Y <- matrix(11:16, nrow = 2, ncol = 3)
X
```

    ##      [,1] [,2] [,3]
    ## [1,]    1    3    5
    ## [2,]    2    4    6

``` r
Y
```

    ##      [,1] [,2] [,3]
    ## [1,]   11   13   15
    ## [2,]   12   14   16

``` r
tXY <- crossprod(X, Y)
XtY <- tcrossprod(X, Y)
tYX <- crossprod(Y, X)
YtX <- tcrossprod(Y, X)

mtrace(tXY) == mtrace(XtY)
```

    ## [1] TRUE

``` r
mtrace(XtY) == mtrace(tYX)
```

    ## [1] TRUE

``` r
mtrace(tYX) == mtrace(YtX)
```

    ## [1] TRUE

``` r
mtrace(YtX) == mtrace(tXY)
```

    ## [1] TRUE

------------------------------------------------------------------------

2) Transformation and Scaling Operations
========================================

Create a matrix M

``` r
M <- mtcars %>% select(mpg, disp, hp, drat, wt)
M <- as.matrix(M)
```

Use `apply()` to compute the vector containing the means of the columns in M

``` r
M_mean <- apply(M, 2, mean)
names(M_mean) <- paste("mean", names(M_mean), sep = "_")
M_mean
```

    ##   mean_mpg  mean_disp    mean_hp  mean_drat    mean_wt 
    ##  20.090625 230.721875 146.687500   3.596563   3.217250

A matrix Mc of mean-centered data

``` r
Mc <- scale(M, center = TRUE, scale = FALSE)
head(Mc, n = 10)
```

    ##                         mpg        disp       hp       drat       wt
    ## Mazda RX4          0.909375  -70.721875 -36.6875  0.3034375 -0.59725
    ## Mazda RX4 Wag      0.909375  -70.721875 -36.6875  0.3034375 -0.34225
    ## Datsun 710         2.709375 -122.721875 -53.6875  0.2534375 -0.89725
    ## Hornet 4 Drive     1.309375   27.278125 -36.6875 -0.5165625 -0.00225
    ## Hornet Sportabout -1.390625  129.278125  28.3125 -0.4465625  0.22275
    ## Valiant           -1.990625   -5.721875 -41.6875 -0.8365625  0.24275
    ## Duster 360        -5.790625  129.278125  98.3125 -0.3865625  0.35275
    ## Merc 240D          4.309375  -84.021875 -84.6875  0.0934375 -0.02725
    ## Merc 230           2.709375  -89.921875 -51.6875  0.3234375 -0.06725
    ## Merc 280          -0.890625  -63.121875 -23.6875  0.3234375  0.22275

``` r
Mc_mean <- apply(Mc, 2, mean)
names(Mc_mean) <- paste("mean", names(Mc_mean), sep = "_")
Mc_mean
```

    ##      mean_mpg     mean_disp       mean_hp     mean_drat       mean_wt 
    ##  4.440892e-16 -1.199041e-14  0.000000e+00 -1.526557e-16  3.469447e-17

Use `sweep()` to mean-center M by sweeping out the vector of column means

``` r
sweep_M <- sweep(M, 2, apply(M, 2, mean), FUN = "-")
head(Mc == sweep_M, n = 10)
```

    ##                    mpg disp   hp drat   wt
    ## Mazda RX4         TRUE TRUE TRUE TRUE TRUE
    ## Mazda RX4 Wag     TRUE TRUE TRUE TRUE TRUE
    ## Datsun 710        TRUE TRUE TRUE TRUE TRUE
    ## Hornet 4 Drive    TRUE TRUE TRUE TRUE TRUE
    ## Hornet Sportabout TRUE TRUE TRUE TRUE TRUE
    ## Valiant           TRUE TRUE TRUE TRUE TRUE
    ## Duster 360        TRUE TRUE TRUE TRUE TRUE
    ## Merc 240D         TRUE TRUE TRUE TRUE TRUE
    ## Merc 230          TRUE TRUE TRUE TRUE TRUE
    ## Merc 280          TRUE TRUE TRUE TRUE TRUE

Compute a vector of column maxima from M

``` r
M_max <- apply(M, 2, max)
names(M_max) <- paste("max", names(M_max), sep = "_")
M_max
```

    ##  max_mpg max_disp   max_hp max_drat   max_wt 
    ##   33.900  472.000  335.000    4.930    5.424

Use `sweep()` to scale the columns of M by dividing by the column maxima

``` r
head(sweep(M, 2, M_max, FUN = "/"), n = 10)
```

    ##                         mpg      disp        hp      drat        wt
    ## Mazda RX4         0.6194690 0.3389831 0.3283582 0.7910751 0.4830383
    ## Mazda RX4 Wag     0.6194690 0.3389831 0.3283582 0.7910751 0.5300516
    ## Datsun 710        0.6725664 0.2288136 0.2776119 0.7809331 0.4277286
    ## Hornet 4 Drive    0.6312684 0.5466102 0.3283582 0.6247465 0.5927360
    ## Hornet Sportabout 0.5516224 0.7627119 0.5223881 0.6389452 0.6342183
    ## Valiant           0.5339233 0.4766949 0.3134328 0.5598377 0.6379056
    ## Duster 360        0.4218289 0.7627119 0.7313433 0.6511156 0.6581858
    ## Merc 240D         0.7197640 0.3108051 0.1850746 0.7484787 0.5881268
    ## Merc 230          0.6725664 0.2983051 0.2835821 0.7951318 0.5807522
    ## Merc 280          0.5663717 0.3550847 0.3671642 0.7951318 0.6342183

Compute a matrix in which all columns of M are scaled shuch that they have minimum = 0 and maximum = 1

``` r
M_min <- apply(M, 2, min)
max_min <- M_max - M_min
M1 <- sweep(M, 2, M_min, FUN = "-")
M2 <- sweep(M1, 2, max_min, FUN = "/")
head(M2, n = 10)
```

    ##                         mpg      disp         hp      drat        wt
    ## Mazda RX4         0.4510638 0.2217511 0.20494700 0.5253456 0.2830478
    ## Mazda RX4 Wag     0.4510638 0.2217511 0.20494700 0.5253456 0.3482485
    ## Datsun 710        0.5276596 0.0920429 0.14487633 0.5023041 0.2063411
    ## Hornet 4 Drive    0.4680851 0.4662010 0.20494700 0.1474654 0.4351828
    ## Hornet Sportabout 0.3531915 0.7206286 0.43462898 0.1797235 0.4927129
    ## Valiant           0.3276596 0.3838863 0.18727915 0.0000000 0.4978266
    ## Duster 360        0.1659574 0.7206286 0.68197880 0.2073733 0.5259524
    ## Merc 240D         0.5957447 0.1885757 0.03533569 0.4285714 0.4287906
    ## Merc 230          0.5276596 0.1738588 0.15194346 0.5345622 0.4185630
    ## Merc 280          0.3744681 0.2407084 0.25088339 0.5345622 0.4927129

``` r
max(M2)
```

    ## [1] 1

``` r
min(M2)
```

    ## [1] 0

Compute the sample covariance matrix of the variables in M

``` r
crossprod(Mc) / (nrow(Mc) - 1)
```

    ##              mpg        disp         hp        drat          wt
    ## mpg    36.324103  -633.09721 -320.73206   2.1950635  -5.1166847
    ## disp -633.097208 15360.79983 6721.15867 -47.0640192 107.6842040
    ## hp   -320.732056  6721.15867 4700.86694 -16.4511089  44.1926613
    ## drat    2.195064   -47.06402  -16.45111   0.2858814  -0.3727207
    ## wt     -5.116685   107.68420   44.19266  -0.3727207   0.9573790

``` r
cov(M) # compare the result to the answer
```

    ##              mpg        disp         hp        drat          wt
    ## mpg    36.324103  -633.09721 -320.73206   2.1950635  -5.1166847
    ## disp -633.097208 15360.79983 6721.15867 -47.0640192 107.6842040
    ## hp   -320.732056  6721.15867 4700.86694 -16.4511089  44.1926613
    ## drat    2.195064   -47.06402  -16.45111   0.2858814  -0.3727207
    ## wt     -5.116685   107.68420   44.19266  -0.3727207   0.9573790

Compute the correlation matrix of the variables in M

``` r
cov_M <- crossprod(Mc) / (nrow(Mc) - 1)
M_var <- apply(M, 2, var)
M_sd <- sqrt(M_var)
sd_M <- tcrossprod(M_sd)
cov_M / sd_M
```

    ##             mpg       disp         hp       drat         wt
    ## mpg   1.0000000 -0.8475514 -0.7761684  0.6811719 -0.8676594
    ## disp -0.8475514  1.0000000  0.7909486 -0.7102139  0.8879799
    ## hp   -0.7761684  0.7909486  1.0000000 -0.4487591  0.6587479
    ## drat  0.6811719 -0.7102139 -0.4487591  1.0000000 -0.7124406
    ## wt   -0.8676594  0.8879799  0.6587479 -0.7124406  1.0000000

``` r
cor(M) # compare the result to the answer
```

    ##             mpg       disp         hp       drat         wt
    ## mpg   1.0000000 -0.8475514 -0.7761684  0.6811719 -0.8676594
    ## disp -0.8475514  1.0000000  0.7909486 -0.7102139  0.8879799
    ## hp   -0.7761684  0.7909486  1.0000000 -0.4487591  0.6587479
    ## drat  0.6811719 -0.7102139 -0.4487591  1.0000000 -0.7124406
    ## wt   -0.8676594  0.8879799  0.6587479 -0.7124406  1.0000000

Write a function `dummify()` that takes a factor or a character vector, and which returns a matrix with dummy indicators.

``` r
dummify <- function(x) {
  if (!is.factor(x) & !is.character(x)) {
    stop("input must be factor or character")
  }
  
  levels <- as.factor(attributes(x)$levels)
  
  m <- matrix(numeric(length(levels) * length(x)), ncol = length(levels), nrow = length(x))
  colnames(m) <- levels
  
  for (i in seq_along(x)) {
    for (j in seq_along(levels)) {
      if (x[i] == levels[j]) {
        m[i, j] <-  1
      }
    }
  }
  
  m
}

cyl <- factor(mtcars$cyl)
dummify(cyl)
```

    ##       4 6 8
    ##  [1,] 0 1 0
    ##  [2,] 0 1 0
    ##  [3,] 1 0 0
    ##  [4,] 0 1 0
    ##  [5,] 0 0 1
    ##  [6,] 0 1 0
    ##  [7,] 0 0 1
    ##  [8,] 1 0 0
    ##  [9,] 1 0 0
    ## [10,] 0 1 0
    ## [11,] 0 1 0
    ## [12,] 0 0 1
    ## [13,] 0 0 1
    ## [14,] 0 0 1
    ## [15,] 0 0 1
    ## [16,] 0 0 1
    ## [17,] 0 0 1
    ## [18,] 1 0 0
    ## [19,] 1 0 0
    ## [20,] 1 0 0
    ## [21,] 1 0 0
    ## [22,] 0 0 1
    ## [23,] 0 0 1
    ## [24,] 0 0 1
    ## [25,] 0 0 1
    ## [26,] 1 0 0
    ## [27,] 1 0 0
    ## [28,] 1 0 0
    ## [29,] 0 0 1
    ## [30,] 0 1 0
    ## [31,] 0 0 1
    ## [32,] 1 0 0

Write a function `crosstable()` that takes two factors, and which returns a cross-table between those factors.

``` r
crosstable <- function(x, y) {
  m1 <- dummify(x)
  m2 <- dummify(y)
  crossprod(m1, m2)
}

cyl <- factor(mtcars$cyl)
gear <- factor(mtcars$gear)
xtb <- crosstable(cyl, gear)
xtb
```

    ##    3 4 5
    ## 4  1 8 2
    ## 6  2 4 1
    ## 8 12 0 2
