
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flexoptr

The goal of flexoptr is to provide a suite of functions to generalise
and ease the modelling of energy flexibilities. By defining base
parameters, the needs of a constrained flexibility are calculated, and
optimised over price data. Several functions to facilitate the
optimisation of more complex market and configuration analyses are also
provided.

The only required external package to run this package is magrittr which
introduces the pipe operator and is only used for making code more
readable. The r-Project has already announced plans to make a pipe
operator a native part of base R, development is currently under way.
Therefore, a future adaptation of the code which completely avoids
secondary packages is possible.

## Installation

You can install flexoptr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("henobe/flexoptr")
```

There are currently no plans to release the package on CRAN.

## Example

Given a flexibility of with the physical parameters of a delivery
obligation (constant), a maximum charging power (variable), and a
storage capacity an optimal schedule for minimising energy costs can be
calculated.

``` r
library(flexoptr)

base_parameters <- c(
  "starting_state" = 5,
  "capacity" = 10,
  "charge_rate" = 4,
  "loss_rate" = 1
)

sample_constraints <- build_constraints(
  cycles = 10,
  state = 5,
  parameters = base_parameters
)

sample_prices <- c(37, 17, 4, 4, 9, 21, 22, 47, 48, 5)

optimise_constraints(sample_constraints, sample_prices, 15)
#>  [1] 0 0 4 4 2 1 0 0 0 4
```

This approach is extended in the library and many functions are provided
that facilitate the analysis of many scenarios.

``` r
sample_prices_day <- sample.int(50, 24, replace = TRUE)

optimise_schedule(
  schedule = rep(0, 24),
  parameters = base_parameters,
  prices = format_da_prices(sample_prices_day),
  shift = 24 * base_parameters["loss_rate"]
)
#> $schedule
#>  [1] 0 0 4 0 4 0 4 0 0 0 0 0 4 0 4 0 0 0 0 4 0 0 0 0
#> 
#> $state
#>  [1]  4  3  6  5  8  7 10  9  8  7  6  5  8  7 10  9  8  7  6  9  8  7  6  5
#> 
#> $trades
#>   time volume prices trading_time
#> 1    3      4      4            0
#> 2    5      4      9            0
#> 3    7      4      5            0
#> 4   13      4      5            0
#> 5   15      4      6            0
#> 6   20      4     17            0
```

## Developing with flexoptr

This library is developed and tested under R version 4.0.4 (2021-02-15).
The library provides sophisticated functions for preparing data and
optimising various pre-configured scenarios which are all natively
documented. It is also possible to use the more basic functions and
develop own scenarios.

### Preparing Inputs for Simulations

The basis for most complex optimisations should be
`optimise_schedule()`. Its inputs are however not self-explanatory
because the function can be used very flexibly just by formatting them
differently.

The logic behind the function assumes that a set of times should be
optimised. For each time, there is one price present. By analysing the
format of the price data, the function iterates over times where prices
are present. As an example, the function `format_da_prices()` takes
price data and formats them in a 24-step list which means that all
24-elements are traded and optimised simultaneously.

As a basic use case, one would want to compare the optimisation results
of the same configuration of parameters but on different trading
strategies. The function `simulate_marketing()` is a wrapper of
`optimise_schedule()` that iterates through a day-ahead and intra-day
marketing scenario.

At last, it is important to consider that the optimisation will only
handle whole numbers as parameter inputs. By transforming the parameters
but keeping the relation between the parameters equal, nearly any
configuration can still be simulated.

### Underlying Optimisation Algorithm

The whole simulation can be understood as a wrapper and input
preparation for two basic functions, that comprise the optimisation
logic of a constrained storage.

**Description of physical constraints**: The state and future needs of a
storage can be described over three variables with a specific value for
each time interval:

  - Describing how much energy the storage will need to have charged to
    not be empty at the end of that time interval. In code, this value
    is described as `cummin`.
  - How much energy can be possibly charged so that the storage would be
    full as fast as possible. This is referred to as `cummax` in code.
  - Apart from the storage also the charging power is constrained, as it
    can only take values between zero and the maximum charging power. In
    contrast to the previous to variables it is described as `dirmax`.

The initially described `sample_constraints` are in fact a data.frame
where one column describes each variable:

``` r
sample_constraints
#>    cummin cummax dirmax
#> 1       0      7      4
#> 2       0      7      4
#> 3       0      8      4
#> 4       0      9      4
#> 5       0     10      4
#> 6       1     11      4
#> 7       2     12      4
#> 8       3     13      4
#> 9       4     14      4
#> 10      5     15      4
```

**Optimisation inside constraints**: Beginning from a starting state and
having calculated these three variables for the described points in
time, a charge can be planned. The code first uses the constraints to
make a selection of times where a change in schedule must and could
happen:

  - The first time (from a chronological point of view) that the
    `cummin` value is greater than one. Then, only that or preceding
    time intervals are a charge priority.
  - Similarly, at value zero `cummax` describes that the charge cannot
    be increased at that point in time, or else the storage would
    charged beyond capacity at that or a later time.
  - Finally, the remaining charging power `dirmax` must be greater than
    zero.

The optimisation itself is now a simply process of selecting the time
with the lowest price out of the available prices and then adapting the
constraints to reflect the change in schedule.

In the example of the `sample_constraints`, a selection would be made so
that in the times 1-6 the minimal price would be searched. In the
example at the outset the time 3 would then be chosen. As a consequence,
the constraints are automatically adapted:

``` r
flexoptr:::adapt_constraints(sample_constraints, 3)
#>    cummin cummax dirmax
#> 1       0      7      4
#> 2       0      7      4
#> 3       0      7      3
#> 4       0      8      4
#> 5       0      9      4
#> 6       0     10      4
#> 7       1     11      4
#> 8       2     12      4
#> 9       3     13      4
#> 10      4     14      4
```

As a charge in time 3 happened, the values for `cummin`, `cummax`, and
`dirmax` were all adjusted to reflect the new schedule. This process is
repeated as often as units of charge are to be optimised. This approach
(found in `optimise_constraints()`) thus takes into account the physical
constraints when optimising for minimal prices.

Out of these building blocks, complex simulations are constructed by
building constraints, optimising inside these constraints and then
repeating these steps for consecutive time intervals (which is exactly
what the function `optimise_schedule()` does).
