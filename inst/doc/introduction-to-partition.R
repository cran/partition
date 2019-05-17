## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, 
  fig.height = 5, 
  fig.align = "center",
  fig.dpi = 320,
  warning = FALSE,
  message = FALSE
)

## ------------------------------------------------------------------------
library(partition)
library(ggplot2)
set.seed(1234)
# create a 100 x 15 data set with 3 blocks
df <- simulate_block_data(
  # create 3 correlated blocks of 5 variables each
  block_sizes = rep(5, 3),
  lower_corr = .4,
  upper_corr = .6,
  n = 100
)

df

## ------------------------------------------------------------------------
prt <- partition(df, threshold = .5)

prt

## ------------------------------------------------------------------------
partition_scores(prt)

## ------------------------------------------------------------------------
plot_ncluster(prt) +
  # plot_*() functions return ggplots, so they can be extended using ggplot2
  theme_minimal(14)

## ------------------------------------------------------------------------
plot_information(prt, geom = geom_histogram) +
  theme_minimal(14)

## ------------------------------------------------------------------------
mapping_key(prt)

## ------------------------------------------------------------------------
unnest_mappings(prt)

## ------------------------------------------------------------------------
part_icc()

## ------------------------------------------------------------------------
prt_kmeans <- partition(df, threshold = .5, partitioner = part_kmeans())
prt_kmeans

## ------------------------------------------------------------------------
# create a data.frame of 10 independent variables
ind_df <- purrr::map_dfc(1:10, ~rnorm(30))
ind_part <- partition(ind_df, .5)
ind_part

identical(ind_df, partition_scores(ind_part))

## ------------------------------------------------------------------------
plot_stacked_area_clusters(df) +
  theme_minimal(14)

## ------------------------------------------------------------------------
perms <- test_permutation(df, nperm = 10)
perms

## ---- fig.height = 7-----------------------------------------------------
plot_permutation(perms, .plot = "nreduced") +
  theme_minimal(14)

