## ----setup, include = FALSE---------------------------------------------------
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

## -----------------------------------------------------------------------------
library(partition)

part_icc_rowmeans <- replace_partitioner(
  part_icc,
  reduce = as_reducer(rowMeans)
)

part_icc_rowmeans

## -----------------------------------------------------------------------------
set.seed(1234)

df <- simulate_block_data(
  block_sizes = rep(5, 3),
  lower_corr = .4,
  upper_corr = .6,
  n = 100
)

prt <- partition(df, .5, partitioner = part_icc_rowmeans)
prt
partition_scores(prt)

## -----------------------------------------------------------------------------
inter_item_reliability <- function(.data) {
   corr(.data) %>%
    colMeans(na.rm = TRUE) %>%
    mean()
}

measure_iir <- as_measure(inter_item_reliability)

prt <- partition(df, .5, partitioner = replace_partitioner(part_icc, measure = measure_iir))
prt

## -----------------------------------------------------------------------------
euc_dist <- function(.data) as.matrix(dist(t(.data)))

# find the pair with the minimum distance
min_dist <- function(.x) {
  indices <- arrayInd(which.min(.x), dim(as.matrix(.x)))

  #  get variable names with minimum distance
  c(
    colnames(.x)[indices[1]],
    colnames(.x)[indices[2]]
  )
}

## -----------------------------------------------------------------------------
direct_euc_dist <- as_director(euc_dist, min_dist)

prt <- partition(df, .5, partitioner = replace_partitioner(part_icc, direct = direct_euc_dist))
prt

## ---- eval = FALSE------------------------------------------------------------
#  function(spearman = FALSE) {
#    as_partitioner(
#      direct = direct_dist(spearman = spearman),
#      measure = measure_icc,
#      reduce = reduce_scaled_mean
#    )
#  }

## -----------------------------------------------------------------------------
custom_part <- as_partitioner(
  direct = as_director(euc_dist, min_dist),
  measure = as_measure(inter_item_reliability),
  reduce = as_reducer(rowMeans)
)

partition(df, .5, custom_part)

## ---- eval = FALSE------------------------------------------------------------
#  function(.partition_step) {
#    reduce_cluster(.partition_step, rowMeans)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  function(.partition_step) {
#    map_cluster(.partition_step, rowMeans)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  function(.partition_step, na.rm = FALSE) {
#    partialized_rowMeans <- purrr::partial(rowMeans, na.rm = na.rm)
#    map_cluster(.partition_step, partialized_rowMeans)
#  }

## -----------------------------------------------------------------------------
direct_hcluster <- function(.partition_step) {
  #  set initial k to 1 - number of cols in data
  if (is.null(.partition_step$k)) {
    .partition_step$k <- ncol(.partition_step$reduced_data) - 1
  }
  
  #  stop partition if all k checked
  if (.partition_step$k == 0) {
    #  tell the partition algorithm to stop
    .partition_step$all_done <- TRUE
    return(.partition_step)
  }
  
  if (is.null(.partition_step$hc)) {
    #  save hclust object for future use
    .partition_step$hc <- hclust(dist(t(.partition_step$reduced_data)))
  }
  
  .partition_step$target <- cutree(.partition_step$hc, k = .partition_step$k)
  
  .partition_step
}

## -----------------------------------------------------------------------------
part_hcluster <- as_partitioner(
  direct = direct_hcluster,
  #  use same functions as part_kmeans() but search k linearly 
  measure = purrr::partial(measure_min_icc, search_method = "linear"),
  reduce =   purrr::partial(reduce_kmeans, search = "linear")
)

partition(df, .5, part_hcluster)

