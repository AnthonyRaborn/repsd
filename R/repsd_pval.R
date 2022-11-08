#' Calculating p-values for repsd
#'
#' @details Calculates the p-values for `repsd` for the data set.
#' It can be used as a wrapper function by providing
#' the `null_repsd` function and the `repsd_each_item`
#' output of the `repsd` function (each with proper
#' arguments) as the arguments to `null_dist` and
#' `repsd_each_item`, respectively.
#'
#' @param alpha The alpha level to calculate significance.
#' @param null_dist A `data.frame`-type object with the null distribution simulation for each item as columns.
#' @param repsd_each_item A vector of the repsd values for each item.
#' @param responses
#'
#' @return If the `colorDF` package is installed and accessible, a `colorDF` with the significant items highlighted. Otherwise, a `data.frame`. Both have columns with the `items` names, the `repsd` value, the `p.value`, and the `sig` (0 = false, 1 = true) for each item.
#' @export
#'
#' @examples
#'
repsd_pval <-
  function(
    alpha = .05,
    null_dist = null_repsd(),
    repsd_each_item = repsd()$repsd_each_item,
    responses = timmsData,
    focalColumn = 21
    ) {

    # p value and sig for each item
    p_for_each_item  <- c()
    sig_for_each_item <- c()

    for (column in 1:ncol(null_dist)) {
      null_for_item <-
        null_dist[, column] |> unlist()
      null_values <-
        subset(null_for_item, null_for_item >= repsd_each_item[column]) |> unlist()
      p <-
        ((length(null_values)) + 1) / ((length(null_for_item)) + 1)
      # p formula based on North et. al. 2002
      sig <- ifelse(p <= alpha, 1, 0)

      p_for_each_item <- c(p_for_each_item, p)
      sig_for_each_item <- c(sig_for_each_item, sig)
    }

    results <-
      data.frame(
        'items' = colnames(responses[, -focalColumn]),
        'repsd' = repsd_each_item,
        'p-value' = p_for_each_item,
        'sig' = sig_for_each_item
      )

    if (require(colorDF)) {
      results |>
        colorDF::colorDF(theme = 'wb') |>
        colorDF::highlight('sig' == 1)
    } else {
      results
    }

  }
