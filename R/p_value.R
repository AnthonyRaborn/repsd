###############################################
############ calculating p for repsd ##########
###############################################
repsd_pval <-
  function() {
    alpha = .05
    # calling null distribution
    null = null_repsd()
    repsd_each_item = repsd()

    # p value and sig for each item
    p_for_each_item  <- c()
    sig_for_each_item <- c()

    for (column in 1:ncol(null)) {
      null_for_item <- null[, column]
      null_values <-
        subset(null_for_item, null_for_item >= repsd_each_item[column])
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

    results |>
      colorDF::colorDF(theme = 'wb') |>
      colorDF::highlight('sig' == 1)

  }
