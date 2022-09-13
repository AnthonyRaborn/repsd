#' null_repsd
#'
#' @param item_count numeric - How many items?
#' @param focal_sample numeric - How large is the focal sample?
#' @param focal_prop numeric, between 0 and 1 (exclusive). What is the proportion
#' of the focal sample compared to the rest of the data?
#' @param impact numeric - What is the expected effect size between focal group
#' and the rest of the population?
#' @param item_params_a numeric vector - What are the discrimination parameters
#' of the items in the data set?
#' @param item_params_b numeric vector - What are the difficulty parameters of
#' the items in the data set?
#' @param iterations numeric - How many iterations for the function to run?
#'
#' @return A data.frame with repsd values for each item
#' @export
#'
#' @importFrom stats rnorm runif
#' @importFrom progress progress_bar


null_repsd <- function(item_count = 20,
                       focal_sample = 88,
                       focal_prop = .09,
                       impact = -.417,
                       item_params_a = c(
                         1.188,
                         .528,
                         1.065,
                         1.294,
                         0.721,
                         1.129,
                         .339,
                         1.685,
                         .861,
                         1.08,
                         1.003,
                         .652,
                         1.637,
                         1.268,
                         1.218,
                         1.32,
                         2.049,
                         .791,
                         1.448,
                         .35
                       ),
                       item_params_b = c(
                         .398,
                         2.22,
                         -.078,
                         -.984,
                         1.341,
                         .006,
                         -.831,
                         -.118,
                         -1.838,
                         -2.227,
                         1.051,
                         .233,
                         .971,
                         -.795,
                         -.143,
                         -.213,
                         1.106,
                         1.007,
                         .6,
                         2.405
                       ),
                       iterations = 10000) {
  null_repsd_est <- c()
  pb <- progress::progress_bar$new(total = iterations)
  cat('Beginning repsd Null Distribution Estimation.\n')

  for (i in 1:iterations) {
    pb$tick()
    totalss <- focal_sample / focal_prop
    thetafoc <- stats::rnorm(focal_sample, impact, 1)
    nonfocss <-
      (focal_sample / focal_prop) - focal_sample #see how formula was obtained in scanned notes in pub folder
    nonfocmean <-
      ((impact + 1) - (impact * focal_prop)) / (1 - focal_prop) #see how formula was obtained in scanned notes in pub folder
    thetanonfoc <- stats::rnorm(nonfocss, nonfocmean, 1)
    theta <- c(thetafoc, thetanonfoc)
    thetaframe <- data.frame("theta" = theta,
                             "focal" = c(rep(1, focal_sample), rep(0, (nonfocss))))

    # using item parameters from mplus 2pl run on the data
    a <- item_params_a
    b <- item_params_b
    itemparam <- data.frame("item" = c(1:item_count),
                            "a" = a,
                            "b" = b)

    twopl <- function(iparam, theta, i) {
      ((exp(iparam[i, 2] * (
        theta - iparam[i, 3]
      ))) /
        (1 + (exp(
          iparam[i, 2] * (theta - iparam[i, 3])
        ))))
    }

    data <- c()

    for (j in 1:item_count) {
      data <- cbind(data,
                    ifelse(twopl(itemparam, theta, j) >= stats::runif(length(theta)), 1, 0))
    }

    data <-
      cbind(data, c(rep(1, focal_sample), rep(0, (
        totalss - focal_sample
      ))))

    ttscore <- rowSums(data[, 1:item_count])

    stratum_group <-
      cut(
        ttscore,
        breaks = seq(min(ttscore), max(ttscore), length.out = 5),
        include.lowest = TRUE,
        labels = FALSE
      )

    data <- cbind(data, stratum_group, ttscore)

    data_temp <- subset(data, data[, (item_count + 1)] == 1)
    maxi <- max(data_temp[, (item_count + 3)])
    data <- subset(data, data[, (item_count + 3)] <= maxi)

    ## getting new focal sample size , after removing strata with less than 10
    data_helping <- subset(data, data[, (item_count + 1)] == 1)
    stratum_group_temp <- data_helping[, item_count + 2]
    ns1 <- table(stratum_group_temp)[1]
    ns2 <- table(stratum_group_temp)[2]
    ns3 <- table(stratum_group_temp)[3]
    ns4 <- table(stratum_group_temp)[4]
    ns <- c(ns1, ns2, ns3, ns4)

    flags <- c()
    for (z in 1:4) {
      flag <- ifelse(ns[z] < 10, ns[z], 0)
      flags <- c(flags, flag)
    }

    total_removed <- sum(flags)
    focal_sample_used <- focal_sample - total_removed


    # calculating resd for each item
    repsd_each_item <- c()

    for (items_again in 1:item_count) {
      brackets <- c()

      for (s in unique(stratum_group)) {
        sub_data <- subset(data, data[, (item_count + 2)] == s)
        sub_data_foc <-
          subset(sub_data, sub_data[, (item_count + 1)] == 1)

        if (length(sub_data_foc[, items_again] < 10)) {
          # ignoring strata with less than 10 focal examinees
          brackets <- brackets
        }

        if (length(sub_data_foc[, items_again] >= 10)) {
          # using only strata with 10 or more focal examinees

          psg <- mean(sub_data_foc[, items_again])
          psc <- mean(sub_data[, items_again])
          nsg <- length(sub_data_foc[, items_again])
          ng <- focal_sample_used
          bracket <- ((psg - psc) ^ 2) * (nsg / ng)
          brackets <- c(brackets, bracket)
        }
      }

      repsd <- sqrt(sum(brackets))
      repsd_each_item <- c(repsd_each_item, repsd)
    }

    null_repsd_est <- rbind(null_repsd_est, repsd_each_item)

  }
  null_repsd_est
}
