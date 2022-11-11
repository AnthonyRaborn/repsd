#' repsd
#'
#' @param focalSample numeric - How large is the focal sample in the data set?
#' @param focalProp numeric, between 0 and 1 (exclusive). What is the proportion
#' of the focal sample compared to the rest of the data?
#' @param responses data.frame, matrix, or
#' other object which includes the item
#' responses and the focal group ID column.
#' @param focalColumn numeric or character - The location or name of the column
#' that holds the focal group data.
#' @param focalGroupID numeric or character - The value that identifies the focal
#' group.
#' @param matching numeric. How many stratum for matching should be use
#'
#' @return Matrix of repsd values for each item.
#' @export
#'
repsd <-
  function(focalSample = 88,
           focalProp = .09,
           responses = timmsData,
           focalColumn = 21,
           focalGroupID = 1,
           matching = 4) {
    ###############################################
    ############ calculating repsd ################
    ###############################################

    # creating strata
    itemresp_foc <-
      subset(responses,
             responses[, focalColumn] == focalGroupID)
    ttscore_foc <-
      rowSums(itemresp_foc[,-focalColumn])  # I am cutting the strata based on total score of focal group only!
    ttscore <-
      rowSums(responses[,-focalColumn])

    breaks_foc <-
      seq(min(ttscore_foc),
          max(ttscore_foc),
          length.out = matching+1)
    stratum_group <-
      cut(
        ttscore,
        breaks = breaks_foc,
        include.lowest = TRUE,
        labels = FALSE
      )

    itemresp <-
      cbind(responses,
            stratum_group,
            ttscore)

    # removing nonfoc persons with total score higher than max focal total score

    maxi <-
      max(ttscore_foc)
    itemresp <-
      subset(itemresp,
             itemresp[, 'ttscore'] <= maxi)
    num_removed_over_foc_max <-
      (focalSample / focalProp) - nrow(itemresp)

    # removing nonfoc persons with total score lower than min focal total score

    mini <-
      min(ttscore_foc)
    itemresp <-
      subset(itemresp,
             itemresp[, 'ttscore'] >= mini)
    num_removed_under_foc_min <-
      (focalSample / focalProp) - nrow(itemresp)

    ### handling cases of low N for focal group within strata

    focal_N_in_strata <-
      c()  # figuring out number of focal in each strata

    for (s in c(1:matching)) {
      sub_data <- subset(itemresp, itemresp[, 'stratum_group'] == s) # DON'T HARDCODE THE STRATA COLUMN!
      sub_data_foc <- subset(sub_data, sub_data[, focalColumn] == focalGroupID)
      focal_N_in_strata <-
        c(focal_N_in_strata, length(sub_data_foc[, 1]))

    }

    flags <-
      c()  # flagging strata with less than 10 focal group members
    for (z in 1:matching) {
      flag <- ifelse(focal_N_in_strata[z] < 10, focal_N_in_strata[z], 0)
      flags <- c(flags, flag)
    }

    total_focal_removed <-
      sum(flags)  # number of focal persons removed
    new_focal_ss <-
      focalSample - total_focal_removed  # sample size to use in repsd

    ### handling cases of low N for non-focal group within strata

    non_focal_N_in_strata <-
      c()  # figuring out number of nonfocal in each strata

    for (s in c(1:matching)) {
      sub_data <- subset(itemresp, itemresp[, 'stratum_group'] == s)
      sub_data_nonfoc <- subset(sub_data, sub_data[, focalColumn] != focalGroupID)
      non_focal_N_in_strata <-
        c(non_focal_N_in_strata, length(sub_data_nonfoc[, 1]))

    }

    flags <-
      c()  # flagging strata with less than 10 nonfocal group members
    for (z in 1:matching) {
      flag <-
        ifelse(non_focal_N_in_strata[z] < 10, non_focal_N_in_strata[z], 0)
      flags <- c(flags, flag)
    }

    total_non_focal_removed <-
      sum(flags)  # number of focal persons removed

    # calculating resd for each item

    repsd_each_item <- c()

    for (column in 1:(ncol(responses) - 1)) {
      brackets <- c()

      for (s in 1:matching) {
        sub_data <- subset(itemresp, itemresp[, 'stratum_group'] == s)
        sub_data_foc <- subset(sub_data, sub_data[, focalColumn] == focalGroupID)
        sub_data_non_foc <- subset(sub_data, sub_data[, focalColumn] != focalGroupID)

        if (length(sub_data_foc[, 1] < 10)) {
          # ignoring strata with less than 10 focal examinees
          brackets <- brackets
        }

        if (length(sub_data_non_foc[, 1] < 10)) {
          # ignoring strata with less than 10 nonfocal examinees
          brackets <- brackets
        }

        if ((length(sub_data_foc[, 1] >= 10)) &
            (length(sub_data_non_foc[, 1] >= 10))) {
          # using only strata with 10+ foc and nonfoc

          psg <- mean(sub_data_foc[, column])
          psc <- mean(sub_data[, column])
          nsg <- length(sub_data_foc[, column])
          ng <-
            new_focal_ss                # ensuring weighting of resd is based on corrected sample size
          bracket <- ((psg - psc) ^ 2) * (nsg / ng)
          brackets <- c(brackets, bracket)
        }
      }

      repsd <- sqrt(sum(brackets))

      repsd_each_item <- c(repsd_each_item, repsd)
    }
    return(
      list('repsd_each_item' = repsd_each_item,
           'total_focal_removed' = total_focal_removed,
           'total_non_focal_removed' = total_non_focal_removed,
           'num_removed_over_foc_max' = total_focal_removed
      )
    )

  }
globalVariables('timmsData')
