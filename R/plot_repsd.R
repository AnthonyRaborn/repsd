#' REPSD Null vs Observed Histogram
#'
#' @param repsd_values A numerical vector of repsd values, the output of `repsd()$repsd_each_item`.
#' @param null_values A matrix of the repsd null distribution, the output of `null_repsd()`.
#' @param pvalues A numerical vector of the repds p-values, the output of `repsd_pval()$p.value`
#' @param which_item A numerical indicator of the specific item to plot.
#' @param bins A numerical indicator on the number of bins to output in the histogram.
#'
#' @return A `plot` of the REPSD null distribution for the indicated item with
#' the observed REPSD value as a red line and the observed p-value
#' @export
#'
#' @examples
#' # Not run
#' \dontrun{
#' example_repsd <-
#'     repsd()
#' example_null <-
#'     null_repsd()
#' example_pvals <-
#'     repsd_pval(
#'                alpha = .05,
#'                null_dist = example_null,
#'                items_repsd = example_repsd$repsd_each_item
#'                )
#' # Only one plot
#' plot_repsd(repsd_values = example_repsd$repsd_each_item,
#'            null_values = example_null,
#'            pvalues = example_pvals$p.value,
#'            which_item = 18,
#'            bins = 50)
#' # Multiple plots on the same plot
#' par(mfrow=c(2,2))
#' for (i in c(1,8,16,18)) {
#'   plot_repsd(
#'              repsd_values = example_repsd$repsd_each_item,
#'              null_values = example_null,
#'              pvalues = example_pvals$p.value,
#'              which_item = 18,
#'              bins = 50
#'              )
#'   }
#' }

plot_repsd <-
  function(repsd_values, null_values, pvalues, which_item, bins = 30) {
    hist(null_values[,which_item], breaks = bins,
         main = 'Distribution of Simulated Null REPSD Values',
         xlab = 'Null REPSD Values',
         sub = 'Red line indicates observed REPSD value.')
    abline(v = repsd_values[which_item], col = 'red')
    text_to_write = paste0("p-val:\n", pvalues[which_item] |> round(3))
    mtext(text_to_write, side=3, adj = 1)
    mtext(paste0("For Item Number ", which_item))
  }


