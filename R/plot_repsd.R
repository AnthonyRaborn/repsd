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

plot_repsd(repsd_values, null_values, pvalues, 18, 50)

par(mfrow=c(2,2))

for (i in c(1,8,16,18)) {

  plot_repsd(repsd_values, null_values, pvalues, i, 50)

}

