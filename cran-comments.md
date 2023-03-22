# REPSD v1.0.0

This is a new release to CRAN. This packages provides a new nonparametric DIF 
measure for IRT item analysis. Helper functions provide additional utility,
including a plot function to demonstrate the shape of the user-generate null 
distribution of REPSD values and the location of the observed REPSD value in that
distribution. 

The package is a companion to an article that we are submitting
to a journal for review; when the article is published, we will update the 
package description to include a link to the article describing the method
in detail.

This submission was modified to address comments from the initial submission:

- 'DIF' is now written out in the description to define the term in its first use
- `\dontrun{}` was removed as the example was modified and tested to run in <5 sec
- The example also resets the `par()` options to the default user value as suggested
- Replaced a `cat()` call with `message()` to allow for suppression and added 
a `verbose` argument to turn on/off the progress bar in `repsd::null_repsd()`

It has been tested on:

- A local Windows 11 install
- On R-hub
- With `devtools::check_win_devel`

The only NOTES are for spelling issues which, upon inspection, are not actual issues.
