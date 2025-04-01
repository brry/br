#' @title check packages for cran release
#' @description convenience function to run cran package checks on rhub and win-builder
#' @return nothing
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, included here Aug 2023
#' @keywords package
#' @importFrom rhub check_for_cran
#' @importFrom devtools check_win_devel
#' @export
#'
crancheck <- function() 
 {
 message("checking for rhub...")
 if(!file.exists(".github/workflows/rhub.yaml")) stop(".github/workflows/rhub.yaml not found. Run    rhub::rhub_setup()")
 rhub::rhub_doctor()
 message("Submitting to rhub...")
 rhub::rhub_check()
 message("Submitting to win-builder...")
 devtools::check_win_devel(quiet=TRUE)
 message("Done. Results will come in by email.")
 }
