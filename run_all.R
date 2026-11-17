# Runs the whole pipeline in order, each script in a fresh R session.
#   Rscript run_all.R          all steps
#   Rscript run_all.R 05 06    only the scripts whose number is given

steps <- sort(list.files("R", pattern = "^\\d{2}_.*\\.R$", full.names = TRUE))
steps <- steps[!grepl("^R/00_", steps)]
only <- commandArgs(trailingOnly = TRUE)
if (length(only)) steps <- steps[substr(basename(steps), 1, 2) %in% only]

rscript <- file.path(R.home("bin"), "Rscript")
for (s in steps) {
  message("\n== ", s)
  start <- Sys.time()
  status <- system2(rscript, s)
  if (status != 0) stop(s, " failed with status ", status)
  message(sprintf("   done in %.0f s", as.numeric(difftime(Sys.time(), start, units = "secs"))))
}
