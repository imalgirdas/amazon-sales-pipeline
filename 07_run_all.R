# -----------------------------------------------------------------------------
# Script:   07_run_all.R
# Purpose:  Run the whole project in order (load, clean, transform, explore,
#           visualise) and then render the HTML dashboard. Each script runs
#           in its own environment and the run stops at the first error.
#
# Input:    Data/*.csv
# Output:   Output/*, 06_dashboard.html, Output/07_run_log.csv
# -----------------------------------------------------------------------------

# Settings --------------------------------------------------------------------
# Scripts can sit in R/ or next to this file
script_dir <- if (dir.exists("R") && file.exists("R/01_load.R")) "R" else "."
open_dashboard <- interactive()   # opens the browser only in an interactive session (RStudio)

# Order matters, each step reads the output of the one before
steps <- c(
  "01_load.R",
  "02_clean.R",
  "03_transform.R",
  "04_explore.R",
  "05_visualize.R",
  "06_dashboard.Rmd"
)

# Stop early if the data folder or a script is missing
if (!dir.exists("Data")) {
  stop("Folder 'Data' not found. Open the Amazon RStudio project or setwd() to the Amazon folder.")
}
missing_files <- steps[!file.exists(file.path(script_dir, steps))]
if (length(missing_files) > 0) stop("Missing script(s): ", paste(missing_files, collapse = ", "))
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown")

# Run -------------------------------------------------------------------------
# The Rmd goes through rmarkdown::render, the .R files through source().
# Errors are caught so the timing log is still written.
run_log <- data.frame(step = character(), status = character(), seconds = numeric())
start_all <- Sys.time()

for (i in seq_along(steps)) {
  step <- steps[i]
  path <- file.path(script_dir, step)
  message(sprintf("\n[%d/%d] %s  (started %s)", i, length(steps), step, format(Sys.time(), "%H:%M:%S")))
  t0 <- Sys.time()

  result <- tryCatch({
    if (grepl("\\.Rmd$", step)) {
      out <- rmarkdown::render(path, output_dir = ".", envir = new.env(), quiet = TRUE,
                               knit_root_dir = getwd())
      message("      Dashboard written to ", normalizePath(out))
    } else {
      source(path, local = new.env(), echo = FALSE)
    }
    "ok"
  }, error = function(e) {
    message("      FAILED: ", conditionMessage(e))
    "failed"
  })

  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  run_log <- rbind(run_log, data.frame(step = step, status = result, seconds = secs))
  message(sprintf("      %s in %.1f s", if (result == "ok") "Done" else "Stopped", secs))

  if (result == "failed") break
}

# Summary ---------------------------------------------------------------------
# Timings also saved to Output/07_run_log.csv to compare runs
total <- round(as.numeric(difftime(Sys.time(), start_all, units = "secs")), 1)
message("\nPipeline summary")
print(run_log, row.names = FALSE)
message(sprintf("Total time: %.1f s", total))

dir.create("Output", showWarnings = FALSE)
write.csv(transform(run_log, run_at = format(start_all, "%Y-%m-%d %H:%M:%S")),
          file.path("Output", "07_run_log.csv"), row.names = FALSE)

if (all(run_log$status == "ok") && nrow(run_log) == length(steps)) {
  message("All steps completed. Dashboard: 06_dashboard.html")
  if (open_dashboard) utils::browseURL("06_dashboard.html")
} else {
  stop("Pipeline stopped at ", run_log$step[run_log$status == "failed"], ". See the message above.")
}
