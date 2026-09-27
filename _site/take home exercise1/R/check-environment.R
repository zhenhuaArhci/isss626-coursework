# Setup diagnostics only. No packages are installed and no analysis is run.
local({
  required <- c("knitr", "rmarkdown", "jsonlite", "digest")
  candidates <- c("sf", "dplyr", "readr", "tmap", "spNetwork")
  packages <- c(required, candidates)
  versions <- vapply(packages, function(package) {
    if (requireNamespace(package, quietly = TRUE)) as.character(utils::packageVersion(package))
    else "not installed"
  }, character(1))
  print(data.frame(package = packages,
                   role = c(rep("workspace", length(required)),
                            rep("method-dependent", length(candidates))),
                   version = unname(versions)), row.names = FALSE)
  cat("\n", R.version.string, "\n", sep = "")
  quarto_path <- Sys.which("quarto")
  cat("Quarto: ", if (nzchar(quarto_path)) quarto_path else "not found", "\n", sep = "")
  if (any(versions[required] == "not installed") || !nzchar(quarto_path)) {
    stop("The workspace needs the missing required packages and Quarto before rendering.")
  }
  invisible(versions)
})
