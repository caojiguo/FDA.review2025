# argo data are available through the package
# {argoFloats}
# Check the package documents for further information 

packages <- c(
  "oce", "argoFloats",
  "dplyr", "tidyr", "ggplot2", "lubridate"
)
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

library(argoFloats)
library(oce)
library(lubridate)

#> Loading required package: gsw
## 1. Get worldwide float-profile index, saving to ~/data/argo by default.
# ATTENTION: this operation will download many files
getIndex(destdir = "data/argo/", age=0)
# indexAll <- getIndex(filename = "data/argo/ar_index_global_prof.rda",
#   server = NULL, destdir = "data/argo")
# set arguments in subset() to select the spatial/temporal range
currdate = "2024-01-01"
index <- subset(indexAll,
  time=list(
    from=ymd(currdate) |> as.POSIXct(),
    to=(ymd(currdate) + days(1)) |> as.POSIXct())
)
## 3. Get NetCDF files for these profiles, saving to ~/data/argo by default.
profiles  <- getProfiles(index, destdir = "data/argo")
## 4. Read the NetCDF files.
argos <- readProfiles(profiles, destdir = "data/argo")
## 5. Examine QC flags, and set questionable data to NA.
argosClean <- applyQC(argos)

