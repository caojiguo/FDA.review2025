library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)


# Import, Clean, Impute, Aggregate ----------------------------------------

raw_dir = file.path("data", "canadian_climate_aq", "raw_files")
dat_dir = file.path("data", "canadian_climate_aq")

# climate = do.call(
#   rbind,
#   lapply(
#     0:46, \(i) {
#       read_csv(
#         file.path(raw_dir, "climate", paste0("climate-daily (",i,").csv")),
#         show_col_types = FALSE
#       )
#     }
#   )
# ) |> 
#   rename(
#     Longitude = x,
#     Latitude = y,
#     Station = STATION_NAME,
#     DateTime = LOCAL_DATE,
#     Province = PROVINCE_CODE,
#     Year = LOCAL_YEAR,
#     Month = LOCAL_MONTH,
#     Day = LOCAL_DAY,
#     MeanTemperature = MEAN_TEMPERATURE,
#     MinRelHumidity = MIN_REL_HUMIDITY,
#     MaxRelHumidity = MAX_REL_HUMIDITY
#   ) |> 
#   mutate(
#     Date = as.Date(DateTime)
#   ) |> 
#   dplyr::select(
#     Longitude,
#     Latitude,
#     Station,
#     Date,
#     Province,
#     Year,
#     Month,
#     Day,
#     MeanTemperature,
#     MinRelHumidity,
#     MaxRelHumidity
#   )

# write_csv(climate, file.path(raw_dir, "climate2023.csv"))

# climate <- read_csv(file.path(raw_dir, "climate2023.csv")) |> 
#   distinct(Station, Date, .keep_all = TRUE)

# read_pollutant <- function(path) {
#   dat <- read_csv(path, na = c("", "NA", "-999"), show_col_types = FALSE)
#   names(dat) <- str_split(names(dat), "//") |> sapply(\(x) x[1])
#   padzeros <- sapply(1-floor(log10(1:24)), \(n) paste0(rep("0", n), collapse = ""))
#   valmat <- dat[,paste0("H", padzeros, 1:24)]
#   na_ratio <- rowMeans(is.na(valmat))
#   meanval <- rowMeans(valmat, na.rm = TRUE)
#   dat |> 
#     rename(Province = `Province/Territory`, NAPSID = `NAPS ID`) |> 
#     mutate(
#       NAPSID = as.integer(NAPSID),
#       MeanPPM = if_else(na_ratio <= 0.4, meanval, NA)
#     ) |> 
#     dplyr::select(
#       Pollutant, NAPSID, City, Province,
#       Longitude, Latitude,
#       Date, MeanPPM
#     )
# }

# pollute <- do.call(
#   rbind,
#   lapply(
#     c("CO", "NO", "NO2", "O3", "PM10", "PM25", "SO2"),
#     \(p) read_pollutant(file.path(raw_dir, paste0(p, "_2023.csv")))
#   )
# ) |> 
#   distinct(Pollutant, NAPSID, Date, .keep_all = TRUE) |> 
#   pivot_wider(
#     names_from = Pollutant,
#     values_from = MeanPPM
#   )


# Match climate data with pollutant data ---------------------------------

# site_climate <- climate |> 
#   distinct(Station, Longitude, Latitude)

# site_pollute <- pollute |> 
#   distinct(NAPSID, .keep_all = TRUE) |> 
#   select(NAPSID, City, Longitude, Latitude)

# library(RANN)

# nn <- nn2(data  = as.matrix(site_climate[,c("Longitude", "Latitude")]),
#           query = as.matrix(site_pollute[,c("Longitude", "Latitude")]),
#           k     = 1,
#           eps   = 0)
# closest.idx  <- nn$nn.idx[,1]    # integer index into rows of Y
# closest.dist <- nn$nn.dists[,1]  # Euclidean distance

# site_pollute <- site_pollute |> 
#   mutate(
#     Station = site_climate$Station[closest.idx],
#     Longitude.Climate = site_climate$Longitude[closest.idx],
#     Latitude.Climate = site_climate$Latitude[closest.idx],
#     SiteDistance = closest.dist
#   )

# match_id <- match(pollute$NAPSID, site_pollute$NAPSID)

# dat <- pollute |> 
#   mutate(
#     Station = site_pollute$Station[match_id]
#   ) |> 
#   inner_join(
#     climate |> rename(
#       StationLongitude = Longitude,
#       StationLatitude = Latitude
#     ) |> 
#       dplyr::select(-Province),
#     by = c("Station", "Date")
#   )


# Save preprocessed data -------------------------------------------------

# write_csv(dat, file.path(raw_dir, "ca_climate_airqlty_2023.csv"))


# Save data in a subject-matrix format -----------------------------------
# dat <- read_csv(file.path(raw_dir, "ca_climate_airqlty_2023.csv"))

# # high rate of missing: CO, PM10, SO2
# # rest of variables:
# # NO, NO2, O3, PM2.5, MeanTemperature, MinRelHumidity, MaxRelHumidity

# unique_sites <- sort(unique(dat$NAPSID)) 
# unique_dates <- sort(unique(dat$Date))
# gridobj <- expand.grid(date=unique_dates, site=unique_sites)

# # pad dates if they are missing
# dat <- tibble(NAPSID=gridobj$site, Date=gridobj$date) |> 
#   left_join(dat, by = c("NAPSID", "Date"))

# # Seperate subjects (NAPS monitoring sites) and remove certain amount of missing values
# var_names <- c(
#   "NO", "NO2", "O3", "PM2.5",
#   "MeanTemperature", "MinRelHumidity", "MaxRelHumidity"
# )
# dat_matrices <- lapply(
#   var_names, \(v) {
#     dat |> select(all_of(c("Date", "NAPSID", v))) |> 
#       pivot_wider(names_from = "NAPSID", values_from = v)
#   }
# )
# names(dat_matrices) <- var_names

# # remove sites and dates with high rate of missing values
# mr_site <- sapply(dat_matrices, \(mat) select(mat, -Date) |> is.na() |> colMeans())
# rm_sites <- apply(mr_site > 0.4, 2, which) |> 
#   unlist() |> unique() |> 
#   (\(x) names(select(dat_matrices[[1]], -Date))[x])()
# remain_sites <- unique_sites[!(unique_sites %in% as.integer(rm_sites))]
# dat_matrices <- lapply(
#   dat_matrices, \(mat) mat |> select(all_of(c("Date", as.character(remain_sites))))
# )

# # NOTE: CAREFUL, don't run when there is nothing to be removed
# # mr_date <- sapply(dat_matrices, \(mat) select(mat, -Date) |> is.na() |> rowMeans())
# # remain_dates <- apply(mr_date > 0.3, 2, which) |> 
# #   unlist() |> unique() |> sort() |> 
# #   (\(x) dat_matrices[[1]]$Date[-x])()
# # dat_matrices <- lapply(
# #   dat_matrices, \(mat) mat |> filter(Date %in% remain_dates)
# # )

# for (v in names(dat_matrices)) {
#   saveRDS(dat_matrices[[v]], file.path(dat_dir, paste0(v, ".rds")))
# }
