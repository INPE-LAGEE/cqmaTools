# compute the yearly influence area

#---- TODO ----


# remove
library(devtools)
devtools::load_all()

#---- Load libraries ----
library(dplyr)
library(raster)

#---- Configuration ----

trajectory_dir <- "/home/lagee/Documents/alber/test/hysplitsimulations/all_co2"
station_data_dir <- "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_3500/by_year/stn_byia"
HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min", 
                      "V8", "V9", "lat", "lon", "height", "pressure") 
grid_resolution <- 1 # DO NOT CHANGE!
grid_lon_range <- c(-80, -30)
grid_lat_range <- c(-40, 10) 

Plot_stations <- FALSE 

site_location_tb <- tribble( 
  ~site, ~longitude, ~latitude, 
  "alf", -55.78, -8.822, 
  "rba", -64.74, -9.02, 
  "san", -54.95, -2.85,
  "tab", -69.9, -5.74,
  "tef", -66.5, -3.68
) 


#---- compute the statistics ----

# Create a table of files.
trajectory_tb <- trajectory_dir %>% 
  list.files(full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = value) %>% 
  dplyr::mutate(filename = basename(file_path)) %>% 
  tidyr::separate(filename, c("site", "traj_year", "traj_month", "traj_day", "traj_hour", "traj_height"), 
                  sep = '_') %>% 
  dplyr::mutate(data = purrr::map(file_path,  readr::read_delim, delim = ' ', 
                                  skip = 7,  
                                  trim_ws = TRUE,
                                  col_names = HYSPLIT.COLNAMES, 
                                  col_types = readr::cols( V1 = readr::col_character(),
                                                           V2 = readr::col_character(),
                                                           year = readr::col_integer(),
                                                           month = readr::col_integer(),
                                                           day = readr::col_integer(),
                                                           hour = readr::col_integer(),
                                                           min = readr::col_integer(),
                                                           V8 = readr::col_character(),
                                                           V9 = readr::col_character(),
                                                           lat = readr::col_double(),
                                                           lon = readr::col_double(),
                                                           height = readr::col_double(),
                                                           pressure = readr::col_double()))) %>%
  tidyr::unnest(cols = c(data)) %>% 
  dplyr::filter(lon >= grid_lon_range[1], lon <= grid_lon_range[2],
                lat >= grid_lat_range[1], lat <= grid_lat_range[2]) %>% 
  dplyr::mutate(sp_index = stringr::str_c(floor(lon), '_', floor(lat))) %>% 
  dplyr::filter(height <= 3500)
    
traj_stats <- trajectory_tb %>% 
  dplyr::group_by(site, traj_year, sp_index) %>% 
  dplyr::summarize(n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename("total_year" = `n()`)

traj_stat_total <- trajectory_tb %>% 
  dplyr::group_by(site, sp_index) %>% 
  dplyr::summarize(n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename("total_site" = `n()`)

site_years <- traj_stats %>% 
  dplyr::select(site, traj_year) %>% 
  dplyr::distinct() %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::rename("years_per_site" = `n()`) 

traj_stats <- traj_stats %>% 
  dplyr::left_join(traj_stat_total, by = c("site", "sp_index")) %>% 
  dplyr::left_join(site_years, by = "site") %>% 
  dplyr::mutate("mean_trajectory" = total_site / years_per_site)

#---- build a raster grid ----

ext <- raster::extent(min(grid_lon_range), max(grid_lon_range), 
                      min(grid_lat_range), max(grid_lat_range))
grid_raster <- raster(ext)
res(grid_raster) <- c(grid_resolution, grid_resolution)
projection(grid_raster) <- CRS("+init=epsg:4326")
grid_raster[] <- 0
#grid_raster[] <- 1:raster::ncell(grid_raster)
#plot(grid_raster)

grid_table <- expand.grid(seq(min(grid_lon_range), by = grid_resolution, length.out = diff(grid_lon_range)), 
                          seq(min(grid_lat_range), by = grid_resolution, length.out = diff(grid_lat_range))) %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(lon = Var1, lat = Var2) %>% 
  dplyr::mutate(sp_index = stringr::str_c(floor(lon), '_', floor(lat))) %>% 
  dplyr::arrange(dplyr::desc(lat), lon)

#---- link the data to the raster ----

my_site <- "san"

data_vector <- traj_stats %>% 
  dplyr::filter(site == my_site,
                traj_year == dplyr::pull(dplyr::slice(., 1), traj_year)) %>% 
  ensurer::ensure_that(length(unique(.$site)) == 1, 
                       length(unique(.$traj_year)) == 1) %>% 
  right_join(grid_table, by = "sp_index") %>% 
  dplyr::pull(mean_trajectory)

grid_raster[] <- data_vector

plot(grid_raster)
points(dplyr::select(site_location_tb, longitude, latitude))
maps::map("world",
          xlim = grid_lon_range,
          ylim = grid_lat_range,
          add = TRUE)

traj_stats %>% 
  write.table(file = "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_3500/by_year/site_traj_mean.csv")

  
