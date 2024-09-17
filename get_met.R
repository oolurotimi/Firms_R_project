#' get_annual_met
#' 
#' Downloads CHIRTS/CHIRPS data and summarizes to an annual max/min/total
#' for Tmax, Tmin, and Precip
#' 
#' @param year 4-digit year to get data from
#' @param locs data frame with columns lon, lat IN THAT ORDER, ANY NAME
#' @param var "Tmax", "Tmin", or "Precip"
#'
#' @return data frame of year, lat, lon, and Tmax/Tmin/TotalPrecip and bins
#' @export
#'

get_annual_met <- function(year, locs, var = "Tmax") {
  
  # Create string of all dates from Jan 1 to Dec 31 of `year` 
  date_seqs <- seq(as.Date(str_interp(paste0(year, "-01-01"))),
                   as.Date(str_interp(paste0(year, "-12-31"))),
                   by = "day")
  
  # For precip data...
  if(var == "Precip") {
    
    # Download chirps precip data for all days in that year for all locations in locs
    annual_met <-
      get_chirps(locs,
                 dates = paste0(year, c("-01-01", "-01-31")),
                 server = "CHC") 
    
    # Summarize to annual total precip for each location
    annual_met <- annual_met |>
      mutate(year = substr(date, 1, 4)) |>
      group_by(lon, lat, year) |>
      summarize(TotalPrecip = sum(chirps)) |>
      ungroup()
    
    return(annual_met)
  }
  
  # For Tmax and Tmin...
  # Download daily chirts data for all days in year for all locs in locs
  annual_met <-
    furrr::future_map(date_seqs,
                      .f = (
                        \(x, locs_to_pull)
                        get_chirts(locs_to_pull, c(x, x),
                                   var = var) |>
                          mutate(year = substr(date, 1, 4))
                      ),
                      locs_to_pull = locs) |>
    bind_rows()
  
  # Summarize to annual max for tmax for each location
  # and add bins
  if (var == "Tmax") {
    annual_met <- annual_met |>
      group_by(lon, lat, year) |>
      summarize(temp_max = max(chirts)) |>
      ungroup() |>
      mutate(tmax_bins = cut(temp_max, breaks = c(0, 28, 30, 32, 34, 40)))
    
  }
  
  # Summarize to annual min for tmin for each location
  # and add bins
  if (var == "Tmin") {
    annual_met <- annual_met |>
      group_by(lon, lat, year) |>
      summarize(temp_min = min(chirts)) |>
      ungroup() |>
      mutate(tmin_bins = cut(temp_min, breaks = c(0, 28, 30, 32, 34, 40)))
    
  }
  
  # Return results
  return(annual_met)
  
}

