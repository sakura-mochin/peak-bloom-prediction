
library(tidyverse)

# Combine datasets

cherry <- read.csv("/Users/willardmcdougal/Desktop/peak-bloom-prediction-main/data/washingtondc.csv") %>% 
  bind_rows(read.csv("/Users/willardmcdougal/Desktop/peak-bloom-prediction-main/data/liestal.csv")) %>% 
  bind_rows(read.csv("/Users/willardmcdougal/Desktop/peak-bloom-prediction-main/data/kyoto.csv"))



library(rnoaa)
stations <- ghcnd_stations()

#' Get the annual average maximum temperature at the given station,
#' separated into the 4 meteorological seasons (Winter, Spring, Summer, Fall).
#' 
#' The seasons are span 3 months each.
#' Winter is from December to February, Spring from March to May,
#' Summer from June to August, and Fall from September to November.
#' Note that December is counted towards the Winter of the next year, i.e.,
#' temperatures in December 2020 are accounted for in Winter 2021.
#' 
#' @param stationid the `rnoaa` station id (see [ghcnd_stations()])
#' @return a data frame with columns
#'   - `year` ... the year of the observations
#'   - `season` ... the season (Winter, Spring, Summer, Fall)
#'   - `tmax_avg` ... average maximum temperate in tenth degree Celsius
#'   
get_tmax <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmax_avg = mean(tmax, na.rm = TRUE))
}

get_tmin <- function (stationid) {
  ghcnd_search(stationid = stationid, var = c("tmin"), 
               date_min = "1950-01-01", date_max = "2022-01-31")[[1]] %>%
    mutate(year = as.integer(format(date, "%Y")),
           month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
           season = cut(month, breaks = c(0, 2, 5, 8, 11),
                        include.lowest = TRUE,
                        labels = c("Winter", "Spring", "Summer", "Fall")),
           year = if_else(month == 0, year + 1L, year)) %>%
    group_by(year, season) %>%
    summarize(tmin_avg = mean(tmin, na.rm = TRUE))
}


historic_tmax <-
  tibble(location = "washingtondc", get_tmax("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_tmax("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_tmax("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_tmax("CA001108395")))

historic_tmin <-
  tibble(location = "washingtondc", get_tmin("USC00186350")) %>%
  bind_rows(tibble(location = "liestal", get_tmin("GME00127786"))) %>%
  bind_rows(tibble(location = "kyoto", get_tmin("JA000047759"))) %>%
  bind_rows(tibble(location = "vancouver", get_tmin("CA001108395")))


### Visualizations of historic temps

historic_tmax %>%
  ggplot() + 
  aes(year, tmax_avg) + 
  geom_line() +
  xlim(1950, 2031) +
  labs(x = "Year", y = "Average maximum temperature (1/10 °C)") +
  facet_grid(factor(season) ~ str_to_title(location))


historic_tmin %>%
  ggplot() + 
  aes(year, tmin_avg) + 
  geom_line() +
  xlim(1950, 2031) +
  labs(x = "Year", y = "Average minimum temperature (1/10 °C)") +
  facet_grid(factor(season) ~ str_to_title(location))


### 

# Step 1a extrapolate average seasonal maximum temperature
ls_fit_tmax <- lm(tmax_avg ~ year * season + location, 
                         data = historic_tmax)

tmax_predictions <-
  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
              season = c("Winter", "Spring", "Summer", "Fall"),
              year = 1950:2033) %>%
  bind_cols(predicted_temperature = 
              predict(ls_fit_tmax, newdata = .)) %>%
  filter(season %in% c("Winter", "Spring")) %>%
  pivot_wider(names_from = season, values_from = predicted_temperature)


# Step 1b extrapolate average seasonal minimum temperature
ls_fit_tmin <- lm(tmin_avg ~ year * season + location, 
                         data = historic_tmin)

tmin_predictions <-
  expand_grid(location = c("washingtondc", "liestal", "kyoto", "vancouver" ),
              season = c("Winter", "Spring", "Summer", "Fall"),
              year = 1950:2033) %>%
  bind_cols(predicted_temperature = 
              predict(ls_fit_tmin, newdata = .)) %>%
  filter(season %in% c("Winter", "Spring")) %>%
  pivot_wider(names_from = season, values_from = predicted_temperature)



####### Combine tmax and tmin predictions

colnames_new <- c("location", "year", "Winter_max", "Spring_max"  )
colnames(tmax_predictions) <- colnames_new

colnames_new <- c("location", "year", "Winter_min", "Spring_min"  )
colnames(tmin_predictions) <- colnames_new

temp_predictions <- cbind(tmax_predictions, tmin_predictions$Winter_min, tmin_predictions$Spring_min)
colnames_new <- c("location", "year", "Winter_max", "Spring_max", "Winter_min", "Spring_min"  )
colnames(temp_predictions) <- colnames_new



# Step 2. predict bloom day from extrapolated temperatures


predictions_temperature <-
  temp_predictions %>%
  left_join(cherry,
            by = c("location", "year")) %>%
  lm(bloom_doy ~ Spring_max * Winter_max * Spring_min * Winter_min, data = .) %>%
  predict(newdata = temp_predictions) %>%
  round() %>%
  bind_cols(predicted_doy_temperature = ., temp_predictions)


#' Small helper function to convert the day of year to
#' the actual date.
#' 
#' @param year year as an integer
#' @param doy day of the year as integer (1 means January 1st)
#' @return date string
doy_to_date <- function (year, doy) {
  strptime(paste(year, doy, sep = '-'), '%Y-%j') %>% # create date object
    strftime('%Y-%m-%d') # translate back to date string in ISO 8601 fopredictions_temperature <- predictions_temperature %>% 
  filter(year > 2022) %>% 
  mutate(predicted_date = doy_to_date(year, predicted_doy_temperature))
  }


submission_predictions <- predictions_temperature %>% 
  filter(year > 2022) %>% 
  pivot_wider(names_from = 'location', values_from = 'predicted_doy_temperature', id_cols = 'year' ) %>% 
  select(year, kyoto, liestal, washingtondc, vancouver)
  
 

write.csv(submission_predictions, file = "/Users/willardmcdougal/Desktop/peak-bloom-prediction-main/cherry-predictions.csv",
          row.names = FALSE)







