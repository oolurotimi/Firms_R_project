packages <- c(
  "tidyverse",
  "tidymodels",
  "haven",
  "foreign",
  "lfe",
  "stringi",
  "magrittr",
  "lemon",
  "grid",
  "gridExtra",
  "cowplot",
  "stargazer",
  "chirps",
  "lubridate",
  "beepr"
)

pacman::p_load(packages, character.only = TRUE, install = TRUE)

# Global setting
## You will have to change this

project_dir <- "/Users/mizuhirosuzuki/Dropbox/Uganda_temperature"

# Load data

##
df <- read_dta(file.path(project_dir, "Data/Survey/Uganda_Combined_02072024_nomisdate.dta"))
df <- df %>% 
  group_by(id_districtName, subcounty_new, parishnew, newdate) %>% 
  mutate(parish_date_group = cur_group_id()) %>% 
  ungroup()

 df_collapse <- df %>% 
   drop_na(lon, lat, newdate) %>% 
   group_by(parish_date_group, newdate) %>% 
   slice(1) %>% 
   select(lon, lat) %>% 
   ungroup()

 
 ###Notes to Renata: I think I have mostly figured out how this part of the code works 
# Temperature on survey dates =================================

unique_date <- df_collapse$newdate %>% unique

df_test_temp_list <- vector(mode = "list", length = length(unique_date))

for (i in seq_along(unique_date)) {

  if (i %% 10 == 0) {
    print(i)
  }

  test_date <- unique_date[i]

  date_c <- c(test_date, test_date)
  df_test_date <- df_collapse %>%
    filter(newdate == test_date)

  lonlat <- df_test_date %>%
    select(lon, lat) %>%
    data.frame()

  df_temp <- get_chirts(
    lonlat, date_c, var = "Tmax"
  )

  df_test_temp_list[[i]] <- df_test_date %>% bind_cols(temp_max = df_temp$chirts)

  Sys.sleep(runif(1, 5, 10))

}

df_test_date_temp <- df_test_temp_list %>% bind_rows()

df_analysis_clean <- df %>%
  drop_na(lon, lat, newdate) %>%
  left_join(df_test_date_temp %>% select(parish_date_group, temp_max), by = "parish_date_group") %>%
  mutate(
    temp_bins = cut(temp_max, breaks = c(0, 28, 30, 32, 34, 40))
  )

df_all <- read_dta(file.path(project_dir, "Data/Survey/Uganda_Combined_02072024_nomisdate.dta")) %>%
  drop_na(lon, lat, newdate) %>%
  mutate(
    english_best = (english == 5),
    math_best = (math == 7)
    ) %>%
  group_by(id_districtName, subcounty_new) %>%
  mutate(district_subcounty = cur_group_id()) %>%
  mutate(female = ifelse(gender == 2, 1, 0)) %>%
  ungroup() %>%
  left_join(
      df_analysis_clean %>%
      drop_na(lon, lat, newdate) %>%
      group_by(id_districtName, subcounty_new, parishnew, newdate) %>%
      select(id_districtName, subcounty_new, parishnew, newdate, temp_max, temp_bins) %>%
      slice(1) %>%
      ungroup(),
    by = c("id_districtName", "subcounty_new", "parishnew", "newdate")
    )

write_delim(df_all, file.path(project_dir, "Data/Cleaned/test_temp_conflict_merged.csv"))
saveRDS(df_all, file.path(project_dir, "Data/Cleaned/test_temp_conflict_merged.Rds"))


###Note to Renata: this is the part of the code I am having trouble with, as I need to download annual temperature and raninfall for each year for each location

# Long-run temperature  =================================

df_collapse <- df %>% 
  drop_na(lon, lat, newdate) %>% 
  group_by(id_districtName, subcounty_new, parishnew) %>% 
  slice(1) %>% 
  select(lon, lat) %>% 
  ungroup()

lonlat <- df_collapse %>% 
  ungroup() %>% 
  select(lon, lat) %>% 
  data.frame()

data_year <- 1985
  
date_seq <- seq(as.Date(str_interp("${data_year}-01-01")), as.Date(str_interp("${data_year}-12-31")), by = "day")
df_test_temp_list <- vector(mode = "list", length = length(date_seq))

for (j in seq_along(date_seq)) {
  
  if (j %% 100 == 0) {
    print(date_seq[j])
  }
  
  date_c <- c(date_seq[j], date_seq[j])
  
  df_temp <- get_chirts(
    lonlat, date_c, var = "Tmax"
  )
  
  df_test_temp_list[[j]] <- df_temp
  
  Sys.sleep(runif(1, 5, 10))
}

saveRDS(
  left_join(
    df_collapse %>% 
      ungroup() %>% 
      mutate(id = row_number()) %>% 
      select(-c(lon, lat)),
    df_test_temp_list %>% 
      bind_rows,
    by = "id"
    ) %>% 
    select(-id),
  file.path(
    project_dir,
    str_interp("Data/Temperature/temperature_${data_year}.Rds")
  )
)

beep(sound = 5)

goopy


a <- df_temp %>% 
  as_tibble() %>% 
  select(- c(lon, lat)) %>% 
  pivot_wider(id_cols = id, names_from = date, values_from = chirts)

df_temp %>% filter(id == 9)

df_test_date_temp <- df_test_temp_list %>% bind_rows()

df_analysis_clean <- df %>% 
  drop_na(lon, lat, newdate) %>% 
  left_join(df_test_date_temp %>% select(parish_date_group, temp_max), by = "parish_date_group") %>% 
  mutate(
    temp_bins = cut(temp_max, breaks = c(0, 28, 30, 32, 34, 40))
  )

df_all <- read_dta(file.path(project_dir, "Data/Survey/Uganda_Combined_02072024_nomisdate.dta")) %>% 
  drop_na(lon, lat, newdate) %>% 
  mutate(
    english_best = (english == 5),
    math_best = (math == 7)
    ) %>% 
  group_by(id_districtName, subcounty_new) %>% 
  mutate(district_subcounty = cur_group_id()) %>% 
  mutate(female = ifelse(gender == 2, 1, 0)) %>% 
  ungroup() %>%
  left_join(
    df_analysis_clean %>% 
      drop_na(lon, lat, newdate) %>% 
      group_by(id_districtName, subcounty_new, parishnew, newdate) %>% 
      select(id_districtName, subcounty_new, parishnew, newdate, temp_max, temp_bins) %>% 
      slice(1) %>% 
      ungroup(),
    by = c("id_districtName", "subcounty_new", "parishnew", "newdate")
    )

write_delim(df_all, file.path(project_dir, "Data/Cleaned/test_temp_conflict_merged.csv"))
saveRDS(df_all, file.path(project_dir, "Data/Cleaned/test_temp_conflict_merged.Rds"))
