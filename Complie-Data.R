library(tidyverse)
library(janitor)
library(readxl)

for (year in 2010:2020){
  read_excel(paste("data/original_atf_data/sourcerecovery_by_state_cy", year, ".xlsx", sep = ""), skip = 1) |>
  select(-1) |>
  rename("Source_State" = "...2") |>
  head(55) |>
  t() |>
  as.data.frame() |>
  row_to_names(row = 1) |>
  rownames_to_column("Recovery_State") |>
  pivot_longer(names_to = "Source_State", values_to = "Guns_Recovered", cols = -Recovery_State) |>
  mutate(Year = year) |>
  write_rds(paste("data/wrangled_by_state/state-recovery", year, ".rds", sep = "")) 
}

condensed <- bind_rows(read_rds("data/wrangled_by_state/state-recovery2010.rds"), read_rds("data/wrangled_by_state/state-recovery2011.rds"), read_rds("data/wrangled_by_state/state-recovery2012.rds"), read_rds("data/wrangled_by_state/state-recovery2013.rds"), read_rds("data/wrangled_by_state/state-recovery2014.rds"), read_rds("data/wrangled_by_state/state-recovery2015.rds"), read_rds("data/wrangled_by_state/state-recovery2016.rds"), read_rds("data/wrangled_by_state/state-recovery2017.rds"), read_rds("data/wrangled_by_state/state-recovery2018.rds"), read_rds("data/wrangled_by_state/state-recovery2019.rds"), read_rds("data/wrangled_by_state/state-recovery2020.rds"))

write_rds(condensed, "data/consolidated/combined2010to2020.rds")
