#Load neccesary packages. 
library(tidyverse)
library(janitor)
library(readxl)
library(cdlTools)
library(tools)

for (year in 2010:2020){
  #Read in xl file that lives on my computer. They can be found on the ATF site 
  # (https://www.atf.gov/resource-center/firearms-trace-data-2020), file called "Number of Firearms Sourced and Recovered in the United States and Territories"
  
  #First row of each file is "Recovery State", and first col is "Source State"
  #Skip the first row while reading and later deselect first col to remove junk.
  read_excel(paste("data/original_atf_data/sourcerecovery_by_state_cy", year, ".xlsx", sep = ""), skip = 1) |>
  select(-1) |>
  rename("Source_State" = "...2") |>
  #Remove footnotes
  head(55) |>
  #I wish to treat Source State as the dependent variable, so I transposed the dataframe.
  t() |>
  as.data.frame() |>
  #Fix generic col names caused by transposing. 
  row_to_names(row = 1) |>
  rownames_to_column("Recovery_State") |>
  #Convert data to long format for ease of combining. 
  pivot_longer(names_to = "Source_State", values_to = "Guns_Recovered", cols = -Recovery_State) |>
  mutate(Guns_Recovered = as.integer(Guns_Recovered)) |>
  mutate(Year = year) |>
  #Create 11 seperate files, one for each year 2010 through 2020, with the uncleaned, uncombined data. 
  write_rds(paste("data/wrangled_by_state/state-recovery", year, ".rds", sep = "")) 
}

#Combine each of the 11 seperate dataframes, one for each year between 2010 and 2020. 
condensed <- bind_rows(read_rds("data/wrangled_by_state/state-recovery2010.rds"), read_rds("data/wrangled_by_state/state-recovery2011.rds"), read_rds("data/wrangled_by_state/state-recovery2012.rds"), read_rds("data/wrangled_by_state/state-recovery2013.rds"), read_rds("data/wrangled_by_state/state-recovery2014.rds"), read_rds("data/wrangled_by_state/state-recovery2015.rds"), read_rds("data/wrangled_by_state/state-recovery2016.rds"), read_rds("data/wrangled_by_state/state-recovery2017.rds"), read_rds("data/wrangled_by_state/state-recovery2018.rds"), read_rds("data/wrangled_by_state/state-recovery2019.rds"), read_rds("data/wrangled_by_state/state-recovery2020.rds"))

#Some State names appear differently between annual reports. This code is meant to standardize them. 
condensed[condensed == "TOTALS"] <- "TOTAL"
#Worth noting that Guam refers to both Guam and the Northern Marina Islands.
condensed[condensed == "GUAM & NORTHERN MARIANA ISLANDS"] <- "GUAM"
condensed[condensed == "US VIRGIN  ISLANDS" | condensed == "US VIRGIN ISLND" | condensed == "US VIRGIN ISLANDS"] <- "VIRGIN ISLANDS"
condensed[condensed == "DST OF COLUMBIA"] <- "DISTRICT OF COLUMBIA"

#Covert to title case for convention
condensed$Recovery_State <- condensed$Recovery_State |> tolower() |> toTitleCase()
condensed$Source_State <- condensed$Source_State |> tolower() |> toTitleCase()

#! Creation of FIPS column is likely unncessecary. Keep it simple. 

#Creation of a FIPS code column that may be helpful for mapping. 
#condensed <- condensed |>
  #mutate("Source_FIPS" = cdlTools::fips(Source_State, to = "FIPS"))

#cdlTools does not include Virgin Islands FIPS, so do it manually. 
#condensed$Source_FIPS <- ifelse(condensed$Source_State == "VIRGIN ISLANDS", 78, condensed$Source_FIPS)

#Write final, condensed dataset. 
write_rds(condensed, "data/consolidated/combined2010to2020.rds")
