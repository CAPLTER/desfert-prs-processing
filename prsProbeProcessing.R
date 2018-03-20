
# README



# libraries ----
library(RPostgreSQL)
library(devtools)
library(tidyverse)
library(stringr)
library(readxl)


# database connections ----
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local


# FULL-SUITE DATA ----

# SESSION: 2015-2016 winter deployment ----

# read_csv removed the A \ B from the
# Sample.ID column...weird read_excel could not handle the dates
data <- read.csv('~/Desktop/Nutrient Supply Rate Data_ Project 1572_ Sally Wittlinger.csv', skip = 5, stringsAsFactors = F, na.strings = "NA")

# SESSION: 2016 summer deployment (full-suite) ----

# did not have any problems with read_excel but used read.csv so I could use
# the code below with minimal reworking. Had to change the date format and name
# of cations and anions but otherwise everything below worked fine
data <- read.csv('~/Desktop/Nutrient Supply Rate Data_ Project 1673_ Sally Wittlinger.csv', skip = 5, stringsAsFactors = F, na.strings = "NA")

moddat <- data %>%
  filter(!is.na(Sample.ID)) %>%
  filter(Sample.ID != "") %>%
  gather(id, result, Total.N:Cd) %>%
  mutate(burial_date = as.POSIXct(Burial.Date, format = "%Y-%m-%d")) %>%
  mutate(retrieval_date = as.POSIXct(Retrieval.Date, format = "%Y-%m-%d")) %>%
  mutate(plotid = as.numeric(str_match(Sample.ID, "\\d+"))) %>%
  mutate(location = ifelse(gsub("[[:digit:]]", "", Sample.ID, ignore.case = T) == 'A', 'under plant',
                    ifelse(gsub("[[:digit:]]", "", Sample.ID, ignore.case = T) == 'B', 'between plant', NA))) %>%
  mutate(location = ifelse(plotid > 75, 'BLANK', location)) %>%
  mutate(flag = ifelse(result <= 2.0 & (id == 'Total.N' | id == 'NO3..N' | id == 'NH4..N' | id == 'Ca' | id == 'S'), "below detection limit", NA)) %>%
  mutate(flag = ifelse(result <= 4.0 & ( id == 'Mg' | id == 'K' ), "below detection limit", flag)) %>%
  mutate(flag = ifelse(result <= 0.2 & ( id == 'P' | id == 'Mn' | id == 'Cu' | id == 'Zn' | id == 'B' | id == 'Pb' | id == 'Cd'), "below detection limit", flag)) %>%
  mutate(flag = ifelse(result <= 0.4 & ( id == 'Fe' | id == 'Al' ), "below detection limit", flag)) %>%
  mutate(id = gsub("\\.", "-", id)) %>%
  mutate(id = gsub("\\--", "-", id)) %>%
  mutate(WAL.. = as.numeric(WAL..)) %>%
  mutate(Notes = ifelse(Notes == "", NA, Notes)) %>%
  select(WAL.., plotid, burial_date, retrieval_date, id, result, flag, location, X.Cation = X..Cation, X.Anion = X..Anion, Notes)

moddat <- moddat %>% mutate(result = as.numeric(result)) # had to add this for summer 2016


# N-ONLY DATA ----

# SESSION: winter 2017-2018 ----

winter_2017_2018 <- read_excel('~/Desktop/Nitrogen Supply Rate Data_ Project 1836_ Sally Wittlinger.xlsx',
                               skip = 5)

newprs <- winter_2017_2018 %>% 
  filter(!is.na(`Sample ID`)) %>% 
  rename(`Total-N` = `Total N`) %>% 
  gather(id, result, `Total-N`:`NH4-N`) %>% # stack
  mutate(
    plotid = as.numeric(gsub("[[:alpha:]]", "", `Sample ID`)),
    location = ifelse(gsub("[[:digit:]]", "", `Sample ID`, ignore.case = T) == 'A', 'under plant',
                      ifelse(gsub("[[:digit:]]", "", `Sample ID`, ignore.case = T) == 'B', 'between plant', NA)), # location
    location = ifelse(plotid > 75, 'BLANK', location), # location if blank
    flag = ifelse(result <= 2.0, "below detection limit", NA) # flag bdl
  ) 



# SESSION: summer 2017 ----

# using read.csv so as to be copasetic with the prsmod function
data <- read.csv('~/Desktop/Nutrient Supply Rate Data_ Project 1786_ Sally Wittlinger.csv', skip = 5, stringsAsFactors = F)
data[data == ""] <- NA
data <- data %>% filter(!is.na(Sample.ID))
newprs <- prsmod(data)


# SESSION: winter 2016-2017 ----

# winter 2016-2017 PRS N data were too different to use the prsmod script. These
# data may be too nuanced to script, maybe just follow general steps.
# Bewildering that WesternAg output is so different with each run.
winter_2016_2017 <- read_excel('~/Desktop/Nutrient Supply Rate Data_ Project 1720_ Sally Wittlinger.xlsx',
                               skip = 5,
                               col_types = c('numeric', 'text', rep('date', 2), rep('numeric', 2), 'text', rep('numeric', 3), rep('text', 14)))

winter_2016_2017 <- winter_2016_2017[,c(1:10)]

winter_2016_2017 <- winter_2016_2017 %>% 
  filter(!is.na(`Sample ID`)) %>% 
  rename(`NH4-N` = `NH4+-N`) %>% 
  rename(`NO3-N` = `NO3--N`) %>% 
  rename(`Total-N` = `Total N`) %>% 
  gather(id, result, `Total-N`:`NH4-N`) %>% # stack
  mutate(plotid = as.numeric(gsub("[[:alpha:]]", "", `Sample ID`))) %>% # extract plot id
  mutate(location = ifelse(gsub("[[:digit:]]", "", `Sample ID`, ignore.case = T) == 'A', 'under plant',
                           ifelse(gsub("[[:digit:]]", "", `Sample ID`, ignore.case = T) == 'B', 'between plant', NA))) %>% # location
  mutate(location = ifelse(plotid > 75, 'BLANK', location)) %>% # location if blank
  mutate(flag = ifelse(result <= 2.0, "below detection limit", NA)) %>% # flag bdl
  mutate(id = gsub("\\.", "-", id)) 


# generic processing function ----

# WesternAg is not consistent with their output, may need to adjust this
# function slightly to accomodate different date formats and column names note
# May 2016: in fact unuseable for winter 2015-2016 deployment owing, at least in
# part, to full suite of ions
prsmod <- function(dataframe) {

  dataMod <- dataframe %>%
    gather(id, result, Total.N:NH4.N) %>% # stack
    mutate(Burial.Date = as.POSIXct(Burial.Date, format = "%Y-%m-%d")) %>% # date format
    mutate(Retrieval.Date = as.POSIXct(Retrieval.Date, format = "%Y-%m-%d")) %>% # date format
    mutate(plotid = as.numeric(gsub("[[:alpha:]]", "", Sample.ID))) %>% # extract plot id
    mutate(location = ifelse(gsub("[[:digit:]]", "", Sample.ID, ignore.case = T) == 'A', 'under plant',
                             ifelse(gsub("[[:digit:]]", "", Sample.ID, ignore.case = T) == 'B', 'between plant', NA))) %>% # location
    mutate(location = ifelse(plotid > 75, 'BLANK', location)) %>% # location if blank
    mutate(flag = ifelse(result <= 2.0, "below detection limit", NA)) %>% # flag bdl
    mutate(id = gsub("\\.", "-", id)) %>% # replace dots in analyte name with dashes
    select(WAL.., plotid, Burial.Date, Retrieval.Date, id, result, flag, location, X..Cation, X..Anion, Notes) # pare and order
  
  return(dataMod)
  
  }

# sensu: newprs <- prsmod(imported)


# data to database ----

# write new data data to pg
if (dbExistsTable(pg, c('urbancndep', 'newprs'))) dbRemoveTable(pg, c('urbancndep', 'newprs')) # make sure tbl does not exist
dbWriteTable(pg, c('urbancndep', 'newprs'), value = newprs, row.names = F) # write temp table

# need to alter database fields namely because dbWriteTable creates a date field
# with time zones. Changing the Wal ID to integer may or may not be required
# depending on how WesternAg provides the data

# note that WesternAg uses different column names almost every time so these
# have to be updated for each run

dbExecute(pg,'
  ALTER TABLE urbancndep.newprs 
    ALTER COLUMN "Burial Date" TYPE date USING ("Burial Date"::date), 
    ALTER COLUMN "Retrieval Date" TYPE date USING ("Retrieval Date"::date),
    ALTER COLUMN "WAL #" TYPE integer USING ("WAL #"::integer);
')


# note that WesternAg uses different column names almost every time so these
# have to be updated for each run
dbExecute(pg,'
INSERT INTO urbancndep.prs_analysis
(
  wal_id,
  plot_id,
  start_date,
  end_date,
  analyte,
  final_value,
  flag,
  location_within_plot,
  num_cation_probes,
  num_anion_probes,
  notes
)
(
  SELECT
    "WAL #",
    plotid,
    "Burial Date",
    "Retrieval Date",
    id,
    result,
    flag,
    location,
    "# Cation",
    "# Anion",
    "Notes"
  FROM
    urbancndep.newprs
);')

# clean up
dbRemoveTable(pg, c('urbancndep', 'newprs'))
