# libraries ----
library('RPostgreSQL')
library("devtools")
library("tidyverse")
library("stringr")
library("readxl")

# local settings ----
source("./local_settings.R")

# DB connections ----
# localhost
pg <- dbConnect(dbDriver("PostgreSQL"),
                user=db_user,
                dbname="working",
                host="localhost",
                password=.rs.askForPassword("Enter password:"))
# production
pg <- dbConnect(dbDriver("PostgreSQL"),
                user=db_user,
                dbname="caplter",
                host=db_host,
                password=.rs.askForPassword("Enter password:"))

# full-suite of ions ----

# SESSION: 2015-2016 winter deployment, read_csv removed the A \ B from the
# Sample.ID column...weird read_excel could not handle the dates
data <- read.csv('~/Desktop/Nutrient Supply Rate Data_ Project 1572_ Sally Wittlinger.csv', skip = 5, stringsAsFactors = F, na.strings = "NA")

# SESSION: 2016 summer deployment, full-suite of ions
# did not have any no problems with read_excel but used read.csv so I could use
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

if (dbExistsTable(pg, c('urbancndep', 'newprs'))) dbRemoveTable(pg, c('urbancndep', 'newprs')) # make sure tbl does not exist
dbWriteTable(pg, c('urbancndep', 'newprs'), value = moddat, row.names = F) # write temp table

# insert into results
dbGetQuery(pg,'
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
  "WAL..",
  plotid,
  burial_date,
  retrieval_date,
  id,
  result,
  flag,
  location,
  "X.Cation",
  "X.Anion",
  "Notes"
  FROM
  urbancndep.newprs
);')

# clean up
dbRemoveTable(pg, c('urbancndep', 'newprs'))

# END: 2015-2016 winter deployment, full-suite of ions

# N-only ----

# WesternAg is not consistent with their output, may need to adjust this function slightly to accomodate different date formats and column names
# note May 2016: in fact unuseable for winter 2015-2016 deployment owing, at least in part, to full suite of ions
prsmod <- function(dataframe) {

  dataframe <- dataframe %>%
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
  }

# sensu: newdata <- prsmod(imported)

# write new data data to pg
if (dbExistsTable(pg, c('urbancndep', 'newprs'))) dbRemoveTable(pg, c('urbancndep', 'newprs')) # make sure tbl does not exist
dbWriteTable(pg, c('urbancndep', 'newprs'), value = newdata, row.names = F) # write temp table

# insert into results
dbGetQuery(pg,'
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
    "WAL..",
    plotid,
    "Burial.Date",
    "Retrieval.Date",
    id,
    result,
    flag,
    location,
    "X..Cation",
    "X..Anion",
    notes
  FROM
    urbancndep.newprs
);')

# clean up
dbRemoveTable(pg, c('urbancndep', 'newprs'))

