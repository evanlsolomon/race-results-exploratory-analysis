library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(stringi)
library(lubridate)
library(ggmap)

# IMPORT DATA
setwd("/Users/evanlsolomon/environment/RProjects/biostatistics/final_project")
raw_runner_data <- read_csv("./20211122_112206_anonymized-raw-runner-data.csv")


#-------- SPLIT AND FORMAT Race COLUMN INTO race_year AND race_type COLUMNS
#split Race column to  race_year and race_type columns
pattern_for_split_race_years_and_type <- "([\\d]+) ([\\s\\S]+)"
raw_runner_data <- raw_runner_data %>%  extract(Race, into = c("race_year","race_type"), regex = pattern_for_split_race_years_and_type) 

# change race_type to factor
raw_runner_data$race_type <- as.factor(raw_runner_data$race_type)

#rename race_type level '10m' to '10M'
levels(raw_runner_data$race_type)[levels(raw_runner_data$race_type)=="10m"] <- "10M"

# change race_year and races_run types to integer
raw_runner_data[c("race_year", "races_run")] <- sapply(raw_runner_data[c("race_year", "races_run")], as.integer)


# ---------- PARSE HOMETOWNS ------------
# add extra columns to parse 'Hometown' into
raw_runner_data <- raw_runner_data %>%  mutate(city = NA, state = NA, country_1 = NA, country_2 = NA)

# How many Hometown's are NA? 5,854
raw_runner_data[which(is.na(raw_runner_data$Hometown)),]

# fill in missing hometown data by ID's, where you have the hometown for a runner from a different race year
raw_runner_data <- raw_runner_data %>% 
  group_by(ID) %>% 
  dplyr::arrange(race_year, .by_group = TRUE) %>%
  fill(Hometown, .direction = "updown") %>%
  ungroup(ID)

# Now it's 3,876 NAs in the hometown column
raw_runner_data[which(is.na(raw_runner_data$Hometown)),]

#374,325 total records
#these entries are na's in Hometown field 3,876 total 
hometown_is_na <- raw_runner_data%>% filter(is.na(Hometown))

# there are 370,449 records with a value in the hometown field
hometown_is_not_na <- raw_runner_data %>% filter(!is.na(Hometown))

# 1. search for all entries of just two letters. 
# send these to the state vector figure out what the later convert NR to NA
# there are 42 records
pattern_for_just_two_letters_in_hometown <- "^\\s*([a-zA-Z]{2})\\s*$"


is_just_two_letters <- raw_runner_data %>%
  filter(str_detect(Hometown, pattern_for_just_two_letters_in_hometown))
# add columns to parse hometown into
is_just_two_letters <- is_just_two_letters %>% mutate(state = Hometown)


# 2. search for all entries where hometown ends in a space and two letters
#    some two letter endings are not US states, see next chunk
#    there are 369,555 records
pattern_for_ends_in_a_space_two_letters <-"([,.\\w\\s]*) ([\\w]{2})$"

ends_in_a_space_two_letters <- raw_runner_data %>%
  filter(str_detect(Hometown, pattern_for_ends_in_a_space_two_letters))

# view unique values for comma space last two letters
str_match(ends_in_a_space_two_letters$Hometown, pattern_for_ends_in_a_space_two_letters) %>% unique()

#  2.a Non US two letter endings: QC, ON, AE, DD, EN , Mb, NS, MB, CN, SK, PC, VI, BC, NB, AB, AP, NR, NE
#      there are 504 of these records
pattern_for_non_US_two_letter_endings <- "([,.\\w\\s]*) (QC|ON|AE|DD|EN|Mb|NS|MB|CN|SK|PC|VI|BC|NB|AB|AP|NR)$"
non_US_two_letter_endings <- raw_runner_data %>%
  filter(str_detect(Hometown, pattern_for_non_US_two_letter_endings))

#  2.b just state abbreviation endings: negate match for non_US_two_letter_endings
#      369,051 records in this
US_two_letter_endings <- setdiff(ends_in_a_space_two_letters, non_US_two_letter_endings)

# view unique values for non US two letter endings
# get rid of double comma in record 353
non_US_two_letter_endings$Hometown <- str_replace(non_US_two_letter_endings$Hometown, ",{1,2}", ",")
non_US_two_letter_endings <- non_US_two_letter_endings %>%  separate(Hometown, c("city", "state"), ", ", remove = FALSE)

pattern_to_separate_US_two_letter_endings <- "^([\\w\\s';.-]*)[\\w, .]*([\\w\\s]{2})$"

# find problem entries -- there are just two, make the appropriate corrections below
US_two_letter_endings %>% 
  extract(Hometown, into = c("city", "state"), regex = pattern_to_separate_US_two_letter_endings, remove = FALSE) %>%
  filter(is.na(state)) %>% distinct(Hometown, .keep_all = TRUE) %>% select(ID, Hometown)

# Note that ID:126207's hometown is missing for only one year -- Hometown is Rockville MD
US_two_letter_endings[US_two_letter_endings$ID == 126207,]
US_two_letter_endings[US_two_letter_endings$ID == 126207,"Hometown"] <- "Rockville, MD"

# Also note that ID:177648's appears to have mistyped their hometown, and their hometown is: Columbia, MD
US_two_letter_endings[US_two_letter_endings$ID == 177648,]
US_two_letter_endings[US_two_letter_endings$ID == 177648,"Hometown"]  <- "Columbia, MD"

#no more problems, good to go
US_two_letter_endings %>% extract(Hometown, into = c("city", "state"), regex = pattern_to_separate_US_two_letter_endings, remove = FALSE)
US_two_letter_endings <- US_two_letter_endings %>% extract(Hometown, into = c("city", "state"), regex = pattern_to_separate_US_two_letter_endings, remove = FALSE)

#some states are not capitalized
US_two_letter_endings$state %>% unique()
#uppercase all states
US_two_letter_endings$state <- US_two_letter_endings$state %>% toupper()
# add United States as country to this group
US_two_letter_endings <- US_two_letter_endings %>% mutate(country_1 = "United States", country_2 = NA)
US_two_letter_endings

#  2.c separate out the NR records



# 3.  search for all hometown entries without a comma and without just two letters (STATE) at the end--send these to a country vector.
#     for just countries, put two commas in front.
#     this is 846 records
pattern_for_just_countries <- "[\\w .,]*\\b([\\w]{3,})$"
just_countries <- raw_runner_data %>%
  filter(str_detect(Hometown, pattern_for_just_countries))
str_match(just_countries$Hometown, pattern_for_just_countries)
#stri_split_fixed(just_countries$Hometown, pattern= ",", n=3, omit_empty = TRUE, simplify=TRUE)

just_countries_trial<- just_countries %>% separate(Hometown, into = c("city","country_1", "country_2"), sep = ",", remove = FALSE)
#stri_split_fixed(just_countries_trial$Hometown, pattern= ",", n=3, omit_empty = TRUE, simplify=TRUE)

#if country_1 is blank set country_1 equal to city and city as NA
just_countries_trial[is.na(just_countries_trial$country_1), "country_1"] <- just_countries_trial[is.na(just_countries_trial$country_1), "city"] 
#for those same countries that you copied over from city, you need to set the city field to NA
just_countries_trial[just_countries_trial$country_1 == just_countries_trial$city,"city"] <- NA

#trim
just_countries_trial$city<-  just_countries_trial$city %>% str_trim(side = "both")
just_countries_trial$state<-  just_countries_trial$state %>% str_trim(side = "both")
just_countries_trial$country_1<-  just_countries_trial$country_1 %>% str_trim(side = "both")
just_countries_trial$country_2<-  just_countries_trial$country_2 %>% str_trim(side = "both")



#change country_1:ETH to Ethiopia
just_countries_trial[just_countries_trial$country_1 == "ETH","country_1"] <- "Ethiopia"
just_countries_trial[just_countries_trial$country_1 == "KEN","country_1"] <- "Kenya"
just_countries_trial[just_countries_trial$country_1 == "GBR","country_1"] <- "Great Britain"
just_countries_trial[just_countries_trial$country_1 == "BER","country_1"] <- "Great Britain"
just_countries_trial[just_countries_trial$country_1 == "CRC","country_1"] <- "Costa Rica"
just_countries_trial[just_countries_trial$country_1 == "USA","country_1"] <- "United States"
just_countries_trial[just_countries_trial$country_1 == "MEX","country_1"] <- "Mexico"
just_countries_trial[just_countries_trial$country_1 == "BRN","country_1"] <- "Brunei"
just_countries_trial[just_countries_trial$country_1 == "BEL","country_1"] <- "Belgium"
just_countries_trial[just_countries_trial$country_1 == "NOR","country_1"] <- "Norway"
just_countries_trial[just_countries_trial$country_1 == "BRA","country_1"] <- "Brazil"
just_countries_trial[just_countries_trial$country_1 == "JPN","country_1"] <- "Japan"
just_countries_trial[just_countries_trial$country_1 == "SRI","country_1"] <- "Sri Lanka"
just_countries_trial[just_countries_trial$country_1 == "CZE","country_1"] <- "Czech Republic"
just_countries_trial[just_countries_trial$country_1 == "GER","country_1"] <- "Germany"
just_countries_trial[just_countries_trial$country_1 == "SWE","country_1"] <- "Sweden"
just_countries_trial[just_countries_trial$country_1 == "SUI","country_1"] <- "Switzerland"
just_countries_trial[just_countries_trial$country_1 == "NED","country_1"] <- "Netherlands"
just_countries_trial[just_countries_trial$country_1 == "AUS","country_1"] <- "Australia"
just_countries_trial[just_countries_trial$country_1 == "IRL","country_1"] <- "Ireland"
just_countries_trial[just_countries_trial$country_1 == "HKG","country_1"] <- "Hong Kong"
just_countries_trial[just_countries_trial$country_1 == "LBY","country_1"] <- "Libya"
just_countries_trial[just_countries_trial$country_1 == "THA","country_1"] <- "Thailand"
just_countries_trial[just_countries_trial$country_1 == "RSA","country_1"] <- "South Africa"
just_countries_trial[just_countries_trial$country_1 == "ZIM","country_1"] <- "Zimbabwe"
just_countries_trial[just_countries_trial$country_1 == "CAY","country_1"] <- "Cayman Islands"
just_countries_trial[just_countries_trial$country_1 == "IND","country_1"] <- "India"

#inspect unique city country_1 country_2 combos to find simplifications
just_countries_trial %>% select(city, country_1, country_2) %>% unique()
just_countries_trial[!is.na(just_countries_trial$country_2),] %>% select(ID, Hometown, city, country_1, country_2)

#fix the records where the algorithm unnecessarily split the town, but leave the records where the runner identifies with two countries
just_countries_trial[just_countries_trial$ID == 156519,]
just_countries_trial[just_countries_trial$ID == 156519,"country_1"] <- "Mexico"
just_countries_trial[just_countries_trial$ID == 156519,"country_2"] <- NA

#check again
just_countries_trial[!is.na(just_countries_trial$country_2),] %>% select(ID, Hometown, city, country_1, country_2)

just_countries_trial[just_countries_trial$ID == 183775,]
just_countries_trial[just_countries_trial$ID == 183775,"country_1"] <- "Australia"
just_countries_trial[just_countries_trial$ID == 183775,"country_2"] <- NA

#check again
just_countries_trial[!is.na(just_countries_trial$country_2),] %>% select(ID, Hometown, city, country_1, country_2)

just_countries_trial[just_countries_trial$ID == 199725,]
just_countries_trial[just_countries_trial$ID == 199725,"country_1"] <- "Switzerland"
just_countries_trial[just_countries_trial$ID == 199725,"country_2"] <- NA

#check again, you can leave these
just_countries_trial[!is.na(just_countries_trial$country_2),] %>% select(ID, Hometown, city, country_1, country_2)

just_countries_trial <- just_countries_trial %>% mutate(state = as.character(NA))

just_countries_trial %>% select(country_1) %>% unique()

just_countries_trial[just_countries_trial$country_1 == "New Jersey",]
just_countries_trial[just_countries_trial$ID == 201979, "state"] <- "NJ"
just_countries_trial[just_countries_trial$ID == 201979, "country_1"] <- "United States"

just_countries_trial[just_countries_trial$country_1 == "Vienna",]
just_countries_trial[just_countries_trial$ID == 138429, "state"] <- "NA"
just_countries_trial[just_countries_trial$ID == 138429, "city"] <- "Vienna"
just_countries_trial[just_countries_trial$ID == 138429, "country_1"] <- "Austria"

just_countries_trial

# 4. search for all entries where the hometown ends in a comma. Deal with these case by case--these are just city...
#     there are 5 records, these are all included in just_countries (check with intersect())
pattern_for_ends_in_comma <- "(,)$"
ends_in_comma <- raw_runner_data %>%
  filter(str_detect(Hometown, pattern_for_ends_in_comma))

ends_in_comma <- ends_in_comma %>% mutate(city = Hometown, state = NA, country_1 = NA, country_2 = NA)
ends_in_comma <- ends_in_comma %>% relocate(c(city, state, country_1, country_2))
ends_in_comma$city <- ends_in_comma$city %>% str_replace(pattern = ",", replacement = "")
ends_in_comma$city

# rbind all for subsets
# hometown_is_na
# is_just_two_letters
# non_US_two_letter_endings
# US_two_letter_endings
# just_countries_trial
# ends_in_comma

raw_runner_data <- rbind(hometown_is_na,is_just_two_letters,non_US_two_letter_endings,US_two_letter_endings,just_countries_trial,ends_in_comma)

#take a look
raw_runner_data

raw_runner_data <- raw_runner_data %>% relocate(races_run, .after = country_2)
raw_runner_data

#find all runners with Age == NA who have run more than once, maybe we can impute their age from a race entry that does include their age
# grab groups that have NA in Age, but also not all the records in that group are NA (NAND)
# there are 235 of these runners with NA for Age  (grouped by IDs)
raw_runner_data %>% 
  group_by(ID) %>%
  filter(n() != 1 & !all(is.na(Age)) & any(is.na(Age)))  %>% 
  summary()


# calculate age based on runner's other race records
raw_runner_data <- raw_runner_data %>%
  group_by(ID) %>% 
  dplyr::arrange(ID, .by_group = TRUE) %>%
  mutate(imputedAge= ifelse( !is.na(Age), Age, (race_year - first(race_year) + first(Age))
  )) %>%
  ungroup()

raw_runner_data <- raw_runner_data %>% relocate(imputedAge, .after = Age)

raw_runner_data <- raw_runner_data %>% 
                    mutate(age = imputedAge) %>% 
                    select(-Age, -imputedAge) %>%
                    relocate(age, .after = sex)

raw_runner_data


# clean up workspace: rm a bunch of variables
rm(ends_in_a_space_two_letters)
rm(ends_in_comma)
rm(hometown_is_na)
rm(hometown_is_not_na)
rm(is_just_two_letters)
rm(just_countries)
rm(just_countries_trial)
rm(non_US_two_letter_endings)
rm(US_two_letter_endings)
rm(pattern_for_ends_in_a_space_two_letters)
rm(pattern_for_ends_in_comma)
rm(pattern_for_just_countries)
rm(pattern_for_just_two_letters_in_hometown)
rm(pattern_for_non_US_two_letter_endings)
rm(pattern_for_split_race_years_and_type)
rm(pattern_to_separate_US_two_letter_endings)

# trim cities, states and countries
raw_runner_data$state<- raw_runner_data$state %>% str_trim(side = "both")
raw_runner_data$city<- raw_runner_data$city %>% str_trim(side = "both")
raw_runner_data$country_1<- raw_runner_data$country_1 %>% str_trim(side = "both")
raw_runner_data$country_2<- raw_runner_data$country_2 %>% str_trim(side = "both")

# ------Impute missing country info-----
# subset distinct combinations of records where country is NA and city or state is not NA,
# in other words, country is missing, and we have either the city, the state or both
# There are 551 of these with 210 distinct combinations of city and state
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) )
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) ) %>% distinct(city, state, .keep_all = TRUE)

# 345 of these are in Canada with 76 distinct city state combinations:
raw_runner_data[raw_runner_data$state %in% c("ON","AB", "QC", "BC", "SK", "NS", "NB"),]
raw_runner_data[raw_runner_data$state %in% c("ON","AB", "QC", "BC", "SK", "NS", "NB"),] %>% distinct(city, state, .keep_all = TRUE)

raw_runner_data[raw_runner_data$state %in% c("ON","AB", "QC", "BC", "SK", "NS", "NB"),"country_1"] <- "Canada"

# check the subset again -- 206 are left with 134 distinct city/state combinations
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) )
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) ) %>% distinct(city, state, .keep_all = TRUE)

# all of the VI for state are most likely Virginia except for St. Thomas which is a US Virgin Island
raw_runner_data[raw_runner_data$state %in% "VI",]

# change the St. Thomas VI state to NA and country_1 to U.S. Virgin Islands
raw_runner_data[raw_runner_data$ID == 32526 & raw_runner_data$state %in% "VI",] <- raw_runner_data %>% 
  filter(ID == 32526, state %in% "VI") %>%
  mutate(state = NA, country_1 = "U.S. Virgin Islands")

# change all remaining VI states to VA and their country to US
raw_runner_data[raw_runner_data$state %in% "VI",] <- raw_runner_data %>% 
  filter(state %in% "VI") %>% 
  mutate(state = "VA", country_1 = "United States")

# check the subset again -- 195 are left with 128 distinct city/state combinations
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) )
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) ) %>% distinct(city, state)


# convert APO FPO DPO to NA's as these are not actual locations
not_real_cities <- c("Apo", "APO", "Fpo", "FPO", "DPO", "Dpo")
raw_runner_data %>% filter(city %in% not_real_cities)
raw_runner_data[raw_runner_data$city %in% not_real_cities,] <- raw_runner_data[raw_runner_data$city %in% not_real_cities,] %>% 
  mutate(city = NA,
         state = ifelse(state %in% c("AE", "AP", "NR"), NA, state),
         country_1 = "United States")

# check the subset again -- 168 are left with 118 distinct city/state combinations
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) )
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) ) %>% distinct(city, state)
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) ) %>% count(city, state)

# take a look at the count by state
# there are 115 records with NR as state
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) )  %>% count(state)

# change this record's state from NR to NA to make the next section easier
raw_runner_data %>% filter(state %in% "NR" & is.na(city))
raw_runner_data[is.na(raw_runner_data$city) & raw_runner_data$state %in% "NR", "state"] <- NA

# fill in states that are NR with states for cities of identical name (only if there is 1 unique matching city name)  
# convert character "NA" in city to NA
raw_runner_data[raw_runner_data$city == "NA" & !is.na(raw_runner_data$city),]$city <- NA

# There are 114 records with state as NR
raw_runner_data[raw_runner_data$state %in% "NR",]

# cities of records with state as NR
cities_with_state_as_NR <- raw_runner_data[raw_runner_data$state %in% "NR","city"]
cities_with_state_as_NR

# id the state names you can fill--these are states of records that have city names that 
# are in the cities_with_state_as_NR tbl, but also only have one matching state
states_to_fill_in_for_records_with_state_as_NR <-
raw_runner_data[raw_runner_data$city %in% cities_with_state_as_NR$city & !(raw_runner_data$state %in% "NR"),] %>% 
  distinct(city, state) %>% add_count(city, name = "unique_states_for_city") %>% filter(unique_states_for_city == 1)

#states_for_cities_with_states_as_NR <- 
#  raw_runner_data[raw_runner_data$city %in% cities_with_state_as_NR$city,] %>% 
#  filter(!is.na(state) & !is.na(city) & state != "NR") %>% 
#  distinct(city, .keep_all = TRUE)
#
raw_runner_data <- raw_runner_data %>% 
  mutate(
    state = ifelse(
      state %in% "NR" & city %in% states_to_fill_in_for_records_with_state_as_NR$city,
      states_to_fill_in_for_records_with_state_as_NR[match(city,states_to_fill_in_for_records_with_state_as_NR$city),]$state,
      state
    )
  )


# There are 85 records with NA in the country that have US state abbreviations
raw_runner_data[(raw_runner_data$state %in% c("CA","CT","DE","GA","IL","MA","MD","ME","NJ","NY","OH","PA", "VA","WA","WV")) & is.na(raw_runner_data$country_1),]
raw_runner_data[(raw_runner_data$state %in% c("NY","VA","WA","CA","NJ","MD","PA","OH")) & is.na(raw_runner_data$country_1),] %>% distinct(city,state)

# change the US state abbreviations to have country_1 of "United States"
raw_runner_data[(raw_runner_data$state %in% c("CA","CT","DE","GA","IL","MA","MD","ME","NJ","NY","OH","PA", "VA","WA","WV")) & is.na(raw_runner_data$country_1), "country_1"] <- "United States"

# check the subset again -- 82 are left with 68 distinct city/state combinations
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) )
raw_runner_data %>% filter( is.na(country_1) & (!is.na(city) | !is.na(state)) ) %>% distinct(city, state)
raw_runner_data %>% filter(is.na(country_1) & !is.na(city) &  !is.na(state) ) %>% add_count(state, sort = TRUE)

# clean up some individual entries:
raw_runner_data %>% filter(ID == 27214)
raw_runner_data[raw_runner_data$ID == 27214 & raw_runner_data$city %in% "Kennebunkport",] <- raw_runner_data %>% 
  filter(ID == 27214 & city %in% "Kennebunkport") %>%
  mutate(state = "ME",
         country_1 = "United States")

raw_runner_data %>% filter(ID == 42170)
raw_runner_data[raw_runner_data$ID == 42170,] <- raw_runner_data %>% 
  filter(ID == 42170) %>%
  mutate(state = NA,
         country_1 = "Great Britain")

raw_runner_data %>% filter(ID == 58636)
raw_runner_data[raw_runner_data$ID == 58636,] <- raw_runner_data %>% 
  filter(ID == 58636) %>%
  mutate(state = "MD",
         country_1 = "United States")

raw_runner_data %>% filter(ID == 75481)
raw_runner_data[raw_runner_data$ID == 75481,] <- raw_runner_data %>% 
  filter(ID == 75481) %>%
  mutate(city="Brechin",
         state = NA,
         country_1 = "Scotland")

raw_runner_data %>% filter(ID == 167919)
raw_runner_data[raw_runner_data$ID == 167919 & raw_runner_data$city %in% "Washington",] <- raw_runner_data %>% 
  filter(ID == 167919 & city %in% "Washington") %>%
  mutate(state = "DC",
         country_1 = "United States")

raw_runner_data %>% filter(ID == 50567)
raw_runner_data[raw_runner_data$ID == 50567,] <- raw_runner_data %>% 
  filter(ID == 50567) %>%
  mutate(state = "ON",
         country_1 = "Canada")

raw_runner_data %>% filter(ID == 181545)
raw_runner_data[raw_runner_data$ID == 181545,] <- raw_runner_data %>% 
  filter(ID == 181545) %>%
  mutate(city = "Sackville",
         state = "NS",
         country_1 = "Canada")


raw_runner_data %>% filter( is.na(country_1) & !is.na(city) & is.na(state) ) %>% distinct(city, state)
raw_runner_data[is.na(raw_runner_data$country_1) & !is.na(raw_runner_data$city) & is.na(raw_runner_data$state) ,"country_1"] <- "Great Britain"


raw_runner_data %>% filter(is.na(country_1) & !is.na(city) &  !is.na(state) )
raw_runner_data %>% filter(is.na(country_1) & !is.na(city) &  !is.na(state) ) %>% add_count(state)
raw_runner_data %>% filter(is.na(country_1) & !is.na(city) &  !is.na(state) ) %>% distinct(city, state, .keep_all = TRUE)

ids_of_runners_with_NR_for_state <- raw_runner_data %>% filter(state %in% "NR") %>% .$ID

# fill in states that are NR based on states on record from same runner in other years
raw_runner_data[raw_runner_data$races_run>1 & raw_runner_data$ID %in% ids_of_runners_with_NR_for_state,] <- raw_runner_data %>% 
  group_by(ID) %>% 
  filter(races_run >1 & ID %in% ids_of_runners_with_NR_for_state) %>% 
  mutate(state = ifelse(state %in% c("NR", "MA"),NA,state)) %>% 
  fill(state, country_1, .direction = "updown") %>% 
  ungroup()

raw_runner_data %>% filter(is.na(country_1) & !is.na(city) &  !is.na(state) )
# check the subset again -- 42 are left with 37 distinct city/state combinations
# leave them for now
raw_runner_data %>% filter( state %in% "NR" )
raw_runner_data %>% filter( state %in% "NR" ) %>% distinct(city, state)


# --------- clean times ---------
# Note: 'a' The 2015 running of the 10 mile race had to be cut short and had a total distance of 9.39 miles.
# time adjustments for 2015 9.386205 distance run -- multiply by 10/9.386205 to get projected numbers /pace
# 'b' 2019 also had a 9.96 distance due to a misplaced turn marker -- no adjustment made
# 2020 has an adjustment with letter c attached to times
# c: The 2020 ten mile and 5K were replaced by Virtual Runs due to the cancellation due to the COVID-19 virus.

#create an adjusted time column to correct for 2015,2019,2020 match website corrections and have a time column that contains time data objects
raw_runner_data %>% filter(race_year %in% c(2015,2019,2020) & race_type == "10M") %>% select(race_year, race_type, ID, Time)

#match all time patterns with an optional a, b, or c at the end for 2015, 2019, or 2020
#total of 372,074 records
typical_time_pattern <- raw_runner_data[str_which(raw_runner_data$Time, "^([0-9]{0,1}:{0,1})([0-9]{1,2}):([0-9]{1,2})(a|b|c{0,1})$"),]

# total of 2,250 with NA in Time
non_typical_time_pattern <- setdiff(raw_runner_data, typical_time_pattern)

# chop the 'a|b|c' in the time, keeping the time for every runner
raw_runner_data <- raw_runner_data %>% 
  mutate(imputed_time = gsub("a|b|c.*","",raw_runner_data$Time))

# times less than 1 hour need to have 00: prepended
raw_runner_data %>% filter(race_year == 2015)%>% select(ID, race_year, race_type, Time, imputed_time, Hometown)
raw_runner_data$imputed_time <- ifelse(nchar(raw_runner_data$imputed_time) < 6, paste0("00:",raw_runner_data$imputed_time), raw_runner_data$imputed_time )
# see that it's fixed
raw_runner_data %>% filter(race_year == 2015)%>% select(ID, race_year, race_type, Time, imputed_time, Hometown)


#check that the conversion kept the same number of NAs (2250) in the imputed_time column
raw_runner_data[is.na(raw_runner_data$imputed_time),] %>% select(ID, race_year, race_type, Time, imputed_time, Hometown)

#the warning from the below line is regarding the 2250 NAs
raw_runner_data$imputed_time <- raw_runner_data$imputed_time %>% hms()

# peek at 2015 10M times before adjustment
raw_runner_data %>% filter(race_year == 2015)%>% select(ID, race_year, race_type, Time, imputed_time, Hometown)


# update only the 'a' 2015 records that need a time adjustment
raw_runner_data <- raw_runner_data %>% 
  mutate(imputed_time = if_else(str_detect(Time, "a$"), seconds_to_period(round(unclass(as.duration(imputed_time))*10/9.386205)), imputed_time ))

# save imputed_time as difftime h:m:s format 
raw_runner_data <- raw_runner_data %>% mutate(imputed_time = hms::as_hms(as.difftime(imputed_time)))

# peek at 2015 10M times after adjustment
raw_runner_data %>% filter(race_year == 2015)%>% select(ID, race_year, race_type, Time, imputed_time, Hometown)

# clean up workspace
rm(cities_with_state_as_NR)
rm(states_to_fill_in_for_records_with_state_as_NR)
rm(ids_of_runners_with_NR_for_state)
rm(not_real_cities)
rm(typical_time_pattern)
rm(non_typical_time_pattern)


#save mostly cleaned dataset
fname <- paste0(format(lubridate::now(), "%Y%m%d_%H%M%S_"), "clean-anonymized-runner-data.csv")
write_csv(raw_runner_data, fname)

rm(fname)
rm(raw_runner_data)
