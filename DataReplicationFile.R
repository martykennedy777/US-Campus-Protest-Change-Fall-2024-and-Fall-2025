##########################################################################################
##########################################################################################
##########################################################################################
#Part 1: Creating Dataset
##########################################################################################
##########################################################################################
##########################################################################################

##############################
#Loading Packages and Data
##############################
library(tidytext)
library(dataverse)
library(readr)
library(dplyr)
library(lubridate)
library(geosphere)
library(writexl)
library(readxl)
library(fuzzyjoin)
library(stringi)
library(sf)

#IPEDS data - list of all colleges/universities in US
colleges1 <- read.csv("Input/Replication_IPEDSData.csv")

#Manual check of protest on college campuses
correct <- read_xlsx("Input/Replicationkey_correctprotests.xlsx")

#Manual check of protests NOT on college campuses
delete <- read_xlsx("Input/Replicationkey_deleteprotests.xlsx")

#############################################
#Step 1: Download CCC Data from Website#
#############################################
Sys.setenv(DATAVERSE_SERVER = "dataverse.harvard.edu")

#2021-2024 protest dataset
campus_2021_2024 <- get_dataframe_by_id(
  fileid = 10822959, .f = read_csv,
  original = TRUE)

#2025-present protest dataset
new_ccc <- get_dataframe_by_id(
  #Look at linked website under "Metadata" for fileid, which will update when the CCC is updated.
  #https://dataverse.harvard.edu/file.xhtml?fileId=13448233&version=13.0
  fileid = 13448233, .f = read_csv,
  original = TRUE)

#Altering some data structure to match other CCC datasets
colnames(new_ccc)[which(names(new_ccc) == "location_detail")] <- "location"
class(new_ccc$arrests)  <- "character"
new_ccc$date <- as.Date(new_ccc$date, format =  "%m/%d/%Y")
new_ccc$date <- as.Date(new_ccc$date, format =  "%Y-%m-%d")
class(new_ccc$fips_code) <- "character"

for (i in 1:30){
  oldname <- paste0("source_",i)
  newname <- paste0("source",i)
  colnames(campus_2021_2024)[which(names(campus_2021_2024) == oldname)] <- newname}

campus_2021_2024 <- campus_2021_2024 %>%
  rename(location = location_detail)

#create full dataset
new_ccc <- full_join(new_ccc,campus_2021_2024)

####################################################################
#Step 2: Narrowing Months & Years (Fall 2024 & 2025 Semesters)
####################################################################
newmonth <- new_ccc %>% mutate(date = as.Date(date), year = year(date))
newmonth <- newmonth[newmonth$year %in% c(2024, 2025), ]
newmonth <- newmonth[format(newmonth$date, "%m") %in% c("08","09","10","11","12"), ]


#################################################################
#Step 3: Four-Pronged Approach to Extract Protest Data
#################################################################

#========================================================================
#Step 3a: Chenoweth et al. (2026) approach
#========================================================================
schools <- c("college(?! (?:st(reet)?|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights|point|green|athletic))",
             "university(?! (?:st(reet)?|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights|city|behavioral|hospital|plaza|lakes|office|irving))",
             "school(?! (?:st(reet)?\\b|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights))",
             "\\bcooper union",
             "institute of technology",
             "\\bpoly(technic (state )?(?:institute|university))?",
             "auraria campus",
             "pentacrest",
             "(?:naval|air force|military) academy|west point(?! hwy)",
             "\\b(?:c|s)uny\\b",
             "\\buc\\b(?! theatre)")

regex_schools <- paste(schools, collapse = "|")

newmonth <- newmonth %>%
  mutate(schools = ifelse(grepl(regex_schools, location, ignore.case = TRUE, perl = TRUE), 1, 0))

chenoweth_colleges <- newmonth %>% filter(schools == 1)

#========================================================================
#Step 3b: University and College approach
#========================================================================
my_patterns <- c("University","university","College","college")
universityprotest <- newmonth[grepl(paste(my_patterns, collapse = "|"), newmonth$location), ]

#========================================================================
#Step 3c: Campus and Students approach
#========================================================================
studentprotests <- newmonth[
  grepl("students", newmonth$participants, ignore.case = TRUE),]

campus_patterns <- c("campus","Campus")
campusprotests <- newmonth[grepl(paste(campus_patterns, collapse = "|"), newmonth$location), ]

student_and_campus_protests <- rbind(studentprotests,campusprotests)

#========================================================================
#Step 3c: Geographic approach
#========================================================================
geographic <- newmonth[format(newmonth$date, "%m") %in% c("08","09","10","11","12"), ]

# Turn off s2 to avoid geometry validity issues
sf_use_s2(FALSE)

# Convert to sf object (dropping missing coords)
geographic_clean <- geographic[!is.na(geographic$lat) & !is.na(geographic$lon), ]
geographic_sf <- st_as_sf(geographic_clean, coords = c("lon", "lat"), crs = 4326)

# Load college campuses from ArcGIS
colleges_sf <- st_read("https://services5.arcgis.com/HDRa0B57OVrv2E1q/arcgis/rest/services/Colleges_and_Universities_Campuses/FeatureServer/0/query?where=1%3D1&outFields=*&f=geojson")

# Fix any invalid geometries and match CRS
colleges_sf <- st_make_valid(colleges_sf)
colleges_sf <- st_transform(colleges_sf, crs = 4326)

# Add TRUE/FALSE and school guess columns for whether each protest is on a campus
result <- st_join(geographic_sf, colleges_sf, join = st_within)
geographic_clean$on_campus <- lengths(st_within(geographic_sf, colleges_sf)) > 0
geographic_clean$college_guess <- result$NAME

geographic_true <- geographic_clean %>% filter(on_campus)


#################################################################
#Step 4: Identifying unique protests events from each approach
#################################################################

#How many protests did each method identify -- total
universityprotest <- unique(universityprotest)
nrow(universityprotest) #2082

student_and_campus_protests <- unique(student_and_campus_protests)
nrow(student_and_campus_protests) #1434

geographic_true <- unique(geographic_true)
nrow(geographic_true) #674

chenoweth_colleges <- unique(chenoweth_colleges)
nrow(chenoweth_colleges) #2220

#Protests in university/college method not captured in Chenoweth method
chenoweth_colleges$id <- paste(chenoweth_colleges$date, chenoweth_colleges$location, chenoweth_colleges$source1)
universityprotest$id <- paste(universityprotest$date, universityprotest$location, universityprotest$source1)

university_only <- universityprotest %>% filter(!id %in% chenoweth_colleges$id)
nrow(university_only) #302

## Protests in student/campus method not captured in Chenoweth or university/college method
student_and_campus_protests$id <- paste(student_and_campus_protests$date, 
                                        student_and_campus_protests$location, student_and_campus_protests$source1)

student_and_campus_only <- student_and_campus_protests %>% filter(!id %in% c(chenoweth_colleges$id, universityprotest$id))
nrow(student_and_campus_only) #273

## Protests in geographic method not captured in Chenoweth, university/college, or student/campus method
geographic_true$id <- paste(geographic_true$date, geographic_true$location, geographic_true$source1)

geography_only <- geographic_true %>%
  filter(!id %in% c(chenoweth_colleges$id, universityprotest$id, student_and_campus_protests$id))
nrow(geography_only) #482

###################################################################
#Step 5: Combining All 4 data collection approaches into 1 df 
###################################################################
#Noting Method for each df
chenoweth_colleges$method <- "Chenoweth"
university_only$method <- "university"
student_and_campus_only$method <- "student_campus"
geography_only$method <- "geography"

##Removing columns to get matches
setdiff(colnames(student_and_campus_only),colnames(university_only)) #none
setdiff(colnames(geography_only),colnames(university_only))
#geography_only <- geography_only %>% select(-college_guess)
setdiff(colnames(chenoweth_colleges),colnames(university_only))
chenoweth_colleges <- chenoweth_colleges %>% select(-schools)

newmonth_final <- bind_rows(university_only,student_and_campus_only,geography_only,chenoweth_colleges)
table(newmonth_final$method)


###################################################################
#Step 6: Confirming Protests on College Campuses
###################################################################
protests <- newmonth_final

##IPEDS method: If the protest location directly matches a college/university name, it was on that college's campus
colleges1 <- colleges1 %>% distinct(school.name, .keep_all = TRUE)
colleges1 <- colleges1 %>% filter(school.degrees_awarded.highest >= 2)

protests1 <- protests %>% mutate(match_key = location) %>% left_join(
  colleges1 %>% mutate(match_key = school.name), by = "match_key") %>%
  select(-match_key) %>% relocate(`school.name`, .before = location)

sum(is.na(protests1$school.name)) #2266 not identified by IPEDS method

##Other school method: Removing protests from non-college schools (e.g., elementary, high schools)
nrow(protests1) #3277
protests1 <- protests1[!grepl("high school", protests1$location, ignore.case = TRUE), ]
protests1 <- protests1[!grepl("middle school", protests1$location, ignore.case = TRUE), ]
protests1 <- protests1[!grepl("elementary school", protests1$location, ignore.case = TRUE), ]
protests1 <- protests1[!grepl("school district", protests1$location, ignore.case = TRUE), ]
protests1 <- protests1[!grepl("public school", protests1$location, ignore.case = TRUE), ]
protests1 <- protests1[!grepl("public schools", protests1$location, ignore.case = TRUE), ]
nrow(protests1) #2923


##Manual Check: use excel with manual check to confirm protests on college campus
sum(is.na(protests1$school.name)) #1888

#getting text style similar
normalize_location <- function(x) {x %>%
    stringi::stri_trans_general("Latin-ASCII") %>% 
    gsub("[\r\n]+", " ", .) %>%                    
    gsub("\\s+", " ", .) %>%                       
    trimws() %>% tolower()}

protests2 <- protests1 %>%
  mutate(location = normalize_location(location),
         locality = normalize_location(locality),
         state    = normalize_location(state))

correct <- correct %>%
  mutate(location = normalize_location(location),
         locality = normalize_location(locality),
         state    = normalize_location(state))

delete <- delete %>%
  mutate(location = normalize_location(location),
         locality = normalize_location(locality),
         state    = normalize_location(state))

# Remove protests identified as NOT on college campuses 
protests2 <- protests2 %>%
  anti_join(delete, by = c("locality","state","location"))
sum(is.na(protests2$school.name)) #836

#Making a unique dictionary of identified college campus protest
correct_unique <- correct %>% group_by(locality, state, location) %>%
  summarise(school.name = first(school.name),.groups = "drop")

# Attach correct school names
protests3 <- protests2 %>% select(-school.name) %>% left_join(correct_unique,
                                                              by = c("locality","state","location"))

sum(is.na(protests3$school.name)) #0 - all protests labeled with college campus
protests3 <- protests3 %>% relocate(school.name, .before = location)

###################################################################
#Step 7: Crafting Final Dataset
###################################################################
##Adding in college information
protests4 <- protests3 %>%
  left_join(colleges1,  by = "school.name",suffix = c("", ".new")) %>%
  mutate(school.state = coalesce(school.state, school.state.new),
         school.ownership = coalesce(school.ownership, school.ownership.new),
         school.degrees_awarded.highest = coalesce(school.degrees_awarded.highest, school.degrees_awarded.highest.new),
         school.degrees_awarded.predominant = coalesce(school.degrees_awarded.predominant, school.degrees_awarded.predominant.new),
  ) %>% select(-ends_with(".new"))


##Manual Fixes
#1. Northeastern University in Arlington - Northeastern in Boston, but
#Arlington campus, so therefore manually editing to be Arlington,VA
num <- which(is.na(protests4$school.ownership))
protests4$school.city[num] <- "Arlington"
protests4$school.state[num] <- "VA"
protests4$school.ownership[num] <- 2
protests4$school.ownership[num] <- 2
protests4$school.degrees_awarded.highest[num] <- 4
protests4$school.degrees_awarded.predominant[num] <- 3

#2. There is one protest picked up in Guam, removing since looking at US states/DC and n = 1
protests4 <- protests4 %>% filter(state != "gu")

##Removing protests at 2-year degree colleges (n = 41)
protests4 <- protests4 %>% filter(school.degrees_awarded.highest != 2)

sum(is.na(protests4$school.ownership)) #should be 0
protests4 <- protests4 %>% relocate(school.name, .before = location)
protests4 %>% count(method)
#1771 protests --> Chenoweth method
#18 protests --> University/college approach not in Chenoweth approach
#17 protests --> Student/Campus approach not in Chenoweth or college/university approach
#7 protests --> Geographic approach not in Chenoweth, university/college, or campus/student approach


#Trimming down on rows
protests5 <- protests4 %>% select(date, locality, state, school.name,location, title, event_type, organizations,participants,
                                  claims_summary,size_mean,year,schools,method,school.city,school.state,school.ownership,
                                  school.degrees_awarded.highest)

##########################################################################################
##########################################################################################
##########################################################################################
#Part 2: Data Analysis
##########################################################################################
##########################################################################################
##########################################################################################

##############################
#Loading Packages and Data
##############################
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(gtsummary)

#political party demarcation
party <- read_excel("Input/Replicationpolitical_party_breakdown.xlsx")


##############################
#Step 1: Cleaning initial data
##############################
party$state <- tolower(party$state)
party <- party %>% select(state,Final_Demarcation)
protests5 <- left_join(protests5,party, by = "state")

protests5 %>% count(school.name) %>% arrange(desc(n))

####################################
#Step 2: Creating Graph Variables
####################################

#public/private status
protests5$public <- ifelse(protests5$school.ownership == 1, 1, 0)
protests5 %>% count(public)

#Final_Demarcation --> changing name and adding DC
protests5 <- protests5 %>% rename(state_politics = Final_Demarcation)
protests5$state_politics <- ifelse(protests5$state == "dc","D",protests5$state_politics)

#region 
###Trying this with regions
ne <- c("CT", "ME", "MA", "NH", "RI", "VT","NJ", "NY", "PA")
mw <- c("IL", "IN", "MI", "OH", "WI","IA", "KS", "MN", "MO", "NE", "ND", "SD")
s <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV","AL", "KY", "MS", "TN","AR", "LA", "OK", "TX")
w <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY","AK", "CA", "HI", "OR", "WA")

protests5$region <- ifelse(protests5$school.state %in% ne, "northeast",
                           ifelse(protests5$school.state %in% mw, "midwest",
                                  ifelse(protests5$school.state %in% s, "south",
                                         ifelse(protests5$school.state %in% w, "west",NA))))

##Putting DC as a part of south
protests5$region <- ifelse(protests5$state == "dc","south",protests5$region)

#gaza protest
patterns <- c("Palestin", "Gaza", "Israel")
combined <- paste(patterns, collapse = "|")

pal <- grepl(combined, protests5$claims_summary)
protests5$pal <- pal

#schools with gaza protest ARRESTS
schools_w_arrests <- c("Arizona State University Campus Immersion","California State Polytechnic University-Humboldt",
                       "California Polytechnic State University-San Luis Obispo","Case Western Reserve University",
                       "CUNY City College","Columbia University in the City of New York",
                       "Dartmouth College","DePaul University","Emerson College","Emory University",
                       "Fashion Institute of Technology","Florida State University","Fordham University",
                       "George Washington University","Indiana University-Bloomington",
                       "Massachusetts Institute of Technology","New Mexico State University-Main Campus",
                       "The New School","New York University","North Carolina State University at Raleigh",
                       "Northeastern University","Northern Arizona University","Ohio State University-Main Campus",
                       "Portland State University","Princeton University","Stanford University",
                       "State University of New York at New Paltz","SUNY at Purchase College","Stony Brook University",
                       "School of the Art Institute of Chicago","Tulane University of Louisiana",
                       "University at Buffalo","University of Arizona","University of California-Berkeley",
                       "University of California-Irvine","University of California-Los Angeles",
                       "University of California-San Diego","University of California-Santa Cruz",
                       "University of Colorado Denver/Anschutz Medical Campus","University of Connecticut",
                       "University of Florida","University of Georgia","University of Houston",
                       "University of Illinois Urbana-Champaign","University of Kansas","University of Mary Washington",
                       "University of Massachusetts-Amherst","University of Michigan-Ann Arbor",
                       "University of Minnesota-Twin Cities","University of New Hampshire-Main Campus",
                       "University of New Mexico-Main Campus","University of North Carolina at Chapel Hill",
                       "University of North Carolina at Charlotte","University of North Florida",
                       "University of Notre Dame","University of Pennsylvania",
                       "University of Pittsburgh-Pittsburgh Campus","University of South Carolina-Columbia",
                       "University of South Florida","University of Southern California",
                       "The University of Tennessee-Knoxville","The University of Texas at Austin",
                       "The University of Texas at Dallas","University of Utah",
                       "University of Virginia-Main Campus","University of Wisconsin-Madison",
                       "University of Wisconsin-Milwaukee","Virginia Commonwealth University",
                       "Virginia Polytechnic Institute and State University","Washington University in St Louis",
                       "Wayne State University","Xavier University","Yale University")

protests5$arrests <- ifelse(protests5$school.name %in% schools_w_arrests,1,0)

#########################################################
#Step 3: Getting % Changes for each variable and variable grouping
##########################################################

# ================================================================
###Variable 1: Public/Private
# ================================================================
justpubpriv <- protests5 %>% count(public,year)
pubpriv <- justpubpriv %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total <- pubpriv

# ================================================================
###Variable 2: State
# ================================================================
juststate <- protests5 %>% count(state_politics,year)
state <- juststate %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total <- bind_rows(total,state)

# ================================================================
###Variable 3: Region
# ================================================================
region1 <- protests5 %>% count(region,year)
region2 <- region1 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total <- bind_rows(total,region2)

# ================================================================
###Variable 4: Gaza
# ================================================================
pal2 <- protests5 %>% count(year,pal)
pal3 <- pal2 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total <- bind_rows(total,pal3)

# ================================================================
###Variable 5: Arrests
# ================================================================
arrest <- protests5 %>% count(year,arrests)
arrest2 <- arrest %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total <- bind_rows(total,arrest2)

# ================================================================
###Variable Group 1: Public & State
# ================================================================
group1 <- protests5 %>% count(year,public,state_politics)
group1 <- group1 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- group1

# ================================================================
###Variable Group 2: Public & Region
# ================================================================
group2 <- protests5 %>% count(year,public,region)
#no west privates, so adding this
westprivate <- tibble(public = 0, region = "west", year = 2025, n = 0)
group2 <- bind_rows(group2,westprivate)

group2 <- group2 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- bind_rows(total_group,group2)

# ================================================================
###Variable Group 3: Public & Arrests
# ================================================================
group3 <- protests5 %>% count(year,public,arrests)
group3 <- group3 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- bind_rows(total_group,group3)

# ================================================================
###Variable Group 4: Public & Gaza
# ================================================================
group4 <- protests5 %>% count(year,public,pal)
group4 <- group4 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- bind_rows(total_group,group4)

# ================================================================
###Variable Group 5: State & Gaza
# ================================================================
group5 <- protests5 %>% count(year,state_politics,pal)
group5 <- group5 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- bind_rows(total_group,group5)

# ================================================================
###Variable Group 6: Region & Gaza
# ================================================================
group6 <- protests5 %>% count(year,region,pal)
group6 <- group6 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- bind_rows(total_group,group6)

# ================================================================
###Variable Group 7: Arrests & Gaza
# ================================================================
group7 <- protests5 %>% count(year,arrests,pal)
group7 <- group7 %>%
  pivot_wider(names_from = year, values_from = n) %>%
  mutate(pct_change = (`2025` - `2024`) / `2024` * 100)
total_group <- bind_rows(total_group,group7)

######################################
#Step 4: Changing Value Names
#######################################
##Public
protests5$public <- ifelse(protests5$public == 0, "Private",
                           ifelse(protests5$public == 1,"Public",protests5$public))
total$public <- ifelse(total$public == 0, "Private",
                       ifelse(total$public == 1,"Public",total$public))
total_group$public <- ifelse(total_group$public == 0, "Private",
                             ifelse(total_group$public == 1,"Public",total_group$public))

##State
protests5$state_politics <- ifelse(protests5$state_politics == "D", "Democratic-State",
                                   ifelse(protests5$state_politics == "R","Republican-State",protests5$state_politics))
total$state_politics <- ifelse(total$state_politics == "D", "Democratic-State",
                               ifelse(total$state_politics == "R","Republican-State",total$state_politics))
total_group$state_politics <- ifelse(total_group$state_politics == "D", "Democratic-State",
                                     ifelse(total_group$state_politics == "R","Republican-State",total_group$state_politics))

##Region is fine

##Pal
protests5$pal <- ifelse(protests5$pal == FALSE, "Non-Gaza Related",
                        ifelse(protests5$pal == TRUE, "Gaza Related",protests5$pal))
total$pal <- ifelse(total$pal == FALSE, "Non-Gaza Related",
                    ifelse(total$pal == TRUE, "Gaza Related",total$pal))
total_group$pal <- ifelse(total_group$pal == FALSE, "Non-Gaza Related",
                          ifelse(total_group$pal == TRUE, "Gaza Related",total_group$pal))

##arrests
protests5$arrests <- ifelse(protests5$arrests == 0, "No Arrests",
                            ifelse(protests5$arrests == 1, "Arrests",protests5$arrests))
total$arrests <- ifelse(total$arrests == 0, "No Arrests",
                        ifelse(total$arrests == 1, "Arrests",total$arrests))
total_group$arrests <- ifelse(total_group$arrests == 0, "No Arrests",
                              ifelse(total_group$arrests == 1, "Arrests",total_group$arrests))


######################################
#Step 5: Descriptive Statistics Table
#######################################
#Note: Only doing statistics for variables used in the graph
#Making month variable for this
protests5$month.desc <- format(as.Date(protests5$date), "%m")
protests5$month.desc <- ifelse(protests5$month.desc == "08","August",
                               ifelse(protests5$month.desc == "09","September",
                                      ifelse(protests5$month.desc == "10","October",
                                             ifelse(protests5$month.desc == "11","November",
                                                    ifelse(protests5$month.desc == "12","December",NA)))))

desc <- protests5 %>%
  dplyr::select(year, month.desc, public, region, state_politics,pal,arrests) %>%
  tbl_summary(
    type = list(year ~ "categorical"),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd}, {min}, {max})",
      all_categorical() ~ "{n} ({p}%)"
    ))

protests5 <- protests5 %>% select(-month.desc)
######################################
#Step 6: Saving files
#######################################
#Re-arranging
total <- total %>% relocate(public, state_politics,region,pal,arrests)
total_group <- total_group %>% relocate(public,pal,state_politics,region,arrests)

write.csv(total,"Output/Replication_ProtestVariableChange.csv",row.names=FALSE)
write.csv(total_group,"Output/Replication_ProtestGroupVariablesChange.csv",row.names=FALSE)
write.csv(protests5,"Output/Replication_ProtestDataset.csv")
desc %>%  as_tibble() %>% write.csv("Output/DescriptiveStatistics.csv")


##########################################################################################
##########################################################################################
##########################################################################################
#Part 3: Visualization
##########################################################################################
##########################################################################################
##########################################################################################

##############################
#Loading Packages
##############################
library(tidyverse)
library(patchwork)

######################################
#Step 1: Initial Data Cleaning
#######################################
protest_df <- protests5
protest_df$month <- format(as.Date(protest_df$date), "%m")

monthyear         <- protest_df %>% count(year, month)
monthyearpal      <- protest_df %>% count(year, month, pal)
monthyearpalstate <- protest_df %>% filter(pal == "Gaza Related") %>% count(year, month, state_politics)
all <- bind_rows(monthyear, monthyearpal)
all <- all %>% relocate(year, month, pal, n)

######################################
#Step 2: Monthly Timeline
#######################################
month_levels <- c("08", "09", "10", "11", "12")

monthly <- bind_rows(
  monthyear %>%
    mutate(type_label = "All Protests"),
  monthyearpal %>%
    filter(pal == "Gaza Related") %>%
    mutate(type_label = "Gaza Related"),
  monthyearpal %>%
    filter(pal == "Non-Gaza Related") %>%
    mutate(type_label = "Non-Gaza")
) %>%
  mutate(
    month   = factor(month, levels = month_levels),
    year    = factor(year),
    line_id = factor(
      paste0(type_label, " (", year, ")"),
      levels = c(
        "All Protests (2024)",  "All Protests (2025)",
        "Gaza Related (2024)",  "Gaza Related (2025)",
        "Non-Gaza (2024)",      "Non-Gaza (2025)")))

line_colors <- c(
  "All Protests (2024)" = "#AAAAAA",
  "All Protests (2025)" = "#333333",
  "Gaza Related (2024)" = "#C4A8E0",
  "Gaza Related (2025)" = "#6A2C8E",
  "Non-Gaza (2024)"     = "#A8D4ED",
  "Non-Gaza (2025)"     = "#1565A8")

line_types <- c(
  "All Protests (2024)" = "dashed",
  "All Protests (2025)" = "solid",
  "Gaza Related (2024)" = "dashed",
  "Gaza Related (2025)" = "solid",
  "Non-Gaza (2024)"     = "dashed",
  "Non-Gaza (2025)"     = "solid")

#make monthly plot
p_monthly <- ggplot(monthly,
                    aes(x        = month,
                        y        = n,
                        colour   = line_id,
                        linetype = line_id,
                        group    = line_id)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8) +
  scale_x_discrete(labels = c("08" = "August", "09" = "September", "10" = "October",
                              "11" = "November", "12" = "December")) +
  scale_colour_manual(values = line_colors,  name = NULL) +
  scale_linetype_manual(values = line_types, name = NULL) +
  guides(
    colour   = guide_legend(nrow = 3, byrow = TRUE),
    linetype = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(
    title = "Number of Student Protests by Month \u2014 Overall, Gaza-Related, and Non-Gaza \u2014 Fall 2024 vs Fall 2025",
    x     = NULL,
    y     = "Number of Protests") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position      = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background    = element_rect(fill = "white", colour = NA),
    legend.key.width     = unit(1.8, "lines"),
    plot.title           = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    axis.text            = element_text(size = 9),
    panel.grid.minor     = element_blank())

######################################
#Step 3: Bar Graphs by Variables and Variable Groups
#######################################
var_colors <- c(
  year              = "#888888",
  pub               = "#7EB6D9",
  state_politics    = "#B39DCC",
  region            = "#85BE6E",
  pal               = "#FFB07A",
  arrests           = "#5BBFB5")

#Getting overall % change between semesters
year_totals <- monthyear %>%
  group_by(year) %>%
  summarise(n = sum(n), .groups = "drop")

n_2024 <- year_totals$n[year_totals$year == 2024]
n_2025 <- year_totals$n[year_totals$year == 2025]
year_pct <- round((n_2025 - n_2024) / n_2024 * 100, 1)

year_bar_row <- tibble(
  section    = "Overall",
  bar_label  = "All Protests",
  fill_var   = "year",
  pct_change = year_pct,
  label_text = paste0(year_pct, "% (", n_2024, " \u2192 ", n_2025, ")"))


#bar charts for individual variables
solo_raw <- total %>% mutate(region = str_to_title(region))

solo_keep <- solo_raw %>%
  mutate(
    section    = case_when(
      !is.na(public)            ~ "School Public or Private Status",
      !is.na(region)            ~ "School Region",
      !is.na(state_politics) ~ "Political Makeup of School's State",
      !is.na(pal)               ~ "Gaza-Related",
      !is.na(arrests)           ~ "Schools with Arrests over Gaza Protests"),
    bar_label  = case_when(
      !is.na(arrests) ~ dplyr::recode(arrests,
                                      "Arrests"    = "Arrests at School",
                                      "No Arrests" = "No Arrests at School"),
      TRUE            ~ coalesce(public, region, state_politics, pal)),
    fill_var   = case_when(
      !is.na(public)            ~ "pub",
      !is.na(region)            ~ "region",
      !is.na(state_politics)    ~ "state_politics",
      !is.na(pal)               ~ "pal",
      !is.na(arrests)           ~ "arrests"),
    label_text = paste0(round(pct_change, 1), "% (", `2024`, " \u2192 ", `2025`, ")")) %>%
  filter(!is.na(section)) %>%
  select(section, bar_label, fill_var, pct_change, label_text)

#bar charts for grouped variables
shorten <- function(x) {
  dplyr::recode(x,
                "Public"            = "Pub.",
                "Private"           = "Priv.",
                "Democratic-State"  = "Dem. State",
                "Republican-State"  = "Rep. State",
                "Gaza Related"      = "Gaza",
                "Non-Gaza Related"  = "Non-Gaza" )}

groups_raw <- total_group %>%
  mutate(region = str_to_title(region))

groups_keep <- groups_raw %>% mutate(
    section = case_when(
      !is.na(public) & !is.na(pal)               ~ "School Status \u00d7 Gaza-Related",
      !is.na(public) & !is.na(state_politics)    ~ "School Status \u00d7 Political Makeup",
      !is.na(public) & !is.na(arrests)           ~ "School Status \u00d7 Arrests",
      !is.na(pal)    & !is.na(state_politics)    ~ "Political Makeup \u00d7 Gaza-Related",
      !is.na(pal)    & !is.na(arrests)           ~ "Arrests \u00d7 Gaza-Related"),
    bar_label = case_when(
      !is.na(public) & !is.na(pal)               ~ paste0(shorten(pal), " x ", shorten(public)),
      !is.na(public) & !is.na(state_politics)    ~ paste0(shorten(state_politics), " x ", shorten(public)),
      !is.na(public) & !is.na(arrests)           ~ paste0(arrests, " x ", shorten(public)),
      !is.na(pal)    & !is.na(state_politics)    ~ paste0(shorten(pal), " x ", shorten(state_politics)),
      !is.na(pal)    & !is.na(arrests)           ~ paste0(arrests, " x ", shorten(pal))),
    fill_var = case_when(
      !is.na(public) & !is.na(pal)               ~ "pal",
      !is.na(public) & !is.na(state_politics)    ~ "state_politics",
      !is.na(public) & !is.na(arrests)           ~ "arrests",
      !is.na(pal)    & !is.na(state_politics)    ~ "pal",
      !is.na(pal)    & !is.na(arrests)           ~ "arrests"),
    label_text = paste0(round(pct_change, 1), "% (", `2024`, " \u2192 ", `2025`, ")")) %>%
  filter(!is.na(section)) %>%
  select(section, bar_label, fill_var, pct_change, label_text)

#making full bar graph: variable + variable groups
section_order <- c(
  "Overall",
  "School Region",
  "School Public or Private Status",
  "Political Makeup of School's State",
  "School Status \u00d7 Political Makeup",
  "Gaza-Related",
  "School Status \u00d7 Gaza-Related",
  "Political Makeup \u00d7 Gaza-Related",
  "Schools with Arrests over Gaza Protests",
  "School Status \u00d7 Arrests",
  "Arrests \u00d7 Gaza-Related")

bar_df <- bind_rows(year_bar_row, solo_keep, groups_keep) %>%
  mutate(section = factor(section, levels = section_order)) %>%
  filter(!is.na(section)) %>%
  group_by(section) %>%
  mutate(bar_label = factor(bar_label, levels = unique(bar_label))) %>%
  ungroup()

#Section display labels
section_labels <- c(
  "Overall"                                = "Total",
  "School Region"                          = "School\nRegion",
  "School Public or Private Status"        = "School\nType",
  "Political Makeup of School's State"     = "State\nPolitics",
  "School Status \u00d7 Political Makeup"  = "State Politics\nx School Type",
  "Gaza-Related"                           = "Gaza\nProtests",
  "School Status \u00d7 Gaza-Related"      = "Gaza x\nSchool Type",
  "Political Makeup \u00d7 Gaza-Related"   = "Gaza x\nState Politics",
  "Schools with Arrests over Gaza Protests"= "Gaza \nProtest\nArrests \nat School",
  "School Status \u00d7 Arrests"           = "Arrests x\nSchool Type",
  "Arrests \u00d7 Gaza-Related"            = "Arrests x\nGaza")

#plot full bar graph
p_bar <- ggplot(bar_df, aes(x = bar_label, y = pct_change, fill = fill_var)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  geom_text(
    aes(
      y     = ifelse(abs(pct_change) < 40,
                     ifelse(pct_change < 0, -2, 2),
                     ifelse(pct_change < 0, pct_change + 3, pct_change - 3)),
      hjust = ifelse(abs(pct_change) < 40,
                     ifelse(pct_change < 0, 1, 0),
                     ifelse(pct_change < 0, 0, 1)),
      label = label_text),
    size     = 2.2,
    fontface = "bold",
    colour   = "black",
    angle    = 90,
    vjust    = 0.5) +
  facet_grid(
    . ~ section,
    scales   = "free_x",
    space    = "free",
    labeller = labeller(section = as_labeller(section_labels))) +
  scale_fill_manual(values = var_colors, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "on") +
  labs(
    title    = "Percent Change in US Student Protest Count Based on College and Protest Factors \u2014 Fall 2024 v. Fall 2025",
    subtitle = "Bar labels show % change (Fall 2024 count \u2192 Fall 2025 count)",
    x        = NULL,
    y        = "% Change between Fall 2024 and Fall 2025") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5, colour = "#444444"),
    axis.text.y      = element_text(size = 9),
    axis.title.y     = element_text(size = 10, margin = margin(r = 6)),
    strip.text.x     = element_text(face = "bold", size = 7, lineheight = 0.85),
    strip.clip       = "off",                          # ADD
    strip.background = element_rect(fill = "grey90", colour = NA),
    panel.spacing    = unit(0.8, "lines"),             # CHANGED from 0.5
    plot.margin      = margin(t = 10, r = 20, b = 10, l = 10),  # ADD
    plot.title       = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, colour = "#555520", margin = margin(b = 10)))

######################################
#Step 4: Make one visual
#######################################
p_combined <- p_monthly / p_bar +
  plot_layout(heights = c(1, 1.4)) +
  plot_annotation(
    title = "US Student Protests on College Campuses \u2014 Fall 2024 vs Fall 2025",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15, margin = margin(b = 6))))

ggsave("Output/ReplicationVisual.png", p_combined, width = 11, height = 8.5, dpi = 600)

