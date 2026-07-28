# Homeroom Report 

# Purpose: Take aspen student snapshot exports and look at classroom utilization 
# in elementary schools

# Programmer: Maria Hidalgo Romero

# Date: (Original Script September 16th, 2015) Current Iteration May 5th, 2026

## Testing git changes

# Initialization - call libraries, set directories, define data set -----------

library(tidyverse)
library(googledrive)
library(googlesheets4)
library(RODBC)

today_date <- as.character(Sys.Date())
today_date <- "2026-06-22"

month <- month(Sys.Date())

if (month < 7){
  current_year <- as.character(year(Sys.Date()))
  former_year <- as.character(year(Sys.Date()) - 1)
  
} else {
  current_year <- as.character(year(Sys.Date()) + 1)
  former_year <- as.character(year(Sys.Date()))
}

schyear <- paste0("SY", substr(former_year, 3, 4), substr(current_year, 3, 4)) 

ipt_grades <- c(0, 1, 2, 7, 8)


# Custom Functions --------------------------------------------------------
# Create a function that will act as a negation of %in% 
`%nin%` = Negate(`%in%`)

# Functions that enables strings to become numerive grade values within BPS 
s2nGrade = function(stringGradeCol){
  numGradeCol = ifelse(
    grepl('K', stringGradeCol), 
    as.numeric(str_sub(stringGradeCol, 2, 2)) -2, 
    as.numeric(stringGradeCol)
  )
  
  return(numGradeCol)
  
}

n2sGrade = function(numGradeCol){
  stringGradeCol = ifelse(
    numGradeCol < 1, 
    sprintf('K%s', numGradeCol+2), 
    str_pad(numGradeCol, 2, pad = '0')
  )
  
  return(stringGradeCol)
  
}

# Remove units from value 
clean_units <- function(x){
  
  attr(x, "units") <- NULL
  class(x) <- setdiff(class(x), "units")
  x
}

# Find the mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Inputs ------------------------------------------------------------------
# Import the student snapshots from the homeroom database 
panda_homeroom <- odbcConnectAccess2007('G:/Shared drives/Planning and Analysis/1 - operations management/data/mega files/Planning & Analysis - Homeroom.accdb')
student_snapshot <- sqlQuery(
  panda_homeroom, 
  sprintf(
    "SELECT snapshot_date, studentno, studentname, gender, race, sch, grade, gradenum, homeroom, projection_group, lepstatus, mostrecenteld, sncode, firstlang, isDnr, isDnrExc, dob, sch_year
    FROM student_snapshots 
    WHERE snapshot_date = '%s'", 
    today_date
  )
) %>% 
  
  # Drop NEXT 
  
  filter(sch != 3003)

odbcClose(panda_homeroom)

# Import the homeroom class size limits alongside the structure data 
panda <- odbcConnectAccess2007('G:/Shared drives/Planning and Analysis/1 - operations management/data/mega files/Planning & Analysis.accdb')

structure <- sqlQuery(
  panda,
  sprintf(
    "SELECT 
     sch_year, 
     classyearno, 
     program1, 
     program2, 
     cap_1, 
     cap_2
   FROM structure_data
   WHERE sch_year = '%s'", 
    schyear
  )
) 

homeroom_crosswalk <- sqlQuery(
  panda, 
  'SELECT sch_year, classyearno, homeroom, type, tot_capacity FROM homeroom_crosswalk'
) %>% 
  
  filter(sch_year == schyear)

# Multicampus school table to standardize the school code variable 
split_schools <- sqlQuery(
  panda, 
  'SELECT final_year, sch, new_schname, new_sch FROM split_schools WHERE final_year IS NULL'
) %>% 
  
  # Drop the Henderson, Quincy, and the Lyon HS
  filter(!sch %in% c(4171, 4650, 4391))

school_names <- sqlQuery(panda, 'SELECT sch, schname FROM school_names')

odbcClose(panda)


hr_crosswalk <- left_join(homeroom_crosswalk, structure, by = c("sch_year", "classyearno"))

# Programcode and Descriptions per program 
supTabProgramcodes <- read.csv(
  'G:/Shared drives/Planning and Analysis/1 - operations management/data/master decodes/program codes/program codes for enrollment megafile.csv'
) 

# Create a crosswalk between homeroom type and projection_group so that we can 
# apply max classroom size rules cleanly. 

# List of programcodes where we should use the classroom limit that accounts for
# a paraprofessional by default
alfProg <- c('SL4','SF4','SA4')

# value that determines how many students need to be over-enrolled for a classroom
# to flag as over-enrolled
overEnrolledThreshold <- 1

supTabCTToPG <- supTabProgramcodes %>% 
  
  filter(
    
    # Drop AIM - no official rules 
    projection_group != 'AIM', 
    
    # Dump all inclusion but SI4, to avoid a 1:m merge 
    !(classtype == 'Inclusion' & projection_group != 'SI4')
    
  ) %>% 
  
  group_by(classtype) %>% 
  
  filter(
    
    # Filter for the first instance of each class type to avoid multiples 
    row_number() == 1
    
  ) %>% 
  
  select(classtype, projection_group) %>% 
  
  distinct() %>% 
  
  janitor::clean_names(case = 'lower_camel')

# Import classroom size limits
supTabLimits <-
  read.csv('G:/Shared drives/Planning and Analysis/1 - operations management/data/master decodes/classroom limits/current year classroom limits.csv') %>%
  
  # Merge in the pgrm variable
  
  left_join(supTabProgramcodes %>%
              select(projection_group, prgm) %>%
              distinct(),
            by = 'projection_group') %>% 
  
  janitor::clean_names(case = 'lower_camel') %>% 
  
  select(projectionGroup, grade, sizelimitNoPara, sizelimitWithPara, program2Capacity, prgm) %>% 
  
  # Reformat grade to add in leading 0's
  # Set the para limit to default for the above subseperate program where paras 
  # are consistent regardless of class size 
  mutate(
    grade = str_pad(grade, 2, pad = '0'), 
    sizelimitNoPara = ifelse(
      projectionGroup %in% alfProg, 
      sizelimitWithPara, 
      sizelimitNoPara
    )
  ) %>% 
  
  rename(
    sizelimit = sizelimitNoPara, 
    sizelimitWp = sizelimitWithPara, 
    incSizelimit = program2Capacity
  ) %>% 
  
  filter(!is.na(prgm)) 

# Special Inclusion ratios for the current year, maintained on the G Drive 
supTabInclusionRatios <- read.csv(
  'G:/Shared drives/Planning and Analysis/1 - operations management/data/master decodes/inclusion ratios/nonstandard inclusion ratios current year.csv'
)

# Clean Enrollment Data ---------------------------------------------------

# How many students without a programcode? 
no_code <- student_snapshot %>% 
  
  filter(projection_group == "" | is.na(projection_group))

no_code_num <- nrow(no_code)

print(paste("Number of rows:", no_code_num))

# Enrollment Rollup - merge in Homeroom descriptions and calculate 
# date span for special education classrooms
enrollment <- student_snapshot %>% 
  
  group_by(sch, homeroom) %>% 
  
  mutate(
    dateDob = as.POSIXct(dob, format = '%m/%d/%Y'), 
    dobDaySpan = round(
      difftime(
        max(dateDob), 
        min(dateDob), 
        units = 'days'
      ), 
      # Round to whole integer
      0 
    )
  ) %>% 
  
  ungroup() %>% 
  
  mutate(
    dobDaySpan = as.numeric(clean_units(dobDaySpan)), 
    dobMonthSpan = round(dobDaySpan/30.416666, 2), 
    sch_year = schyear
  ) %>% 
  
  select(-dateDob) %>% 
  
  rename(eld = mostrecenteld) %>% 
  
  # Remerge multicampus schools 
  left_join(split_schools, by = "sch") %>% 
  
  mutate(
    sch = case_when(
      is.na(new_sch) ~ sch,
      TRUE ~ new_sch
    )
  ) %>% 
  
  select(-new_sch, -new_schname, -final_year) %>% 
  
  # Ensure that all inclusion students are under SI4 
  mutate(
    projection_group = case_when(
      grepl("3$", projection_group) ~ "SI4", 
      TRUE ~ projection_group
    )
  ) %>% 
  
  rename(programcode = projection_group) %>% 
  
  left_join(
    supTabProgramcodes, 
    by = 'programcode'
  ) %>% 
  
  select(-prgm)


# Homeroom Identification and sub-table building --------------------------

# Every homeroom needs to be identified into a category, including homerooms with 
# multiple classtypes - aka homerooms with students in an improper programcode 
# or intentional hybrid classrooms. 

# We will identify homerooms according to the following rules:

# Rule 1): Classrooms with SEI and under 3 GenEd students are SEI classrooms 
# Rule 2): Classrooms with SLIFE students as SLIFE classrooms. 
# Rule 3): Classrooms with an Inclusion student are Inclusion classrooms if they are not already SEI 
# Rule 4): Students with majority SPED students are sub-separate classrooms 
# Rule 5): Classrooms with no inclusion students, but Gen-Ed and Subsep students (aka partial inclusion)
#          will be counted as inclusion classrooms. 

# Create a mergable programtype sum to append to the end of the script
# This table will allow us to ensure each homeroom number only has one classtype
prgmtypeCounts <- enrollment %>% 
  
  group_by(sch, homeroom, prgmtype) %>% 
  
  summarize(
    
    # Count all classes 
    total = n()
  ) %>% 
  
  mutate(prgmtype = str_to_lower(gsub('[^[:alnum:]]','',prgmtype))) %>% 
  
  # Alphabetize results 
  arrange(prgmtype) %>% 
  
  pivot_wider(
    names_from = prgmtype, 
    values_from = total, 
    values_fill = 0
  )

# Create a separate table containing maximum classtype by grade 
# for setting the homeroom values later. We're also goihng to shoehorn in a 
# calculation to determine SEI class size max. 

baseMaxRollup <- enrollment %>% 
  
  # Sum up the total number of non-SLIFE, SEI students in the class
  mutate(
    seiFlag = ifelse(grepl('BL[A-Z]', projection_group), 1, 0) 
  ) %>% 
  
  group_by(sch, homeroom, classtype, seiFlag) %>% 
  
  summarize(
    totalStudents = n()
  ) %>% 
  
  group_by(sch, homeroom) 

# Identify the largest group of students based on classtype 
classtypeMax <- baseMaxRollup %>% 
  
  filter(totalStudents == max(totalStudents)) %>% 
  
  # There are a handful of classes with duplicate max values. Luckily, most of 
  # these will have general education as one of the two max values. So it's easier 
  # just to slide these into the non-gen-ed category by dropping any rows labeled 
  # gen ed that have two 2 rows. We will keep any dups with exclusive non-gen-ed maxes
  group_by(homeroom) %>% 
  
  mutate(dupCheck = n()) %>% 
  
  filter(dupCheck == 1 | dupCheck > 1 & classtype != 'Gen Ed') %>% 
  
  # Any remaining classes with max dups are going to be weird. So I'm just going 
  # to make the class type a concatenation of any remaining values. 
  mutate(classtype = paste(classtype, collapse = ' + ')) %>% 
  
  rename(maxClasstype = classtype) %>% 
  
  select(sch, homeroom, maxClasstype) %>% 
  
  distinct()

# SEI Rollup 
seiMax <- baseMaxRollup %>% 
  
  filter(seiFlag == 1) %>% 
  
  group_by(sch, homeroom) %>% 
  
  filter(totalStudents == max(totalStudents)) %>% 
  
  # We need to do some work if there are multiple max groupings. The simplest way 
  # to solve this is to call anything with more than one classtype Multilingual SEI, 
  # because, well, the classroom by definition is multiligual. 
  mutate(
    totalGroup = n(), 
    classtype = ifelse(
      totalGroup > 1, 
      'Multiligual SEI', 
      classtype
    )
  ) %>% 
  
  distinct() %>% 
  
  select(sch, homeroom, classtype) %>% 
  
  rename(largestSEI = classtype)

# We want ELD counts as well for the final table 
eldCounts <- enrollment %>% 
  
  filter(!is.na(eld) & eld != '') %>% 
  
  group_by(sch, homeroom, eld) %>% 
  
  summarize(totalELD = n()) %>% 
  
  mutate(eld = sprintf('eld%s', eld)) %>% 
  
  # For some reason pivot_wider doesn't natively alphabetize its outpusts, so 
  # we must sort them ourselves. 
  arrange(eld) %>% 
  
  pivot_wider(
    names_from = eld, 
    values_from = totalELD, 
    values_fill = 0
  )

# Calculate classtype of each classroom. To start, we're pong ot append in the tables 
# listed above that are necessary to calculate classtype. 

# Append these totals from the rollup tables into the primary data to help calculate 
# the classtype of each classroom. 

enrollTypeCounts <- enrollment %>% 
  
  left_join(
    prgmtypeCounts, 
    by = c('sch', 'homeroom')
  ) %>% 
  
  group_by(sch, homeroom) %>% 
  
  # Create a count of all students with SN designations & a sum of all students 
  # in the class. Count the number of non-inclusion sn designations. 
  # AKA the 1 or 2 sn code level counts 
  mutate(
    snCount = sum(ifelse(!is.na(sncode) & sncode != '', 1, 0)), 
    iepLevel = as.integer(substr(sncode, 2, 2)),
    otherIEP = sum(ifelse(iepLevel == 1 | iepLevel == 2, 1, 0), na.rm = TRUE), 
    totalStudents = n(), 
    noIEP = totalStudents - snCount 
  ) %>% 
  
  # Determine what all classtype are. We're going to append in the classtypeMax 
  # table from eariler to use as a fallback option for classtype 
  left_join(
    classtypeMax, by = c('sch', 'homeroom')
  ) %>% 
  
  left_join(
    seiMax, by = c('sch', 'homeroom')
  )

# Classify the classroom type, tidy variables, and create grade values 
enrollHrIdentified <- enrollTypeCounts %>% 
  
  # Flag Inclusion classrooms AND initialize the hr_classtype variable 
  mutate(
    
    # If there is at least one inclusion kid, the homeroom is inclusive UNLESS 
    # that classroom would be tagged as sub-separate. In this case, we sort 
    # sub separate homerooms later. Check if the sum of gen-ed + sei is greater 
    # than the total number of sub separate students. 
    hrClasstype = ifelse(
      inclusion + subsep > 0 & gened+sei > subsep, 
      'Inclusion',
      ''
    ), 
    
    # If the class has more than 3 gen-ed & SEI (but no inclusion), label it as gen-ed
    hrClasstype = case_when(
      gened < 3 & sei > 0 & hrClasstype != 'Inclusion' & grepl('SEI', maxClasstype) ~ maxClasstype, 
      gened < 3 & sei > 0 & hrClasstype != 'Inclusion' & !(grepl('SLIFE', maxClasstype)) ~ 'Gen Ed', 
      TRUE ~ hrClasstype
    ), 
    
    # Final step - replace classtype with the max group classtype in all remaining cases
    hrClasstype = ifelse(
      hrClasstype == '',
      maxClasstype,
      hrClasstype
    )
  ) %>% 
  
  # Calculate the grade range for each of these classrooms 
  group_by(sch, homeroom) %>% 
  
  mutate(
    minGradenum = min(gradenum), 
    maxGradenum = max(gradenum), 
    minGrade = n2sGrade(minGradenum), 
    maxGrade = n2sGrade(maxGradenum), 
    gradeRange = ifelse(
      minGradenum == maxGradenum, 
      minGrade, 
      paste(minGrade, 'to', maxGrade, sep = " ")
    )
  ) %>% 
  
  left_join(
    eldCounts, 
    by = c('sch', 'homeroom')
  ) %>% 
  
  # Some homerooms don't have ELD levels, so we fill with 0's. 
  # Name all eld levels in the columns
  mutate_at(
    sprintf('eld%s', 1:4), ~replace_na(.,0)
  ) 

# Create the Homeroom Report ----------------------------------------------

homeroomBase <- enrollHrIdentified %>% 
  
  left_join(
    supTabCTToPG, 
    by = c('hrClasstype' = 'classtype')
  ) %>% 
  
  # Attach the BTU class size limits 
  left_join(
    supTabLimits,
    by = c(
      'projectionGroup', 
      'minGrade' = 'grade'
    )
  ) %>% 
  
  # Attach school names 
  left_join(school_names, by = "sch") %>%
  
  mutate(grade = gradeRange) %>% 
  
  rowwise() %>% 
  
  ungroup() %>% 
  
  select(
    sch, 
    schname, 
    homeroom, 
    hrClasstype, 
    grade, 
    minGradenum, 
    maxGradenum, 
    totalStudents, 
    gened, 
    inclusion, 
    subsep, 
    sei, 
    bilingual, 
    eld1:eld4, 
    dobDaySpan, 
    dobMonthSpan, 
    noIEP, 
    snCount, 
    otherIEP, 
    prgm, 
    sizelimit, 
    sizelimitWp, 
    incSizelimit
  ) %>% 
  
  arrange(sch, homeroom) %>% 
  
  distinct() 


# Programs exempt from flags 
flag_exemptions <- c(
  "Autism", 
  "Early Childhood Center-Based", 
  "Emotional Impairment", 
  "Hearing Impairment", 
  "Mild Intellectual Impairment", 
  "Moderate Intellectual Impairment", 
  "Multiple Disabilities", 
  "Severe Intellectual Impairment", 
  "Specific Learning Disability", 
  "Hatian SLIFE", 
  "Cape Verdean SLIFE", 
  "Spanish SLIFE", 
  "Multilingual SLIFE"
)

# General Classroom Types and the non-specialty programs (reg_types)
classroom_types <- c(unique(baseMaxRollup$classtype), "Homeroom", "ABA") 

reg_types <- c("Homeroom", "Gen Ed", "Inclusion")

# Merge in special inclusion ratios for the Roosevelt, Henderson, Gardener, etc 
hrIncCorrection <- homeroomBase %>% 
  
  left_join(
    supTabInclusionRatios %>% select(-schname), 
    by = c('sch', c('minGradenum' = 'gradenum'))
  ) %>% 
  
  mutate(
    
    sizelimit = ifelse(
      hrClasstype == 'Inclusion' & !is.na(inc), 
      inc + gen, 
      sizelimit
    ), 
    
    incSizelimit = ifelse(
      hrClasstype == 'Inclusion' & !is.na(inc), 
      inc, 
      incSizelimit
    )
  ) %>% 
  
  # This line needs to come out if we change the data set
  mutate(
    genedCap = ifelse(
      hrClasstype == 'Inclusion',
      sizelimit - incSizelimit, NA
    ),
    spedCap = ifelse(
      hrClasstype == 'Inclusion',
      incSizelimit, NA
    )
  ) %>%
  # Correct SLIFE and create a number of report specific variables 
  mutate(
    
    #Replace sizelimit with para for inclusion classrooms 
    sizelimit = ifelse(
      totalStudents > sizelimit & (grepl('SEI', hrClasstype) | hrClasstype == 'Dual-Language'), 
      sizelimitWp, 
      sizelimit
    ), 
    
    # SLIFE needs to be manually fixed. We'll fix this later. 
    sizelimit = ifelse(
      grepl('SLIFE', hrClasstype), 
      15, 
      sizelimit
    ), 
    
    # SPED and SLIFE classes exempt from snPct metric 
    util = round(totalStudents/sizelimit, 2), 
    seats = sizelimit - totalStudents, 
    overage = max(totalStudents - sizelimit, 0), 
    snPct = round(snCount/totalStudents, 2), 
    snPct = case_when(
      hrClasstype %in% flag_exemptions ~ NA, 
      TRUE ~ snPct
    )
  ) %>% 
  
  # Built out the BTU contract max capacity limits 
  
  # Swap out NAs with 0s for ease of math - if neither limit is present, mark the btu_limit as NA
  # SPED and SLIFE classes exempt from BTU limits metric 
  mutate(
    sizelimitWp = ifelse(is.na(sizelimitWp), 0, sizelimitWp), 
    sizelimit = ifelse(is.na(sizelimit), 0, sizelimit),
    btu_limit = case_when(
      !(hrClasstype %in% flag_exemptions) & minGradenum < 0 ~ 20,
      !(hrClasstype %in% flag_exemptions) & minGradenum < 3 ~ 22, 
      !(hrClasstype %in% flag_exemptions) & minGradenum < 6 ~ 23, 
      !(hrClasstype %in% flag_exemptions) & minGradenum < 9 ~ 25, 
      TRUE ~ NA
    )
  ) %>% 
  
  # Create a total IEP/BTU max limit and a total IEP/total enrollment 
  mutate(
    btu_max = round(snCount/btu_limit, 2) 
  ) %>% 
  
  select(
    -c(inc, gen, incSizelimit, sizelimitWp)
  ) %>% 
  
  # Drop any weird classes where the total number of assigned students is 
  # greater than 4, or there is no homeroom value, or there's a 000 in the 
  # homeroom code, or the total students is 1, or if the program is SEI or gen 
  # and the total number of students is 5 or fewer, or if the class type is SPED 
  # and there are 2 or fewer students (w/exception of Horace Mann - 4610)
  filter(
    totalStudents < 40, 
    homeroom != '', 
    !grepl('000', homeroom), 
    totalStudents != 1, 
    !(gened + sei + bilingual <= 5 & prgm %in% c('Reg Ed', 'ELL')), 
    !(totalStudents <= 2 & prgm == 'SPED' & sch != 4610)
  ) %>% 
  
  rename(capacity = sizelimit) %>% 
  
  # create an Issue Flag collumn 
  mutate(
    issueFlag = case_when(
      snPct > 0.45 & snPct < 0.50 ~ "Nearing 50% - Enrollment", 
      btu_max > 0.35 & btu_max <= 0.40 ~ "Nearing 40% - BTU Limit", 
      btu_max > 0.40 ~ "Over 40% - BTU Limit", 
      snPct >= 0.5 ~ "Over 50% - Enrollment",
      TRUE ~ NA
    ) 
  ) %>% 
  
  # Class type name  changes 
  # Autism → ABA
  # Gen Ed or Inclusion in K2 or grade 7 → Homeroom
  
  mutate(
    hrClasstype = case_when(
      grepl("Autism", hrClasstype) ~ gsub("Autism", "ABA", hrClasstype), 
      (hrClasstype == 'Gen Ed' | hrClasstype == 'Inclusion') & (minGradenum %in% ipt_grades) ~ "Homeroom", 
      TRUE ~ hrClasstype
    )
  ) %>% 
  
  # Add a "Review Homeroom" flag 
  mutate(
    homeroom_flag = case_when(
      is.na(hrClasstype) ~ "Blank Classtype", 
      (hrClasstype %in% reg_types & grepl("to", grade) & maxGradenum != -1) ~ "Needs Review - Multigraded", 
      grepl("\\+", hrClasstype) ~ "Needs Review", 
      TRUE ~ NA 
    ), 
    
    # Add an exception for the Alighieri, our Montessori school that runs multigraded classrooms
    homeroom_flag = ifelse(sch == 4321, NA, homeroom_flag)
  ) %>% 
  
  mutate(
    groups = case_when(
      hrClasstype == "Homeroom" ~ "Major", 
      hrClasstype == "Inclusion" ~ "Major", 
      hrClasstype == "Gen Ed" ~ "Major", 
      TRUE ~ hrClasstype
    )
  ) 

# Error Checking ----------------------------------------------------------

# Parse for problem homerooms (aka kids not where they should be), 
# by looking for weird or non-standard homeroom types 
error_homerooms <- hrIncCorrection %>%
  
  mutate(
    error = case_when(
      hrClasstype %in% classroom_types ~ 0,
      TRUE ~ 1 
    ), 
    
    error = case_when(
      (hrClasstype %in% reg_types) & (minGradenum != maxGradenum)  & grade != "K0 to K1" & sch != 4321 ~ 1, 
      TRUE ~ error
    )
    
  ) %>%
  
  # SLIFE environment : Due to immigration policy, our SLIFE numbers plummeted in sch_year 2025-2026 
  # This has meant more than ever multingual SLIFE classrooms regardless of the students, 
  # language specific programcode. We will be avoiding marking these classrooms as Errors 
  # AKA look for two SLIFE words
  mutate(
    error = case_when(
      grepl("SLIFE.*SLIFE", hrClasstype) ~ 0, 
      TRUE ~ error
    )
  ) %>% 
  
  
  filter(error == 1) 


# For all of our error homerooms, find the dominant classtype within the homeroom data 
maxClassTypeError <- enrollment %>% 
  
  filter(homeroom %in% error_homerooms$homeroom) %>% 
  
  # Sum up the total number of non-SLIFE, SEI students in the class 
  # If the class 
  mutate(
    seiFlag = ifelse(grepl('BL[A-Z]', projection_group), 1, 0) 
  ) %>% 
  
  group_by(sch, homeroom, classtype, seiFlag) %>% 
  
  summarize(
    totalStudents = n()
  ) %>%
  
  group_by(homeroom) %>% 
  
  filter(totalStudents == max(totalStudents))  %>% 
  
  select(-sch) 

# For REG type classrooms, find the most prevalent grade 
grade <- enrollment %>% 
  
  filter(homeroom %in% error_homerooms$homeroom) %>% 
  
  group_by(homeroom) %>% 
  
  summarize(
    mode_grade = mode(grade)
  )

maxClassTypeError <- left_join(maxClassTypeError, grade, by = "homeroom")

# Merge the MaxClassTypeError table with the student level enrollment data and 
# flag any student that does not match with the dominant class type and flag any 
# students that do not match with the rest of the class's grade reg_type classes 
student_flag <- enrollment %>% 
  
  filter(homeroom %in% error_homerooms$homeroom) %>% 
  
  left_join(school_names, by = "sch") %>% 
  
  select(
    sch,
    studentno, 
    schname, 
    studentname, 
    grade, 
    homeroom, 
    programcode, 
    lepstatus, 
    eld, 
    sncode, 
    classtype
  ) %>% 
  
  rename(prgmtype = classtype) %>% 
  
  left_join(maxClassTypeError, by = "homeroom") %>% 
  
  arrange(homeroom) %>% 
  
  mutate(
    error_flag = case_when(
      classtype == prgmtype ~ NA, 
      classtype == "Gen Ed" & prgmtype == "Inclusion" ~ "NA", 
      TRUE ~ "Program or School Error"
    ), 
    
    error_flag = case_when(
      (classtype %in% reg_types) & (grade != mode_grade) & is.na(error_flag) ~ "Check Grade", 
      TRUE ~ error_flag
    ), 
    
    error_flag = ifelse(sch == 4321, NA, error_flag)
  )%>% 
  
  filter(error_flag == "Program or School Error" | error_flag == "Check Grade") %>% 
  
  select(
    sch, 
    schname, 
    studentno, 
    error_flag, 
    studentname, 
    grade, 
    homeroom, 
    programcode, 
    lepstatus, 
    eld, 
    sncode
  )

# Preparing Reports -------------------------------------------------------

# Prepare a file with all of the student information 
allStudentsWriteFile <- enrollment %>% 
  
  arrange(sch, homeroom, programcode, gradenum) %>% 
  
  left_join(school_names, by = "sch") %>% 
  
  select(
    sch, 
    schname, 
    homeroom, 
    studentno, 
    studentname, 
    grade,
    programcode, 
    lepstatus, 
    eld, 
    firstlang, 
    sncode
  )

# Write Classroom capacity data for the SPED Assignment Dir/Asst Dir in OSS 
classroom_data <- hrIncCorrection %>% 
  
  ungroup() %>% 
  
  arrange(schname, minGradenum, maxGradenum, homeroom) %>% 
  
  select(sch, schname, grade, minGradenum, maxGradenum, hrClasstype, homeroom,
         capacity, totalStudents, seats, gened:bilingual, eld1:eld4, genedCap, spedCap,
         snCount, snPct, dobDaySpan, dobMonthSpan)

# Merge in the proper capacity and program data
homeroom_report <- hrIncCorrection %>% 
  
  select(-capacity, - seats) %>% 
  
  left_join(
    hr_crosswalk, by = c("homeroom")
  ) %>% 
  
  mutate(
    seats = tot_capacity - totalStudents
  ) 

#### SAVEPOINT 

# High level classroom summary 
hrSummary_district <- homeroom_report %>% 
  
  arrange(sch, schname, groups, minGradenum) %>% 
  
  ungroup() %>% 
  
  select(
    sch, 
    schname, 
    grade, 
    hrClasstype, 
    homeroom, 
    homeroom_flag, 
    issueFlag,
    tot_capacity,
    totalStudents, 
    seats,
    noIEP,
    inclusion, 
    subsep, 
    otherIEP,
    snCount, 
    btu_max, 
    btu_limit, 
    snPct,
    dobMonthSpan, 
    eld1:eld4, 
  ) %>% 
  
  rename(
    `School Code` = sch, 
    `School Name` = schname, 
    `Grade` = grade,
    `Class Type` = hrClasstype,
    `Homeroom` = homeroom, 
    `Homeroom Flag` = homeroom_flag, 
    `Issue Flag` = issueFlag, 
    `Capacity` = tot_capacity, 
    `Total Students` = totalStudents, 
    `Available Seats` = seats, 
    `No IEP` = noIEP, 
    `Inclusion` = inclusion, 
    `Sub Seperate` = subsep, 
    `Other IEPs` = otherIEP, 
    `Total IEPs` = snCount, 
    `Pct of BTU Max` = btu_max, 
    `BTU Limit` = btu_limit, 
    `Pct of Enrollment` = snPct
  ) %>% 
  
  mutate(
    report_updated = today_date
  ) %>% 
  
  relocate(report_updated)


hrSummary <- hrSummary_district %>% 
  
  select(-eld1, -eld2, -eld3, -eld4) %>% 
  
  mutate(
    `Class Type` = case_when(
      `Grade` == "K0 to K1" ~ "Homeroom", 
      TRUE ~ `Class Type`
    )
  ) 

# Write to Google Sheets --------------------------------------------------

# Write to google sheets 
# Please note that, in order to run these updates, you'll need to authorize
# tidyverse to interface with Google Sheets. To do this, you'll be prompted to
# install a package called httpuv to authorize tidyverse with your Google drive.
# Type 1 in the console and you will be taken to a page to authorize tidyverse to access it. 
# Once this is done you should be able to
# write to this report, since it's located in the Planning & Analysis shared
# drive.

gs4_auth()
hrSS <- '1LPawUAurBJttP6FWvU9FQkc1BkHrK0GqtLP7uxZcCSA'

write_sheet(
  allStudentsWriteFile,
  ss=hrSS,
  sheet = 'StudentList'
)

write_sheet(
  hrSummary_district,
  ss=hrSS,
  sheet = 'HRReport'
)

write_sheet(
  student_flag,
  ss=hrSS,
  sheet = 'Potential Student Errors'
)

write_sheet(
  classroom_data, 
  ss = hrSS, 
  sheet = "OSS Data"
)

write.csv(
  hrSummary_district, 
  sprintf(
    "G:/Shared drives/Planning and Analysis/1 - operations management/standalone tools/homeroom report/output/Homeroom Report - %s", 
    today_date
  )
)

# School Level Report
sch_list <- as.character(unique(allStudentsWriteFile$sch))

# Delete any students from the Mel-King 
sch_list <- sch_list[sch_list != "1290"]
# sch_list <- sch_list[sch_list != "2450"]
# sch_list <- sch_list[sch_list != "4160"]

hr_data <- split(hrSummary, hrSummary$`School Code`)
student_data <- split(allStudentsWriteFile, allStudentsWriteFile$sch)
error_data <- split(student_flag, student_flag$sch)

# # Testing 
# sch <- sch_list[1]

# #We need to figure out whether this is a historical
# #year or the most recent, because it will impact the 
# #directory (historical directories have ARCHIVE tagged
# #to the end) This will need to be done by the actually current month of the year
# #as well as the year.
# current_month <- as.numeric(format(Sys.Date(), "%m"))
# current_year <- as.numeric(format(Sys.Date(), "%y"))
# 
# if(current_month>10) {
#   current_sch_year <- paste("SY",current_year+1,current_year+2,sep="")
# } else {
#   current_sch_year <- paste("SY",(current_year),current_year+1,sep="")
# }
# 
# gdrive <- "G:/.shortcut-targets-by-id/0B57hJdy8FCR1VmNXbTNVQ2xLVnM/Planning and Analysis Reports"

# School Folders-Planning & Analysis 
folder_id <- "https://drive.google.com/drive/folders/0B57hJdy8FCR1VmNXbTNVQ2xLVnM"

# School Folders Directory 
directory <- drive_ls(folder_id)

# pull in school names and add school codes to the directory 
panda <- odbcConnectAccess2007('data/mega files/Planning & Analysis.accdb')
names <- sqlQuery(panda, 'SELECT* FROM school_names')

directory$new_schname <- substring(directory$name, 33, 70)

directory <- left_join(directory, names, by = "new_schname")

# Create the Reports and Sort them into the appropriate folders 
# Logic - create template - sort template into the right folder - write data 

school_file_directory <- data.frame(
  sch = character(length = length(sch_list)), 
  ss = character(length = length(sch_list)) 
)

i <- 1

for(sch in sch_list){
  
  subfolder_directory <- directory$id[directory$sch == sch]
  subfolder_id <- subfolder_directory[which(!is.na(subfolder_directory))]
  
  subsubfolder_directory <- drive_ls(subfolder_id)
  
  subsubfolder_id <- subsubfolder_directory$id[subsubfolder_directory$name == "School Year 25-26 Reports ARCHIVE"]
  
  report_name <- paste0(sch, " - SY2526 Homeroom Report")
  
  ss <- gs4_create(
    report_name, 
    sheets = c("Homeroom Report", "Student List", "Potential Student Errors")
  )
  
  drive_mv(
    ss, 
    path = as_id(subsubfolder_id), 
    overwrite = TRUE 
  )
  
  sheet_write(
    hr_data[[sch]], 
    ss = ss, 
    sheet = "Homeroom Report"
  )
  
  sheet_write(
    student_data[[sch]], 
    ss = ss, 
    sheet = "Student List"
  )
  
  # Write "Potential Student Errors" sheet only if error_data[[sch]] is not NULL
  if (!is.null(error_data[[sch]])) {
    sheet_write(
      error_data[[sch]], 
      ss = ss, 
      sheet = "Potential Student Errors"
    )
  }
  
  school_file_directory$sch[i] <- sch 
  school_file_directory$ss[i] <- ss
  
  i <- i + 1 
}

write_sheet(
  school_file_directory,
  ss=hrSS,
  sheet = 'School File Directory'
)












