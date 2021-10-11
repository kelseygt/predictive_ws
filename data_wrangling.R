# Loading packages
library(dplyr)
library(lubridate)

# Loading in data sets and left joining course data to student data
course_data <- read.csv(file.choose(), header = T)
grade_data <- read.csv(file.choose(), header = T)
student_data <- read.csv(file.choose(), header = T)

nrow(course_data)  #58348
nrow(grade_data)   #885628
nrow(student_data) #209786

combined <- merge(grade_data, course_data, by = "TERM_COURSE")
head(combined)
nrow(combined)     #885628
combined <- merge(combined, student_data, by = "PIDM_TERM")
nrow(combined)     #790925 (removes GR, Continuing Education, Inter-Institutional, and Undeclared student levels)

# Creating data frames for the total number of courses a student took, the sum of billing hours,
# the total number of withdrawals, and the sum of withdrawn billing hours.
total_courses <- combined %>%
  group_by(PIDM_TERM) %>%
  summarise(total_courses = n())
nrow(total_courses) #209589

total_hrs <- combined %>%
  group_by(PIDM_TERM) %>%
  summarise(total_hrs = sum(SFRSTCR_BILL_HR))
nrow(total_hrs)     #209589

total_w <- combined %>%
  filter(SFRSTCR_GRDE_CODE == "W") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_w = n())
nrow(total_w)       #33254

total_ch_w <- combined %>%
  filter(SFRSTCR_GRDE_CODE == "W") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_ch_w = sum(SFRSTCR_BILL_HR))
nrow(total_ch_w)    #33254

# Merging the above four new columns with the overall data set
merged <- Reduce(function(x, y) merge(x = x, y = y, by = "PIDM_TERM", all.x = TRUE),
                list(combined, total_courses, total_hrs, total_w, total_ch_w))
merged$total_w[is.na(merged$total_w)] <- 0
merged$total_ch_w[is.na(merged$total_ch_w)] <- 0
nrow(merged) # 790925

# Adding full-time/part-time status and course name
merged$time_status <- merged$total_hrs
merged$time_status[merged$total_hrs < 12] <- "Part-Time"
merged$time_status[merged$total_hrs >= 12] <- "Full-Time"
merged$course <- paste(merged$SCBCRSE_SUBJ_CODE, merged$SCBCRSE_CRSE_NUMB)

# Adding instructional method, as well as the number of courses and credit hours taken under each
merged$final_insm <-
  ifelse(merged$SSBSECT_INSM_CODE == "FTF" | (merged$SSBSECT_INSM_CODE == "A" & merged$SSBSECT_CAMP_CODE == "M"), "FTF",
       ifelse(merged$SSBSECT_INSM_CODE == "ONL" | (merged$SSBSECT_INSM_CODE == "B" & merged$SSBSECT_CAMP_CODE == "I"), "ONL", 
              ifelse(merged$SSBSECT_INSM_CODE == "HYB" | 
                       merged$SSBSECT_INSM_CODE == "HYC" |
                       merged$SSBSECT_INSM_CODE == "HYO" |
                       (merged$SSBSECT_INSM_CODE == "A" &  merged$SSBSECT_CAMP_CODE == "I") |
                       (merged$SSBSECT_INSM_CODE == "B" &  merged$SSBSECT_CAMP_CODE == "M"), "HYB", "Other")))

total_f2f <- merged %>%
  filter(final_insm == "FTF") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_f2f = n())

total_ch_f2f <- merged %>%
  filter(final_insm == "FTF") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_ch_f2f = sum(SFRSTCR_BILL_HR))

total_onl <- merged %>%
  filter(final_insm == "ONL") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_onl = n())

total_ch_onl <- merged %>%
  filter(final_insm == "ONL") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_ch_onl = sum(SFRSTCR_BILL_HR))

total_hyb <- merged %>%
  filter(final_insm == "HYB") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_hyb = n())

total_ch_hyb <- merged %>%
  filter(final_insm == "HYB") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_ch_hyb = sum(SFRSTCR_BILL_HR))

total_other <- merged %>%
  filter(final_insm == "Other") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_other = n())

total_ch_other <- merged %>%
  filter(final_insm == "Other") %>%
  group_by(PIDM_TERM) %>%
  summarise(total_ch_other = sum(SFRSTCR_BILL_HR))

insm_prop <- Reduce(function(x, y) merge(x = x, y = y, by = "PIDM_TERM", all.x = TRUE),
                    list(total_f2f, total_ch_f2f, total_onl, total_ch_onl,
                         total_hyb, total_ch_hyb, total_other, total_ch_other))
insm_prop[is.na(insm_prop)] <- 0
insm_prop$total_ch <- insm_prop$total_ch_f2f + insm_prop$total_ch_onl + insm_prop$total_ch_hyb + insm_prop$total_ch_other
head(insm_prop)
nrow(insm_prop)
head(merged)

# Merging the above eight new columns with the overall data set
merged <- Reduce(function(x, y) merge(x = x, y = y, by = "PIDM_TERM", all.x = TRUE),
                 list(merged, total_f2f, total_ch_f2f, total_onl, total_ch_onl,
                      total_hyb, total_ch_hyb, total_other, total_ch_other))
final_data$total_f2f[is.na(final_data$total_w)] <- 0
final_data$total_ch_f2f[is.na(final_data$total_ch_w)] <- 0
final_data$total_onl[is.na(final_data$total_w)] <- 0
final_data$total_ch_onl[is.na(final_data$total_ch_w)] <- 0
final_data$total_hyb[is.na(final_data$total_w)] <- 0
final_data$total_ch_hyb[is.na(final_data$total_ch_w)] <- 0
final_data$total_other[is.na(final_data$total_w)] <- 0
final_data$total_ch_other[is.na(final_data$total_ch_w)] <- 0
nrow(final_data)

# And finally, creating the complete withdrawal flag
final_data$withdraw_flag <- ifelse(final_data$total_courses == final_data$total_w, 1, 0)

# Exporting final data set
# write.csv(final_data, "C:\\Users\\Laserbeams\\Desktop\\Complete Ws\\merged.csv")