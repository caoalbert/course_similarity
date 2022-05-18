library(readxl)
library(tidyverse)

ct <- read_excel("Desktop/141 xp project/course catalog data.xlsx")
table(ct$crs_act_typ_cd)

############################### extract class type #####################################
# allow multiple listing
# 1 = yes; 0 = no
lecture <- rep(0, nrow(ct))
discussion <- rep(0, nrow(ct))
quiz <- rep(0, nrow(ct))
conference <- rep(0, nrow(ct))
seminar <- rep(0, nrow(ct))
recitation <- rep(0, nrow(ct))
laboratory <- rep(0, nrow(ct))
studio <- rep(0, nrow(ct))
activity <- rep(0, nrow(ct))
clinic <- rep(0, nrow(ct))
field <- rep(0, nrow(ct))
tutorial <- rep(0, nrow(ct))
research <- rep(0, nrow(ct))
for (i in 1:nrow(ct)) {
  if (is.null(ct$crs_desc[i]) || is.na(ct$crs_desc[i])) {
    next
  } 
  if (str_detect(ct$crs_desc[i], "[Ll]ecture")) {
    lecture[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Dd]iscussion")) {
    discussion[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Qq]uiz")) {
    quiz[i] <- 1
  } 
  if (str_detect(ct$crs_desc[i], "[Ss]eminar")) {
    seminar[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Rr]ecitation")) {
    recitation[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Ll]aboratory")) {
    laboratory[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Ss]tudio")) {
    studio[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "Activity")) {
    activity[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Cc]linic practicum|[Cc]linic, |[Cc]linical, ")) {
    clinic[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Tt]utorial")) {
    tutorial[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Rr]esearch group meeting")) {
    research[i] <- 1
  }
  if (str_detect(ct$crs_desc[i], "[Ff]ield work")) {
    discussion[i] <- 1
  }
}
ct$lecture <- lecture
ct$discussion <- discussion
ct$quiz <- quiz
ct$seminar <- seminar
ct$recitation <- recitation
ct$laboratory <- laboratory
ct$studio <- studio
ct$activity <- activity
ct$clinic <- clinic
ct$field <- field
ct$tutorial <- tutorial
ct$research <- research

################################### extract class hours ###################################
library(english)
library(gsubfn)

hours <- rep(NA, nrow(ct))
for (i in 1:nrow(ct)) {
  matched <- str_match_all(ct$crs_desc[i], "\\b(half|one|two|three|four|five|six|seven|eight|nine|ten|\\d+) hour[s]?")
  hours_char <- matched[[1]][,2]
  additional_matched <- str_match_all(ct$crs_desc[i], "\\b(\\d+) minutes[,|:|.]?")
  minutes_char <- additional_matched[[1]][,2]
  minutes_char <- as.numeric(minutes_char)
  minutes_in_hour <- minutes_char / 60
  if (is.na(ct$crs_desc[i])) {
    hours[i] <- NA
  } else if (length(hours_char) != 0 | length(minutes_in_hour) != 0) {
    hours_num_char <- gsubfn("\\w+", setNames(as.list(1:10), as.english(1:10)), hours_char)
    hours_num_char <- str_replace_all(hours_num_char, "half", "0.5")
    hours_num <- as.numeric(hours_num_char)
    hours_total <- sum(hours_num) + sum(minutes_in_hour)
    hours[i] <- hours_total
  } else {
    hours[i] <- NA
  }
}
ct$hours <- hours

#################################### simplifing description ############################
clean <- rep(NA, nrow(ct))
ct$clean <- clean
for (i in 1:nrow(ct)) {
  ct$clean[i] <- str_replace_all(ct$crs_desc[i], ".+hour[s]?\\. ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], ".+minutes\\. ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "\\s*S/U.+grading\\.", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "\\s*Letter.+grading\\.", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "P/NP.+grading\\.", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "Requisite[s]?.+?[\\.] ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "Recommended.+?[\\.] ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "Preparation.+?[\\.] ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "Enforced.+?[\\.] ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "Course.+is enforced.+?[\\.] ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "Preparation.+?[\\.] ", "")
  ct$clean[i] <- str_replace_all(ct$clean[i], "\\.+hour[s]? \\(.+?[\\)] ", "")
}

################################# extra simplification ####################################
library(tm)
text <- ct$clean
text <- tolower(text)
text <- gsub("[[:punct:]]", " ", text)
text <- removeWords(text, words = stopwords(kind = "en"))
text <- str_replace_all(text,"[\\s]+", " ")
ct$extra_clean <- text

################################# course id #############################################
cleared_cat_num <- rep(NA, nrow(ct))
for (i in 1:nrow(ct)) {
  cleared_cat_num[i] <- str_match_all(ct$crs_catlg_no[i], "[0]+(\\S*)")[[1]][1,2]
}
cleared_cat_num
course_num <- paste(ct$subj_area_cd, cleared_cat_num, sep = " ")
ct$course_num <- course_num

############################################ save #######################################
write_csv(ct, "parsed.csv")

prereq <- read_excel("Raw Data/requisites data.xlsx")
parsedprereq[,c(1,2,7,8)]

parsed_prereq<- prereq %>% 
  mutate(crs_catlg_no = str_remove(crs_catlg_no, "^0+"),
         rqs_crs_catlg_no = str_remove(rqs_crs_catlg_no, "^0+")) %>%
  mutate(course = paste(subj_area_cd, crs_catlg_no, ""),
         req = paste(rqs_subj_area_cd,rqs_crs_catlg_no,"")) %>%
  select(course, req)

write_csv(parsed_prereq, "parsed_prereq.csv")




