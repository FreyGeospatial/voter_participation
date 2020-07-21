library(tidyverse)
library(eeptools)


df <- read.csv("Data/ADAMS.txt")
df$LAST_NAME <- NULL
df$FIRST_NAME <- NULL
df$MIDDLE_NAME <- NULL
df$COUNTY_ID <- NULL
df$SUFFIX <- NULL
df$PRIMARY.09.10.2019 <- NULL #all NA
df$PRIMARY.09.13.2016 <- NULL
df$WARD <- NULL
df$RESIDENTIAL_POSTALCODE <- NULL
df$MAILING_COUNTRY <- NULL
df$CAREER_CENTER <- NULL
df$MUNICIPAL_COURT_DISTRICT <- NULL
df$CITY <- NULL
df$CITY_SCHOOL_DISTRICT <- NULL
df$RESIDENTIAL_COUNTRY <- NULL
df$COUNTY_COURT_DISTRICT <- NULL
df$PRIMARY.10.14.2008 <- NULL
df$PRIMARY.05.03.2005 <- NULL
df$PRIMARY.09.13.2005 <- NULL
df$PRIMARY.09.10.2013 <- NULL
df$PRIMARY.10.01.2013 <- NULL
df$LIBRARY <- NULL
df$EXEMPTED_VILL_SCHOOL_DISTRICT <- NULL
df$PRIMARY.05.08.2007 <- NULL
df$GENERAL.11.18.2008 <- NULL
df$PRIMARY.09.15.2009 <- NULL
df$PRIMARY.09.29.2009 <- NULL
df$PRIMARY.09.11.2007 <- NULL
df$PRIMARY.09.15.2015 <- NULL
df$GENERAL.06.07.2016 <- NULL
df$PRIMARY.09.12.2017 <- NULL
df$PRIMARY.09.13.2011 <- NULL


for(j in 31:ncol(df)){
  for(i in 1:nrow(df)){
    if (df[i,j] != ""){
      df[i,j] <- 1
    }else{
      df[i,j] <- 0
    }
  }
}


df$YEAR_OF_BIRTH <- df$DATE_OF_BIRTH %>% lubridate::year()
#df$AGE <- 2020 - df$AGE

# df$DATE_OF_BIRTH <- NULL
df$RESIDENTIAL_ADDRESS1 <- NULL
df$RESIDENTIAL_SECONDARY_ADDR <- NULL
df$RESIDENTIAL_ZIP_PLUS4 <- NULL
df$MAILING_ADDRESS1 <- NULL
df$MAILING_CITY <- NULL
df$MAILING_SECONDARY_ADDRESS <- NULL
df$MAILING_ZIP <- NULL
df$MAILING_POSTAL_CODE <- NULL
df$MAILING_STATE <- NULL
df$MAILING_ZIP_PLUS4 <- NULL
df$REGISTRATION_DATE <- NULL

df$VOTER_STATUS <- as.factor(df$VOTER_STATUS)
df$PARTY_AFFILIATION <- as.factor(df$PARTY_AFFILIATION)
df$PRECINCT_NAME <- as.factor(df$PRECINCT_NAME)
df$PRECINCT_CODE <- NULL
df$RESIDENTIAL_STATE <- as.factor(df$RESIDENTIAL_STATE)
df$RESIDENTIAL_CITY <- as.factor(df$RESIDENTIAL_CITY)
df$RESIDENTIAL_ZIP <- as.factor(df$RESIDENTIAL_ZIP)
df$EDU_SERVICE_CENTER_DISTRICT <- as.factor(df$EDU_SERVICE_CENTER_DISTRICT)
df$LOCAL_SCHOOL_DISTRICT <- as.factor(df$LOCAL_SCHOOL_DISTRICT)
df$TOWNSHIP <- as.factor(df$TOWNSHIP)
df$VILLAGE <- NULL

for(i in 17:length(df)){
  df[,i] <- as.integer(df[,i])
}

df <- na.omit(df)

df$AGE_2019 <- 2019 - df$YEAR_OF_BIRTH
df$AGE_GROUP_2019 <- NA
for (i in 1:nrow(df)){
  if(df$AGE_2019[i] >= 18 & df$AGE_2019[i] <= 21){
    df$AGE_GROUP_2019[i] <- "18-21"
  }else if(df$AGE_2019[i] >= 22 & df$AGE_2019[i] <= 29){
    df$AGE_GROUP_2019[i] <- "22-29"
  }else if(df$AGE_2019[i] >= 30 & df$AGE_2019[i] <= 39){
    df$AGE_GROUP_2019[i] <- "30-39"
  }else if(df$AGE_2019[i] >= 40 & df$AGE_2019[i] <= 49){
    df$AGE_GROUP_2019[i] <- "40-49"
  }else if(df$AGE_2019[i] >= 50 & df$AGE_2019[i] <= 59){
    df$AGE_GROUP_2019[i] <- "50-59"
  }else if(df$AGE_2019[i] >= 60 & df$AGE_2019[i] <= 69){
    df$AGE_GROUP_2019[i] <- "60-69"
  }else if(df$AGE_2019[i] >= 70 & df$AGE_2019[i] <= 79){
    df$AGE_GROUP_2019[i] <- "70-79"
  }else if(df$AGE_2019[i] >= 80 & df$AGE_2019[i] <= 89){
    df$AGE_GROUP_2019[i] <- "80-89"
  }else{
    df$AGE_GROUP_2019[i] <- "90+"
  }
}

general <- select(df,
                  VOTER_STATUS,
                  DATE_OF_BIRTH,
                  YEAR_OF_BIRTH,
                  AGE_2019,
                  AGE_GROUP_2019,
                  PARTY_AFFILIATION,
                  RESIDENTIAL_CITY,
                  #RESIDENTIAL_ZIP,
                  #CONGRESSIONAL_DISTRICT,
                  #COURT_OF_APPEALS,
                  EDU_SERVICE_CENTER_DISTRICT,
                  #LOCAL_SCHOOL_DISTRICT,
                  PRECINCT_NAME,
                  #STATE_BOARD_OF_EDUCATION,
                  #STATE_REPRESENTATIVE_DISTRICT,
                  #STATE_SENATE_DISTRICT,
                  #TOWNSHIP,
                  contains("GENERAL"))

general$Last4 <- (general$GENERAL.11.06.2018 + general$GENERAL.11.07.2017 +
               general$GENERAL.11.08.2016 + general$GENERAL.11.03.2015)

general$AGE_GROUP_2019 <- as.factor(general$AGE_GROUP_2019)

general$DATE_OF_BIRTH <- as.Date(general$DATE_OF_BIRTH, "%Y-%m-%d")

general$AGE_2015_GENERAL <- NA

for (i in 1:nrow(general)){
  if (age_calc(dob = general$DATE_OF_BIRTH[i], 
               enddate = as.Date("2017-11-03", "%Y-%m-%d"),
               units = "years") >= 18){
    general$AGE_2015_GENERAL[i] <- "Eligible"
  }else{
    general$AGE_2015_GENERAL[i] <- "Not Eligible"
  }
}

general <- general %>% 
  filter(AGE_2015_GENERAL == "Eligible")

#change this
voter_turnout <- general %>% group_by(AGE_GROUP_2019) %>% summarise(turnout = mean(Last4))



my_model <- glm(general$GENERAL.11.05.2019 ~ 
      general$Last4 + 
      general$AGE_2019 + 
      general$PARTY_AFFILIATION + 
      general$RESIDENTIAL_CITY)

pred <- predict(my_model, type = "response")

general$pred_2020 <- pred


# #reverse for loop: FILTER OUT VOTERS THAT WERE NOT ELIGIBLE TO VOTE IN 2017
# for (i in nrow(df):1){
#   if ((df$AGE[i] - 3) < 18){
#     df <- df[-i,]
#   }
# }


# turnout percentage for each age group












# #reverse for loop: FILTER OUT VOTERS THAT WERE NOT ELIGIBLE TO VOTE IN 2000
# for (i in nrow(df):1){
#   if ((df$AGE[i] - 20) < 18){
#     df <- df[-i,]
#   }
# }


#separate into primary and general election years




# model_gen <- lm(GENERAL.11.05.2019~., general)
# summary(model_gen)
# 
# 
# general[is.na(general),]






