library(tidyverse)

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

df$DATE_OF_BIRTH <- NULL
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

df$AGE_2020 <- 2020 - df$YEAR_OF_BIRTH
for (i in 1:nrow(df)){
  if(df$AGE_2020 >= 18 & df$AGE_2020 <= 21){
    df$AGE_GROUP_2020 <- "18-21"
  }else if(df$AGE_2020 >= 22 & df$AGE_2020 <= 29){
    df$AGE_GROUP_2020 <- "22-29"
  }else if(df$AGE_2020 >= 30 & df$AGE_2020 <= 39){
    df$AGE_GROUP_2020 <- "30-39"
  }else if(df$AGE_2020 >= 40 & df$AGE_2020 <= 49){
    df$AGE_GROUP_2020 <- "40-49"
  }else if(df$AGE_2020 >= 50 & df$AGE_2020 <= 59){
    df$AGE_GROUP_2020 <- "50-59"
  }else if(df$AGE_2020 >= 60 & df$AGE_2020 <= 69){
    df$AGE_GROUP_2020 <- "60-69"
  }else if(df$AGE_2020 >= 70 & df$AGE_2020 <= 79){
    df$AGE_GROUP_2020 <- "70-79"
  }else if(df$AGE_2020 >= 80 & df$AGE_2020 <= 89){
    df$AGE_GROUP_2020 <- "80-89"
  }else{
    df$AGE_GROUP_2020 <- "90+"
  }
}



#reverse for loop: FILTER OUT VOTERS THAT WERE NOT ELIGIBLE TO VOTE IN 2000
for (i in nrow(df):1){
  if ((df$AGE[i] - 20) < 18){
    df <- df[-i,]
  }
}


#separate into primary and general election years
general <- select(df,
                  VOTER_STATUS,
                  YEAR_OF_BIRTH,
                  AGE_2020,
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



# model_gen <- lm(GENERAL.11.05.2019~., general)
# summary(model_gen)
# 
# 
# general[is.na(general),]






