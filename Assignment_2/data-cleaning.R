
#setting working directory
rm(list=ls())

library(tidyverse)

## Setting Path for Data in & Out ##
path <- "/Users/khawajahassan/BA21_Coding/Assignment_2/"

#location folders
data_in  <- paste0(path,"/Data/Raw/")
data_out <- paste0(path,"/Data/Clean/")

## Loading Data ##
df1<-read.csv(paste0(data_in,"Milan.csv"))

# basic data checks
sort(unique(df1$last_scraped)) # they were scraped between 2021 January 06 and 2022 January 09
sum(rowSums(is.na(df1)) == ncol(df1)) # no only NA rows
nrow(df1[duplicated(df1),]) #  no duplicates 
sum(colSums(is.na(df1)) > 0.5*nrow(df1)) # there are 3columns with at least 50% of the values missing 


df1 <- df1 %>% select(-c( "last_scraped",
                         "name",
                         "description",
                         "neighborhood_overview",
                         "picture_url",
                         "host_url",
                         "host_location",
                         "host_about",
                         "host_thumbnail_url",
                         "host_picture_url",
                         "host_total_listings_count",
                         "neighbourhood_group_cleansed",  # only NA values in column
                         "bathrooms", # only NA values in column
                         "minimum_minimum_nights", "minimum_maximum_nights", "minimum_nights_avg_ntm",
                         "maximum_minimum_nights", "maximum_maximum_nights", "maximum_nights_avg_ntm", 
                         "calendar_updated", # only NA values in column
                         "number_of_reviews_ltm", "number_of_reviews_l30d",
                         "calculated_host_listings_count_entire_homes", 
                         "calculated_host_listings_count_private_rooms", 
                         "calculated_host_listings_count_shared_rooms"))

summary(df1)
summary

#display the class and type of each columns
sapply(df1, class)
sapply(df1, typeof)


# FORMATTING COLUMNS ------------------------------------------------------

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df1[[perc]]<-as.numeric(gsub("%","",as.character(df1[[perc]])))
}

#remove dollar signs from price variables
for (i in 1:nrow(df1)){
  df1$price[i] <- as.numeric(gsub("\\$","",as.character(df1$price[i])))
}

df1$price <- as.numeric(df1$price)

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified", "has_availability", "instant_bookable")){
  df1[[binary]][df1[[binary]]=="f"] <- 0
  df1[[binary]][df1[[binary]]=="t"] <- 1
}



# AMENITIES ---------------------------------------------------------------
#amenities
df1$amenities<-gsub("\\[","",df1$amenities)
df1$amenities<-gsub("\\]","",df1$amenities)
df1$amenities<-gsub('\\"',"",df1$amenities)
df1$amenities<-as.list(strsplit(df1$amenities, ","))

#define levels and dummies 
levels(factor(unlist(df1$amenities)))

# ?do.call
# lapply(df1$amenities, factor, levs)
# lapply(lapply(df1$amenities, factor, levs), table)
# do.call(rbind, lapply(lapply(df1$amenities, factor, levs), table)))

?levels
levs <- levels(factor(unlist(df1$amenities)))
df1<-cbind(df1,as.data.frame(do.call(rbind, lapply(lapply(df1$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df1<-df1[ , !(names(df1) %in% drops)]

# create data frame of the amenities
ams <- df1 %>% select(-(1:48))

# delete spaces in the beginning and end of the column names, and transfer all to lower case
names(ams) <- gsub(" ","_", tolower(trimws(names(ams))))

# look at the column names we have
levs <- sort(names(ams))


# merge all the columns with the same column name
ams <- as.data.frame(do.call(cbind, by(t(ams), INDICES= names(ams),FUN=colSums)))

# list of key words to merge together
cat <- c( "kitchen", "stove", "oven", "frige","o_machine|ee_machine|coffee", "gril",
          "free.*on_premises", "free.*street", "paid.*on_premis|valet", "paid.*off_premises|self-parking|parking",
          "wifi|internet", "netflix|tv.*netflix", "cable", "tv", "sound_system",
          "toiletries|shampoo|conditioner|body_soap|gel", "hair_dryer", "washer|dryer", "iron",  
          "heating", "air_cond|fan", "balcony|terrace", "garden",
          "onsite_bar|restaurant", "breakfast",  "work|office", "spa",  "fitness|gym",  
          "children|baby|crib|high|corner|chang", "smoking", "housekeeping", "fireplace", "clothing_storage"
)



# function to merge columns with the same key word in them
for (i in cat) {
  tdf <- ams %>% select(matches(i))
  
  ams$new_col <- ifelse(rowSums(tdf)>0, 1, 0)
  
  names(ams)[names(ams) == "new_col"] <- paste0("have_", i)
  
  ams <- ams %>% select(-colnames(tdf)) 
  
} 



# keep only columns where the percentage of 1s is at least 1% and at most 99%
selected <- sapply(names(ams), function(x){
  ratio <- sum(ams[[x]])/nrow(ams)*100
  if (between(ratio, 1, 99)) {
    return(TRUE)
  } else { return(FALSE) }
})

amenities <- ams[,selected]

df1 <- df1 %>% select((1:48))

df1 <- cbind(df1, amenities)


#write csv
write.csv(df1,file=paste0(data_out,"Milan_cleaned.csv"))

saveRDS(df, paste0(data_out,"Milan_cleaned.rds"))



