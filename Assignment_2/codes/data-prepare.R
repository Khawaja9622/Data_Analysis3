# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)
library(ggcorrplot)

## Setting Path for Data in & Out ##
path <- "/Users/khawajahassan/BA21_Coding/Assignment_2/"

# set data dir, load theme and functions
source(paste0(path, "da_helper_functions.R"))
source(paste0(path, "theme_bg.R"))

data_in <- paste0(path,"Data/Clean/")
data_out <- paste0(path,"Data/Clean/")
output <- paste0(path,"Output/")

#-------------------------------------------------------
# Import data
df <- read.csv(paste0(data_in,"Milan_cleaned.csv"))

# FILTER DATA TO ACTUAL CASE ----------------------------------------------

# check for different property types
types <- df %>% group_by(property_type) %>% 
  summarise(number = n()) %>% 
  arrange(.,-number)


rm(types)

# Entire loft
# Entire serviced apartment
# Entire home/apt
# condo

# keep if property type is Apartment( including condo under apartment category only/ later will create flag variable to check )
df <-df %>% filter(property_type %in% c("Entire loft","Entire serviced apartment","Entire home/apt","Entire condominium (condo)"))
  

# keep if accommodates 2-6 people
df <- df[df$accommodates >= 2 & df$accommodates <= 6,]


# CLEANE VARIABLES AND CREATE WORKFILE ------------------------------------------------
#################
#### FACTORS
#################

# Property type as factor
df %>% 
  group_by(property_type) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)


df$property_type <- word(df$property_type, -1) 

df <- df %>% 
  mutate( f_property_type = factor(property_type))

# removing bracket from condo variable 

df[] <- lapply(df, function(x) gsub("[][(),]", "", x))
df$f_property_type



# only Entire home/apt/dropping
unique(df$room_type)
df$room_type <- NULL

# neighbourhood_cleansed as factors
unique(df$neighbourhood_cleansed)
df <- df %>%
  mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed, levels = c("TICINESE","LAMBRATE",                   "NAVIGLI"   ,                   "PARCO FORLANINI - ORTICA",     "BOVISA"   ,                   
                                                                              "LORETO" ,                      "MAGENTA - S. VITTORE" ,        "S. CRISTOFORO" ,               "GRECO"    ,                    "FARINI"  ,                    
                                                                              "PORTA ROMANA"   ,              "VILLAPIZZONE"    ,             "GIAMBELLINO"   ,               "MACIACHINI - MAGGIOLINA" ,     "CENTRALE"   ,                 
                                                                              "MAGGIORE - MUSOCCO" ,          "BRERA" ,                       "BUENOS AIRES - VENEZIA",       "BARONA"   ,                    "TIBALDI"  ,                   
                                                                              "RONCHETTO SUL NAVIGLIO" ,      "ISOLA",                        "GHISOLFA",                     "SARPI"  ,                      "CITTA' STUDI"  ,              
                                                                              "WASHINGTON" ,                  "VIALE MONZA"  ,                "DUOMO"    ,                    "SCALO ROMANA" ,                "VIGENTINA"   ,                
                                                                              "XXII MARZO" ,                  "DERGANO" ,                     "EX OM - MORIVIONE",            "GRATOSOGLIO - TICINELLO" ,     "PARCO SEMPIONE" ,             
                                                                              "GUASTALLA" ,                   "DE ANGELI - MONTE ROSA" ,      "PAGANO" ,                      "PADOVA" ,                      "GARIBALDI REPUBBLICA" ,       
                                                                              "TORTONA"  ,                    "TRIULZO SUPERIORE" ,           "CORSICA" ,                     "ADRIANO" ,                     "BANDE NERE" ,                 
                                                                              "BOVISASCA" ,                   "BAGGIO"  ,                     "STADERA"  ,                    "PARCO LAMBRO - CIMIANO" ,      "NIGUARDA - CA' GRANDA" ,      
                                                                              "CANTALUPA" ,                   "S. SIRO"   ,                   "UMBRIA - MOLISE" ,             "PORTELLO"   ,                  "LORENTEGGIO"  ,               
                                                                              "RIPAMONTI" ,                   "MECENATE"  ,                   "LODI - CORVETTO"  ,            "SELINUNTE" ,                   "PARCO MONLUE' - PONTE LAMBRO",
                                                                              "AFFORI" ,                      "BRUZZANO"  ) ) )  

#790 NA values is it a viable option to consider this as a predictor 
# get host_response_time as factors
unique(df$host_response_time)
df <- df %>% 
  mutate(f_host_response_time = factor(host_response_time, levels = c( "within an hour",  "within a few hours",
                                                                       "within a day", "a few days or more")))
df %>% 
  group_by(f_host_response_time) %>% 
  summarise(cnt = n())


################################
#### NUMERIC VARIABLES
################################
## Create Numerical variables
df <- df %>%
  mutate( p_host_response_rate = as.numeric(host_response_rate),
          p_host_acceptance_rate = as.numeric(host_acceptance_rate))

#NA 777
df %>% 
  group_by(p_host_acceptance_rate) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

#NA 790
df %>% 
  group_by(p_host_response_rate) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

df$p_host_acceptance_rate
df$p_host_response_rate

### clean number of bathrooms#####

df <- df %>% rename(bathrooms = bathrooms_text)
# get the number of baths from bathroom_text
df$bathrooms <- as.numeric(gsub("[^0-9.-]", "", gsub("half", 0.5, df$bathrooms, ignore.case = T)))
unique(df$bathrooms)

df %>% 
  group_by(bathrooms) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

# dropping air-bnb where there is no washroom 
df <- subset(df, bathrooms!=0)


################################################
# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms", "bedrooms", "beds", "review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights", "availability_365")
df <-  df %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- df %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(df))
colnames(df)[nnames_i] <- paste0("n_", numericals)


######create days since first review######3
df <- df %>%
  mutate(
    n_days_sincefirst = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))



df %>% 
  group_by(n_days_sincefirst) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

#######create days since last review##########
df <- df %>%
  mutate(
    n_days_sincelast = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                     as.Date(last_review ,format="%Y-%m-%d")))

df %>% 
  group_by(n_days_sincelast) %>% 
  summarise(cnt = n()) %>% 
  arrange(- cnt)

###############################
#### DUMMY VARIABLES###########
###############################


# create dummy var
dummies <- c(names(df)[seq(50,123)],"host_is_superhost", "host_identity_verified" ) 
df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))



# CREATE WORK FILE --------------------------------------------------------

# keep columns if contain d_, n_, f_, p_, usd_ and some others
df <- df %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,property_type)


a <- df %>% 
  group_by(price) %>% 
  summarise(cnt = n()) %>% 
  arrange( cnt)
rm(a)

# 13 values were dropped with no price 
# with price info only
df <- df %>%
  drop_na(price)



write_csv(df, paste0(data_out, "airbnb_milan_workfile.csv"))
saveRDS(df, paste0(data_out, "airbnb_milan_workfile.rds"))

library(skimr)
library(modelsummary)


# CLEANING VALUES -------------------------------------------------------------------------

##################################
# DESCRIBE

# Property type (58 missing)
df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

describe(df$f_neighbourhood_cleansed)




#####################
### look at price ###
#####################
df$price <- as.numeric(df$price)

summary(df$price)
describe(df$price)
datasummary_skim(df)


# 0.95 percent of the observation has prices below 294 so dropping 94 observation which had extreme values  
# filter out really high extreme values 

 df %>% filter(price> 300) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

 df <- df %>% filter(price< 300)
 
 df$price<- as.numeric(as.character(df$price))
 
 # Price Distribution
 
 price_hist <- ggplot(df, aes( x = price)) +
   geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan3", color = "black") +
   theme_bw() +
   scale_y_continuous(labels = label_percent()) +
   ylab("Percent") + 
   xlab("Price (Euros)")
 price_hist
 
 ln_price_hist <- ggplot(df, aes(x = log(price))) +
   geom_histogram(aes(y = (..count..)/sum(..count..)),fill = "cyan3", color = "black") +
   theme_bw() +
   scale_y_continuous(labels = label_percent()) +
   ylab("Percent") + 
   xlab("ln(Price, Euros)")
 ln_price_hist

 
 
 price_hist_grid <- ggarrange(
   price_hist,
   ln_price_hist,
   nrow = 1)
 

 annotate_figure(price_hist_grid,bottom = 
                   text_grob("Note: Apartments with 2-6 accommodation capacity. Histogram without extreme values (price < 300 Euros)"))
                                                                                                                                                       
 
 ###############################
 # Handling missing values #
 ###############################
 

 describe(df$n_accommodates)
 describe(df$n_beds)
 # where do we have missing values now?
 to_filter <- sapply(df, function(x) sum(is.na(x)))
 to_filter[to_filter > 0]
 
 # f_neighbourhood_cleansed     f_host_response_time     p_host_response_rate   p_host_acceptance_rate               n_bedrooms 
 # 57                      730                      730                      698                      245 
 # n_beds   n_review_scores_rating      n_reviews_per_month        n_days_sincefirst         n_days_sincelast 
 # 176                      703                      703                      703                      703 
 
 
 # Impute the columns 
  df <-  df %>%
   mutate(
     flag_f_neighbourhood_cleansed=ifelse(is.na(f_neighbourhood_cleansed),1,0),
     flag_days_sincelast=ifelse(is.na(n_days_sincelast),1, 0),
     n_days_sincelast =  ifelse(is.na(n_days_sincelast), median(n_days_sincelast, na.rm = T), n_days_sincelast),
     flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
     n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
     n_beds = ifelse(is.na(n_beds), round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
     n_beds = ifelse(n_beds == 0, round(n_accommodates / 1.5), n_beds), #assume that 1 bed corresponds to about 1.5 accommodates
     n_bedrooms = ifelse(is.na(n_bedrooms), n_accommodates %% 2, n_bedrooms),
     flag_n_bedroom= ifelse(n_bedrooms == 0,1,0 )#assume that bedrooms correlate to around half the number of accommodates
   )
  
 ## adding term missing value for neighbourhood ##  
df$f_neighbourhood_cleansed <- as.character(df$f_neighbourhood_cleansed)
df$f_neighbourhood_cleansed[is.na(df$f_neighbourhood_cleansed)] <- "Missing Value"
df$f_neighbourhood_cleansed <- factor(df$f_neighbourhood_cleansed)


 # 2. drop columns when many missing not important
 
 df$f_host_response_time <- NULL
 df$p_host_acceptance_rate <- NULL
 df$p_host_response_rate <- NULL
 df$n_days_sincefirst <- NULL
 df$ n_reviews_per_month <- NULL

 ################################################
 # look at some key variable &  functional form #
 ################################################
 
 
 ## n_accomodates:
 
 df%>%
   group_by(n_accommodates) %>%
   summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())
 
 price_vs_accommodates <- ggplot(data = df, aes(x=n_accommodates, y=price)) +
   geom_point(size=1, colour=color[3], shape=16)+
   ylim(0,310)+
   xlim(0,7)+
   labs(x="Number of people accomodated",y="Price")+
   geom_smooth(method="lm", colour=color[1], se=FALSE)+
   theme_bg()
 price_vs_accommodates
 
 
 
 ############################
 ## n_bathrooms
 ############################
 
 
 ggplot(df, aes(n_bathrooms)) +
   geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8) +
   xlab("N of bathrooms") +
   theme_bw()
 
 df %>%
   group_by(n_bathrooms) %>%
   summarise(mean_price = mean(price), n = n())
 
 # check number of bathrooms for different number of accommodates
 df %>% 
   group_by(n_accommodates) %>% 
   summarise(num_baths = mean(n_bathrooms, na.rm = T), min_baths = min(n_bathrooms, na.rm = T), max_baths = max(n_bathrooms, na.rm = T))
 
 # Pool accommodations with 0,1,2,4 bathrooms
 df <-  df %>%
   mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,6), labels=c(1,2,3), right = F) )
unique(df$f_bathroom)

df %>% 
    group_by(f_bathroom) %>% 
   summarise(num_values=n()) %>% 
    arrange(-num_values)

############################
#### n_beds
###########################

ggplot(df, aes(n_beds)) +
  geom_histogram( fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of beds") +
  theme_classic()

df %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of beds for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_beds = mean(n_beds, na.rm = T), min_beds = min(n_beds, na.rm = T), max_beds = max(n_beds, na.rm = T))


#Pool accomomdations with 1,2,3,9 beds
df <- df %>%
  mutate(f_beds = cut(n_beds, c(1,2,3,9), labels=c(1,2,3), right = F) )

df %>% 
  group_by(f_beds) %>% 
  summarise(num_values=n()) %>% 
  arrange(-num_values)

############################
## n_bedrooms
############################

ggplot(df, aes(n_bedrooms)) +
  geom_histogram(binwidth = 0.5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  xlab("N of bedrooms") +
  theme_classic()

df %>%
  group_by(n_bedrooms) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

# check number of bedrooms for different number of accommodates
df %>% 
  group_by(n_accommodates) %>% 
  summarise(num_bedrooms = mean(n_bedrooms, na.rm = T), min_bedrooms = min(n_bedrooms, na.rm = T), max_bedrooms = max(n_beds, na.rm = T))

describe(df$n_bedrooms)

#Pool accomomdations with 1,2,3,9 beds
df <- df %>%
  mutate(f_bedrooms = cut(n_bedrooms, c(0,1,4,6), labels=c(1,2,3), right = F) )


df %>% 
  group_by(f_bedrooms) %>% 
  summarise(cnt=n())


############################
## n_review_scores_rating(BC KIYA KARNA HAE ISKA)
ggplot(data = df, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour="cyan4", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_classic()

describe(df$n_review_scores_rating)

df %>% 
  group_by(n_review_scores_rating) %>% 
  summarise(cnt=n()) %>% 
  arrange(-cnt)


# (0-1/2-4/4-5.5)
df <- df %>%
  mutate(f_review_scores_rating = cut(n_review_scores_rating, c(0,1,4,6), labels=c(1,2,3), right = F) )

df %>% 
  group_by(f_review_scores_rating) %>% 
  summarise(cnt=n()) %>% 
  arrange(-cnt)


############################
## n_number_of_reviews(LOG?)
df %>%
  filter(n_number_of_reviews <300) %>% 
  ggplot(aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = "cyan4", color = "white", alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews")

df %>%
  filter(n_number_of_reviews <250) %>% ggplot(aes(x=log(n_number_of_reviews) , y=price)) +
  geom_point(size=1.5, colour="cyan4", shape=16, alpha=0.6) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_classic()

describe(df$n_number_of_reviews)
df %>% 
  group_by(n_number_of_reviews) %>% 
  summarise(cnt=n())

##pool(0-1/1-99/99-max)
df <- df %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,99,max(df$n_number_of_reviews)+1), labels=c(1,2,3), right = F))

df %>% 
  group_by(f_number_of_reviews) %>% 
  summarise(cnt=n())

############################
## n_minimum_nights

###Min Nights dropping three observation with more than 180 nights min and other variable are also missing ####

df %>% 
  filter(n_minimum_nights<=180) %>% 
  summarise(num_values=n()) %>% 
  arrange(num_values)

df <- df %>% filter(n_minimum_nights<=180)


df %>% 
  group_by(n_minimum_nights) %>% 
  summarise(cnt = n()) %>% 
  arrange(-cnt)

ggplot(df, aes(n_minimum_nights)) +
  geom_histogram( fill = "cyan4", color = "white", alpha = 0.8, size = 0.25, binwidth = 1) +
  xlim(0,50)+
  xlab("N of minimum nights") +
  theme_classic()

describe(df$n_minimum_nights)

# Pool and categorize the number of minimum nights: 1,4,3, 3+
df <- df %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,4,6,max(df$n_minimum_nights)+1), labels=c(1,2,3), right = F))

df %>% 
  group_by(f_minimum_nights) %>% 
  summarise(cnt = n())


############################
## n_n_number_of_reviews
skimr::skim(df$n_number_of_reviews)

ggplot(data = df, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour="cyan4", shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="number of days since first review",y=" daily price")+
  theme_classic()





# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)
to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

