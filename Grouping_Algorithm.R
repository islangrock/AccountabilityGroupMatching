# This script matches writing accountability groups run by the Grad Center at Penn.
# The matching output is based on the Associated Qualtrics Survey filled out by
# interested graduate students and matches them based on type of group and availability. 
# This script uses a kmeans clustering approach in order to match most similar groups of a set size. 

# By Isabelle Langrock
# September 2022
##################################################################################

# First, set up the required packages
# If new to R or you don't have the packages installed you will have to run the following lines of code (remove the #) 

#install.packages("tidyverse")
library(tidyverse)

# Second, import the data 
# Download the csv file from Qualtrics and save it to your desktop as Accountability_data.csv for 
# this line of code to run as is. 

all_r <- read.csv("Desktop/Accountability_data_test.csv")

# Third, Tidy up the data frame, renaming variables so it's obvious what they are for. 

tidy_r <- all_r %>%
  select(Q12:Q20) %>%  # We select only the Question Response Columns 
  rename(Name = "Q12", Email = "Q13", school = "Q1", Student_type = "Q14", # We rename the relevant variables
         Year = "Q15", Partner = "Q4", Group_type="Q2", Freq = "Q3", 
         Avail_morn = "Q9_1", Avail_Afternoon="Q9_2", Avail_night= "Q9_3", 
         Where = "Q10") 

  # The Relevant variables for grouping are
  # Partner, Group_type, Freq, Avail_morn, Avail_Afternoon, Avail_night, and Where 

  # We also need to recode each response variable so that the answers are numeric for kmeans to work. 
  # In this case, It doesn't matter what number is assinged to each response value
  # just that each unique response value has a unique number assignned. 

tidy_r$Group_type_num<- recode(tidy_r$Group_type, "Accountability Group: share goals & updates only" = 0, 
      "Co-writing Group: write together regularly" = 1, 
       "Accountability Group: share goals & updates only,Co-writing Group: write together regularly" =2)

tidy_r$Freq_num<-recode(tidy_r$Freq, "Weekly"= 0, "Bi-Weekly (every other week)" = 1, "Monthly" =2, 
       "Weekly,Bi-Weekly (every other week),Monthly" = 3, "Weekly,Monthly"=4, 
       "Weekly,Bi-Weekly (every other week)"=5, "Bi-Weekly (every other week),Monthly"=6)

tidy_r$Where_num <- recode(tidy_r$Where, "In Person" = 0, "Virtually (e.g. on zoom)"=1,
                           "In Person,Virtually (e.g. on zoom)"=2)

summary(tidy_r)  # Check the averages of each group and make sure there are only 2 NAs 

  # Create new columns for each day x time each person mentions 
tidy_r<- tidy_r %>% 
  mutate(Morn_M = ifelse(str_detect(Avail_morn, "Mondays")==TRUE, 1, 0),
         Morn_T = ifelse(str_detect(Avail_morn, "Tuesdays")==TRUE, 1, 0),
         Morn_W = ifelse(str_detect(Avail_morn, "Wednesdays")==TRUE, 1, 0),
         Morn_Th = ifelse(str_detect(Avail_morn, "Thursdays")==TRUE, 1, 0),
         Morn_F = ifelse(str_detect(Avail_morn, "Fridays")==TRUE, 1, 0),
         Morn_Sat = ifelse(str_detect(Avail_morn, "Saturdays")==TRUE, 1, 0),
         Morn_Sun = ifelse(str_detect(Avail_morn, "Sundays")==TRUE, 1, 0),
         Aft_M = ifelse(str_detect(Avail_Afternoon, "Mondays")==TRUE, 1, 0),
         Aft_T = ifelse(str_detect(Avail_Afternoon, "Tuesdays")==TRUE, 1, 0),
         Aft_W = ifelse(str_detect(Avail_Afternoon, "Wednesdays")==TRUE, 1, 0),
         Aft_Th = ifelse(str_detect(Avail_Afternoon, "Thursdays")==TRUE, 1, 0),
         Aft_F = ifelse(str_detect(Avail_Afternoon, "Fridays")==TRUE, 1, 0),
         Aft_Sat = ifelse(str_detect(Avail_Afternoon, "Saturdays")==TRUE, 1, 0),
         Aft_Sun = ifelse(str_detect(Avail_Afternoon, "Sundays")==TRUE, 1, 0),
         Eve_M = ifelse(str_detect(Avail_night, "Mondays")==TRUE, 1, 0),
         Eve_T = ifelse(str_detect(Avail_night, "Tuesdays")==TRUE, 1, 0),
         Eve_W = ifelse(str_detect(Avail_night, "Wednesdays")==TRUE, 1, 0),
         Eve_Th = ifelse(str_detect(Avail_night, "Thursdays")==TRUE, 1, 0),
         Eve_F = ifelse(str_detect(Avail_night, "Fridays")==TRUE, 1, 0),
         Eve_Sat = ifelse(str_detect(Avail_night, "Saturdays")==TRUE, 1, 0),
         Eve_Sun = ifelse(str_detect(Avail_night, "Sundays")==TRUE, 1, 0))

  # omit any NAs (should just be the first two rows of the original data set because of Qualtrics download)
tidy_r <- tidy_r %>%
  na.omit()

  # Now the data should be clean and the necessary variables have been converted to mostly numeric. 


# Fourth, we can start to cluster! 


  # Partner Requests Only -- Make decisions based on number -- we can match them with kmeans as well. 
  
partners <- tidy_r %>%
  filter(Partner == "Partner")

model_partners<- partners %>%
  select(Group_type_num, Freq_num, Where_num, Morn_M:Eve_Sun)

  # identify how many partnerships we would want 
count(model_partners)/2

  # run kmeans assignment, change number to match from above 

fit_partners <- kmeans(model_partners, 13)

  # join cluster assignment with original dataset. 

partners <- data.frame(partners, fit_partners$cluster)


  # GROUP REQUESTS 

groups <- tidy_r %>%
  filter(Partner != "Partner")

model_groups <- groups%>%
  select(Group_type_num, Freq_num, Where_num, Morn_M:Eve_Sun)

  # decide how big we want our average group (here I said 4)
count(groups)/4

  # run kmeans assignment 
fit_groups<-kmeans(model_groups, 6)

groups <- data.frame(groups, fit_groups$cluster)

# All of our clustering is done! 
# Last, tidy up DF and save as csv file so we can review and email everyone. 

partners <- partners %>%
  select(Name:Q20, fit_partners.cluster)%>%
  rename(cluster="fit_partners.cluster")%>%
  mutate(cluster = paste("Partner", cluster, sep=""))

groups <- groups %>%
  select(Name:Q20, fit_groups.cluster) %>% 
  rename(cluster="fit_groups.cluster") %>%
  rbind(partners) %>% # combine dfs of partners and groups 
  rename(space= "Q18", goals="Q20") # make Column titles clear 

write.csv(groups, "Desktop/AccountabilityGroups.csv") # save! 
