library(pacman)
library(tidyverse)

full.ma.data <- read_rds("data/output/full_ma_data.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds")
final.data<-read_rds("data/output/final_ma_data.rds")


#number of observations in the data set 
observations=nrow(full.ma.data)
cat("total number of observations in the data set", observations)
#19126783

#number of unique plan types 
length(unique(full.ma.data$plan_type))
#27

#table of plan types for each year 


table1 <- full.ma.data %>%
  group_by(year, plan_type) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = year, values_from = count)
table1
knitr::kable(table1,
             type="html", caption = "Plan Count by Year", booktabs = TRUE) #this makes it look better 

#Remove all special needs plans (SNP), employer group plans (eghp), and all “800-series” plans. 
#Provide an updated version of Table 1 after making these exclusions.
# ** I am unsure of how to filter out 800 series plans **
table2 <- full.ma.data %>%
  filter(snp=="No" & eghp=="No" & (planid < 800 | planid >= 900))%>%
  group_by(year, plan_type) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = year, values_from = count)
table2
knitr::kable(table2,
             type="html", caption = "Plan Count by Year", booktabs = TRUE) #this makes it look better 



# final.data this is the data set where Dr. McCarthy merged both contract service area data and enrollment data 
#5 create a graph with mean enrollment per county by year 

ma_per_county<-final.data %>%
  filter(!is.na(planid) & !is.na(fips)) %>%
  filter(year>=2008 & year<=2015) %>%
  group_by(year,fips) 
ma_per_county

graph_5 <- ggplot(ma_per_county, aes(x=year, y=avg_enrollment, group=fips, color=fips)) +
  geom_line() +
  geom_point() +
  ggtitle("Number of Individuals per County per Year") +
  xlab("Year") +
  ylab("Average Enrollment")
graph_5


#6 
# drop_na function allows you to remove any missing data 
graph_6<- plan.premiums %>%
  drop_na(premium) %>%
  group_by(year)%>%
  summarize(avgprem=mean(premium))
graph_6

ggplot(graph_6, aes(x=year, y=avgprem)) +
  geom_point() +
  geom_line() +
  ggtitle("Average Premium by year")+
  xlab("Year") +
  ylab("Average Premium")

#7 percentage =0 

graph_7<- plan.premiums %>%
  drop_na(premium) %>%
  group_by(year)%>%
  summarize(zero_prem=(mean(premium==0) * 100))
graph_7
ggplot(graph_7, aes(x=year, y=zero_prem)) +
  geom_point() +
  geom_line() +
  ggtitle("Percent of 0$ premiumns by Year")+
  xlab("Year") +
  ylab("Percent of plans with 0$ Premiums")


#8
#We dropped thed 800 series plans because they are employer sponsored plans 

#Why do so many plans charge a $0 premium? What does that really mean to a beneficiary?
#Briefly describe your experience working with these data (just a few sentences). Tell me one thing you learned and one thing that really aggravated you.


