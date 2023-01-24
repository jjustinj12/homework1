#########################################################################
## Read in enrollment data for january of each year
#########################################################################
install.packages("tidyverse")
library(tidyverse)
for (y in 2007:2015) {
  ## Basic contract/plan information
  ma.path=paste0("data/input/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Contract_Info_",y,"_01.csv")
  contract.info=read_csv(ma.path,
                         skip=1,
                         col_names = c("contractid","planid","org_type","plan_type",
                                       "partd","snp","eghp","org_name","org_marketing_name",
                                       "plan_name","parent_org","contract_date"),
                         col_types = cols(
                           contractid = col_character(),
                           planid = col_double(),
                           org_type = col_character(),
                           plan_type = col_character(),
                           partd = col_character(),
                           snp = col_character(),
                           eghp = col_character(),
                           org_name = col_character(),
                           org_marketing_name = col_character(),
                           plan_name = col_character(),
                           parent_org = col_character(),
                           contract_date = col_character()
                         ))
  
  contract.info = contract.info %>%
    group_by(contractid, planid) %>%
    mutate(id_count=row_number())
  
  contract.info = contract.info %>%
    filter(id_count==1) %>%
    select(-id_count)
  
  ## Enrollments per plan
  ma.path=paste0("data/input/monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Enrollment_Info_",y,"_01.csv")
  enroll.info=read_csv(ma.path,
                       skip=1,
                       col_names = c("contractid","planid","ssa","fips","state","county","enrollment"),
                       col_types = cols(
                         contractid = col_character(),
                         planid = col_double(),
                         ssa = col_double(),
                         fips = col_double(),
                         state = col_character(),
                         county = col_character(),
                         enrollment = col_double()
                       ),na="*")
  
  
  ## Merge contract info with enrollment info
  plan.data = contract.info %>%
    left_join(enroll.info, by=c("contractid", "planid")) %>%
    mutate(year=y)
  
  ## Fill in missing fips codes (by state and county)
  plan.data = plan.data %>%
    group_by(state, county) %>%
    fill(fips)
  
  ## Fill in missing plan characteristics by contract and plan id
  plan.data = plan.data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ## Fill in missing contract characteristics by contractid
  plan.data = plan.data %>%
    group_by(contractid) %>%
    fill(org_type,org_name,org_marketing_name,parent_org)
  
  ## Collapse from monthly data to yearly
  plan.year = plan.data %>%
    group_by(contractid, planid, fips) %>%
    arrange(contractid, planid, fips) %>%
    rename(avg_enrollment=enrollment)
  
  write_rds(plan.year,paste0("data/output/ma_data_",y,".rds"))
}

full.ma.data <- read_rds("data/output/ma_data_2007.rds")
for (y in 2008:2015) {
  full.ma.data <- rbind(full.ma.data,read_rds(paste0("data/output/ma_data_",y,".rds")))
}

write_rds(full.ma.data,"data/output/full_ma_data.rds")
sapply(paste0("ma_data_", 2007:2015, ".rds"), unlink)


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

