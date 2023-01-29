library(pacman)
library(tidyverse)
library(scales)

full.ma.data <- read_rds("data/output/full_ma_data.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds")
final.data<-read_rds("data/output/final_ma_data.rds")


#1. total number of observations
tot.obs<- as.numeric(count(full.ma.data %>% ungroup()))
saveRDS(tot.obs, file = "total_observations.rds")


#2. this is counts the number of observations in each plan 
plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
number_of_plantypes<-nrow(plan.type.table)
saveRDS(number_of_plantypes, file = "number_of_plantypes.rds")
write.csv(plan.type.table, file = "plan_type_table.csv")

#3. Plan types by year 
plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n) %>% filter(plan_type!="NA")
plan.type.year1 <-pivot_wider(plan.type.year1, names_from="year", values_from= "n", names_prefix="")
write.csv(plan.type.year1, file = "plan_type_year1.csv")

#4. Plan types by year cleaned (removed SNP, EGHP, 800 plans)
final.plans <-full.ma.data %>%
  filter(snp == "No" & eghp == "No" & 
           (planid <800 | planid >=900))
plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from="year", values_from="n", names_prefix = "Count_")
plan.type.year2
write.csv(plan.type.year2, file = "plan_type_year2.csv")

#5. Average enrollmment over time
final.data <- final.plans %>%
  inner_join(contract.service.area %>%
               select(contractid, fips,year),
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment)) #limiting to plans with non-missing enrollment 


fig.avg.enrollment <- final.data %>%
  group_by(fips, year) %>%
  select(fips, year, avg_enrollment) %>%
  summarize(all_enroll=sum(avg_enrollment))%>%
  ggplot(aes(x=as.factor(year), y=all_enroll))+ 
  stat_summary(fun.y="mean", geom="bar") +
  labs(
    x="Year",
    y="People",
    title="Average number of Medicare Advantage 
    enrollees per county from 2007 to 2015"
  ) + scale_y_continuous(labels=comma) + 
  theme_bw()
fig.avg.enrollment

figure_5<- ggsave(filename = "fig_avg_enrollment.png", plot = fig.avg.enrollment, height = 8, width = 10)


#6
#fig.avg.premium<- prem.data %>% ungroup() %>% group_by(year) %>%
#  ggplot(aes(x=as.factor(year), y=premium, ))


final.data.pen <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>% select(-ssa) %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))

final.state <- final.data.pen %>% 
  group_by(state) %>% 
  summarize(state_name=last(na.omit(state_long)))

final.data <- final.data %>%
  left_join(final.state,
            by=c("state"))

final.data <- final.data %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year")) 
view(final.data)
fig.avg.premiums<- ggplot(data=final.data, aes(x=year, y=premium)) +
  stat_summary(fun="mean", geom = "bar") +
  labs(
    x= "Year",
    y= "Average Premium Price",
    title ="Average Premium over time"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()


fig.avg.premiums
figure_6<- ggsave(filename = "fig_avg_premiums.png", plot = fig.avg.premiums, height = 8, width = 10)



percent_zero_plans<- final.data %>%
  drop_na(premium) %>%
  group_by(year)%>%
  summarize(zero_prem=(mean(premium==0) * 100))

percent_zero_plans

library(ggplot2)
ggplot(percent_zero_plans, aes(x=year, y=zero_prem)) +
  geom_line() +
  ggtitle("Percent of 0$ premiumns by Year")+
  xlab("Year") +
  ylab("Percent of plans with 0$ Premiums")


perc_zero_graph<- ggplot(percent_zero_plans, aes(x=year, y=zero_prem)) +
  ggtitle("Percent of 0$ premiumns by Year")+
  xlab("Year") +
  ylab("Percent of plans with 0$ Premiums")

figure_7<- ggsave(filename = "perc_zero_graph.png", plot = perc_zero_graph, height = 8, width = 10)




#8: the reason we are dropping 800-series plans because these are insurance plans offered by employers/unions
# given to retired beneficiaries and specifically 800 plans are managed through third party companies such as 
# medicare advantage organizations. This is removed because they are different from plans from traditional medicare or Medicare advantage not through one's employer. 


rm(list=c("full.ma.data", "contract.service.area", "ma.pentration.data", 
          "plan.premiums", "final.plans", "final.data", "fig.avg.enrollment"))
save.image("Hwk1_workspace.Rdata")





