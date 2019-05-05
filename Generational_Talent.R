require(devtools)
install_github("BillPetti/baseballr")
require(baseballr)
library(baseballr)
library(dplyr)
library(ggplot2)
standings_on_date_bref("2019-05-05", "NL Central", from = FALSE)


#start of real coding

#leaders from 2014-2018
fg_bat_leaders_14_18 <- fg_bat_leaders(x = 2014, y = 2018, league = "all", qual = "y", ind = 1)
#leaders from 2009-2013
fg_bat_leaders_09_13 <- fg_bat_leaders(x = 2009, y = 2013, league = "all", qual = "y", ind = 1)
#leaders from 50 years ago 5 season period
fg_bat_leaders_50_yr_ago <- fg_bat_leaders(x = 1965, y = 1969, league = "all", qual = "y", ind = 1)


#subset of data with players under 25, with select stats

#for 2014-2018
young_guns_last5 <- fg_bat_leaders_14_18 %>%
  select(Season, Name, Age, PA, AB, HR, BB, SO, 
         AVG, OBP, SLG, OPS, WAR, wOBA, wRC_plus, ISO) %>%
  filter(Age < 25) %>%
  mutate(ERA = "2014-2018")
#for 2009-2013
young_guns_5before <- fg_bat_leaders_09_13 %>%
  select(Season, Name, Age, PA, AB, HR, BB, SO, 
         AVG, OBP, SLG, OPS, WAR, wOBA, wRC_plus, ISO) %>%
  filter(Age < 25) %>%
  mutate(ERA = "2009-2013")
#for 1965-1969 
young_guns_throwback <- fg_bat_leaders_50_yr_ago %>%
  select(Season, Name, Age, PA, AB, HR, BB, SO, 
         AVG, OBP, SLG, OPS, WAR, wOBA, wRC_plus, ISO) %>%
  filter(Age < 25) %>%
  mutate(ERA = "2009-2013")

#summarizing 




#2014-2018
#averages over the last 5 seasons, split out by seasons 
#just to get a glimpse at the averages in each stat of recent seasons
young_guns_last5 %>%
  arrange(Season) %>%
  group_by(Season) %>%
  summarise(total_PA = sum(PA),
            mean_AVG = mean(AVG),
            mean_OPS = mean(OPS),
            mean_wRC_plus = mean(wRC_plus),
            total_HR = sum(HR),
            total_BB = sum(BB),
            total_SO = sum(SO),
            mean_WAR = mean(WAR),
            mean_wOBA = mean(wOBA),
            mean_ISO = mean(ISO))

#only weighted on-base average (wOBA), power (ISO), weighted runs created (wRC+), and wins above replace (WAR)
#again just out of curiosity
young_guns_last5 %>%
  arrange(Season) %>%
  group_by(Season) %>%
  summarise(mean_wRC_plus = mean(wRC_plus),
            mean_WAR = mean(WAR),
            mean_wOBA = mean(wOBA),
            mean_ISO = mean(ISO))

#the 5-season period averages of each of the stats wOBA,ISO,wRC+,WAR
young_guns_last5 %>%
  arrange(Season) %>%
  summarise(mean_wRC_plus = mean(wRC_plus),
            mean_WAR = mean(WAR),
            mean_wOBA = mean(wOBA),
            mean_ISO = mean(ISO))












#Final subsets for each era

Final_subset_2014_2018 <- young_guns_last5 %>%
  arrange(Season) %>%
  select(Season,
         Name,
         Age,
         wRC_plus,
         WAR,
         wOBA,
         ISO,
         HR) %>% 
  mutate(Years = "2014-2018") %>%
  select(Years, everything())

Final_subset_2009_2013 <- young_guns_5before %>%
  arrange(Season) %>%
  select(Season,
         Name,
         Age,
         wRC_plus,
         WAR,
         wOBA,
         ISO,
         HR) %>% 
  mutate(Years = "2009-2013") %>%
  select(Years, everything())

Final_subset_1965_1969 <- young_guns_throwback %>%
  arrange(Season) %>%
  select(Season,
         Name,
         Age,
         wRC_plus,
         WAR,
         wOBA,
         ISO,
         HR) %>% 
  mutate(Years = "1966-1969") %>%
  select(Years, everything())

Final_subset_all <- bind_rows(Final_subset_2014_2018,
                              Final_subset_2009_2013,
                              Final_subset_1965_1969) %>% arrange(Years)








#wOBA boxplot
ggplot(Final_subset_all, aes(x=Years, y=wOBA)) +
  geom_boxplot(outlier.shape = 25, outlier.fill = "red" ,outlier.color= 'red', aes(shape=Years, color=Years)) +
  geom_segment(aes(x=3.45, y=.30, xend=4, yend=.30), color="brown") +
  geom_segment(aes(x=3.45, y=.320, xend=4, yend=.320), color="brown") +
  geom_segment(aes(x=3.45, y=.340, xend=4, yend=.340), color="brown") +
  geom_segment(aes(x=3.45, y=.370, xend=4, yend=.370), color="brown") +
  geom_segment(aes(x=3.45, y=.40, xend=4, yend=.40), color="brown") +
  annotate("text",x=3.7, y=.305, label="poor") +
  annotate("text",x=3.7, y=.325, label="average") +
  annotate("text",x=3.7, y=.345, label="above average")+
  annotate("text",x=3.7, y=.375, label="great")+
  annotate("text",x=3.7, y=.405, label="excellent") +
  annotate("text",x=3, y=.337, label=".331") +
  annotate("text",x=2, y=.341, label=".335") +
  annotate("text",x=1, y=.329, label=".323") +
  ggtitle("Boxplot of wOBA for each group") 

#the medians for each group
median(Final_subset_2014_2018$wOBA)
median(Final_subset_2009_2013$wOBA)
median(Final_subset_1965_1969$wOBA)

#WAR boxplot
ggplot(Final_subset_all, aes(x = Years, y = WAR, Group=Years)) + 
  geom_boxplot(outlier.shape = 25, outlier.fill = "red" ,outlier.color= 'red',aes( shape=Years, color=Years)) +
  geom_segment(aes(x=3.45, y=2.5, xend=4, yend=2.5), color="brown") + 
  geom_segment(aes(x=3.45, y=3.5, xend=4, yend=3.5), color="brown") +
  geom_segment(aes(x=3.45, y=4.5, xend=4, yend=4.5), color="brown") +
  geom_segment(aes(x=3.45, y=5.5, xend=4, yend=5.5), color="brown") +
  geom_segment(aes(x=3.45, y=7.5, xend=4, yend=7.5), color="brown") +
  annotate("text",x=3.7, y=2.7, label="Solid Starter") +
  annotate("text",x=3.7, y=3.7, label="Good Player") +
  annotate("text",x=3.7, y=4.7, label="All-Star") +
  annotate("text",x=3.7, y=5.7, label="Superstar") +
  annotate("text",x=3.7, y=7.7, label="MVP") +
  annotate("text",x=3, y=3, label="2.8") +
  annotate("text",x=2, y=2.9, label="2.7") +
  annotate("text",x=1, y=2.9, label="2.7") +
  ggtitle("Boxplot of WAR for each group") 

median(Final_subset_2014_2018$WAR)
median(Final_subset_2009_2013$WAR)
median(Final_subset_1965_1969$WAR)


#ISO
ggplot(Final_subset_all, aes(x=Years, y=ISO)) +
  geom_boxplot(outlier.shape = 25, outlier.fill = "red" ,outlier.color= 'red', aes(color=Years)) +
  geom_segment(aes(x=3.45, y=.08, xend=4, yend=.08), color="brown") +
  geom_segment(aes(x=3.45, y=.1, xend=4, yend=.1), color="brown") +
  geom_segment(aes(x=3.45, y=.14, xend=4, yend=.14), color="brown") +
  geom_segment(aes(x=3.45, y=.2, xend=4, yend=.2), color="brown") +
  geom_segment(aes(x=3.45, y=.25, xend=4, yend=.25), color="brown") +
  geom_segment(aes(x=3, y=.173, xend=3.01, yend=.173), color="lightblue") +
  geom_segment(aes(x=2, y=.158, xend=2.01, yend=.158), color="lightblue") +
  geom_segment(aes(x=1, y=.134, xend=1.01, yend=.134), color="lightblue") +
  annotate("text",x=3.7, y=.09, label="Poor") +
  annotate("text",x=3.7, y=.11, label="average") +
  annotate("text",x=3.7, y=.155, label="above average")+
  annotate("text",x=3.7, y=.22, label="great")+
  annotate("text",x=3.7, y=.27, label="excellent") +
  annotate("text",x=3, y=.180, label=".173") +
  annotate("text",x=2, y=.164, label=".158") +
  annotate("text",x=1, y=.139, label=".134") +
  ggtitle("Boxplot of ISO for each group") 

median(young_guns_5before$ISO)
median(young_guns_throwback$ISO)

#wRC_plus Boxplot
ggplot(Final_subset_all, aes(x=Years, y=wRC_plus)) +
  geom_boxplot(outlier.shape = 25, outlier.fill = "red" ,outlier.color= 'red', aes(color=Years)) +
  geom_segment(aes(x=3.45, y=75, xend=4, yend=75), color="brown") +
  geom_segment(aes(x=3.45, y=100, xend=4, yend=100), color="brown") +
  geom_segment(aes(x=3.45, y=115, xend=4, yend=115), color="brown") +
  geom_segment(aes(x=3.45, y=140, xend=4, yend=140), color="brown") +
  geom_segment(aes(x=3.45, y=160, xend=4, yend=160), color="brown") +
  annotate("text",x=3.7, y=78, label="Poor") +
  annotate("text",x=3.7, y=103, label="average") +
  annotate("text",x=3.7, y=118, label="above average")+
  annotate("text",x=3.7, y=143, label="great")+
  annotate("text",x=3.7, y=163, label="excellent") +
  annotate("text",x=3, y=109, label="106") +
  annotate("text",x=2, y=108, label="105") +
  annotate("text",x=1, y=111, label="108") +
  ggtitle("Boxplot of wRC+ for each group")

median(young_guns_last5$wRC_plus)
max(young_guns_last5$wRC_plus)
median(young_guns_5before$wRC_plus)
median(young_guns_throwback$wRC_plus)



#HR Boxplot
ggplot(Final_subset_all, aes(x=Years, y=HR)) +
  geom_boxplot(outlier.shape = 25, outlier.fill = "red" ,outlier.color= 'red', aes(color=Years)) +
  geom_segment(aes(x=3.45, y=5, xend=4, yend=5), color="brown") +
  geom_segment(aes(x=3.45, y=10, xend=4, yend=10), color="brown") +
  geom_segment(aes(x=3.45, y=15, xend=4, yend=15), color="brown") +
  geom_segment(aes(x=3.45, y=20, xend=4, yend=20), color="brown") +
  geom_segment(aes(x=3.45, y=25, xend=4, yend=25), color="brown") +
  annotate("text",x=3.7, y=6, label="Poor") +
  annotate("text",x=3.7, y=11, label="average") +
  annotate("text",x=3.7, y=16, label="above average")+
  annotate("text",x=3.7, y=21, label="great")+
  annotate("text",x=3.7, y=26, label="excellent") +
  annotate("text",x=3, y=20, label="19") +
  annotate("text",x=2, y=17, label="16") +
  annotate("text",x=1, y=14, label="13") +
  ggtitle("Boxplot of HR for each group")

median(Final_subset_2014_2018$HR)
median(Final_subset_2009_2013$HR)


