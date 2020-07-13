
# install packages
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(rvest))
  install.packages("rvest", repos = "http://cran.us.r-project.org")
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(ggthemes))
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) 
  install.packages("kableExtra", 
                   repos = "http://cran.us.r-project.org")

# Read files from data folder
daily_activity<-read_csv('data/data_daily_activity.csv')
daily_matches<- read_csv('data/data_daily_matches.csv')
in_app_purchases<-read_csv('data/data_in_app_purchases.csv')
daily_matches<- daily_matches%>%group_by(userId)%>%arrange(date, .by_group = TRUE)%>%mutate(matches_till_date = cumsum(matches))
#user_data<-daily_matches%>%group_by(userId)%>%summarise(avg_matches=mean(matches),total_matches=sum(matches))

# merge data to single dataframe
user_data <-
  daily_activity %>%
  full_join(daily_matches, by =
              c('userId', 'date')) %>%
  full_join(in_app_purchases, by =
              c('userId', 'date'))
user_data<-
  user_data %>%
  group_by(userId) %>%  
  mutate(acquisition_date =  min(date))%>%
  mutate(
    days_since_acquisition = as.numeric(date - acquisition_date)
  )%>%
  filter(!is.na(abTestGroup))%>%ungroup()

# derive retention_rate,Cumulative_ARPU and Cumulative_conversion from user_data

N_Day_Analysis<-
  user_data%>%
  group_by(abTestGroup, acquisition_date, days_since_acquisition)%>%
  summarise(number_of_users = n(), 
            revenue = sum(cost, na.rm = TRUE),
            conversion=sum(!is.na(cost)))%>%
  mutate(
    retention_rate = number_of_users / max(number_of_users),
    Cumulative_ARPU = cumsum(revenue) / max(number_of_users),
    Cumulative_conversion=cumsum(conversion)/max(number_of_users),
  )%>%
  ungroup()
# orrelation study
cor(N_Day_Analysis[c('revenue','conversion','retention_rate')])

# Analyse cumulative ARPU
N_Day_Analysis%>%
  ggplot(aes(as.factor(days_since_acquisition), Cumulative_ARPU, col = abTestGroup))+
  geom_boxplot()+
  ggtitle("Figure 1 - cumulative ARPU")+
  xlab('days_since_acquisition')+
  ylab('Cumulative ARPU')+ 
  theme_economist() + 
  scale_colour_economist()
N_Day_Analysis%>%
  group_by(abTestGroup,days_since_acquisition)%>%
  summarise(average_ARPU=mean(Cumulative_ARPU))%>%
  ungroup()%>%
  ggplot(aes(as.factor(days_since_acquisition),average_ARPU,fill=abTestGroup))+
  geom_col(position = 'dodge')+
  xlab('days_since_acquisition')+
  ggtitle("Figure 2 - Average cumulative ARPU")+
  theme_economist() + 
  scale_colour_economist()+scale_fill_manual(values=c("#6794a7","#014d64","#01a2d9"))

# Analyse Retention
N_Day_Analysis%>%
  filter(days_since_acquisition >0)%>%
  ggplot(aes(as.factor(days_since_acquisition), retention_rate, col = abTestGroup))+
  geom_boxplot()+
  ggtitle("Figure 3 - Retention Rate")+
  xlab('days_since_acquisition')+
  ylab('Retention_Rate')+ 
  theme_economist() + 
  scale_colour_economist()
N_Day_Analysis%>%
  filter(days_since_acquisition!=0)%>%
  group_by(abTestGroup,days_since_acquisition)%>%
  summarise(avg_retention_rate=mean(retention_rate))%>%
  ggplot(aes(as.factor(days_since_acquisition),avg_retention_rate,fill=abTestGroup))+
  geom_col(position = 'dodge')+
  xlab('days_since_acquisition')+
  ggtitle("Figure 4 -  Average Retention Rate")+
  theme_economist() + 
  scale_colour_economist()+scale_fill_manual(values=c("#6794a7","#014d64","#01a2d9"))

# distribution Studies
purchased<-user_data%>%
  filter(!is.na(product))%>%
  group_by(userId)%>%
  arrange(date, .by_group = TRUE)%>%
  filter(row_number()==1)%>%
  ungroup()
dist_plot<-purchased%>%
  ggplot(aes(matches))+
  geom_density()+
  geom_histogram(
    aes(y = ..density..),
    alpha = 0.4,
    fill="#6794a7"
  )+
  scale_x_log10(breaks = c(1,2,4,8,12,25,40,100))+
  theme_economist() + 
  scale_colour_economist()
dist_plot+
  geom_vline(aes(xintercept = mean(matches, na.rm = TRUE)),
             linetype = "dashed")+
  ggtitle("Figure 5 - Density plot for purchase with matches")

# eCDF
purchased%>%
  ggplot(aes(matches))+
  stat_ecdf(geom='point') +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  ggtitle("Figure 6 - eCDF")+
  theme_economist() + 
  scale_colour_economist()
d_fun<-ecdf(purchased$matches)
d_fun(3)*100
d_fun(5)*100
(d_fun(25)-d_fun(5))*100

# individual distribution and eCDF study
purchased%>%
  group_by(abTestGroup)%>%
  summarise(
    under_3_matches=ecdf(matches)(3)*100,
    under_5_matches=ecdf(matches)(5)*100,
    between_5_and_25_matches=(ecdf(matches)(25)-ecdf(matches)(5))*100,
    after_25_matches=100-ecdf(matches)(25)*100)
