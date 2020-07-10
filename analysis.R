if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(rvest))
  install.packages("rvest", repos = "http://cran.us.r-project.org")
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(ggthemes))
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
daily_activity<-read_csv('data/data_daily_activity.csv')
daily_matches<- read_csv('data/data_daily_matches.csv')
in_app_purchases<-read_csv('data/data_in_app_purchases.csv')
daily_matches<- daily_matches%>%group_by(userId)%>%arrange(date, .by_group = TRUE)%>%mutate(matches_till_date = cumsum(matches))
#user_data<-daily_matches%>%group_by(userId)%>%summarise(avg_matches=mean(matches),total_matches=sum(matches))
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
  filter(!is.na(abTestGroup))



N_Day_Analysis<-
  user_data%>%
  group_by(abTestGroup, acquisition_date, days_since_acquisition)%>%
  summarise(n = n(), 
            revenue = sum(cost, na.rm = TRUE),
            conversion=sum(!is.na(cost)))%>%
  mutate(
    retention_rate = n / max(n),
    Cumulative_ARPU = cumsum(revenue) / max(n),
    Cumulative_conversion=cumsum(conversion)/max(n),
  )
N_Day_Analysis%>%
  ggplot(aes(as.factor(days_since_acquisition), Cumulative_ARPU, col = abTestGroup))+
  geom_boxplot()+
  theme_fivethirtyeight()
N_Day_Analysis%>%
  filter(days_since_acquisition >0)%>%
  ggplot(aes(as.factor(days_since_acquisition), retention_rate, col = abTestGroup))+
  geom_boxplot()+
  theme_fivethirtyeight()

user_data%>%
  filter(!is.na(product))%>%
  ggplot(aes(matches))+
  geom_histogram(
    aes(y = ..density..),
    fill = 'darkblue',
    alpha = 0.2,
    position = "identity"
  )+
  geom_vline(aes(xintercept = mean(matches, na.rm = TRUE)),
             linetype = "dashed")+
  geom_density()+
  scale_x_log10()+
  theme_fivethirtyeight()
purchased<-user_data%>%
  filter(!is.na(product))
density(purchased$matches,na.rm = TRUE)
d_fun<-ecdf(purchased$matches)
d_fun(8)
purchased%>%ggplot(aes(matches)) +
  stat_ecdf(col='blue')+
  theme_fivethirtyeight()+
  geom_vline(xintercept = 5)

fit_lm<-N_Day_Analysis%>%lm(conversion~n,data=.)
