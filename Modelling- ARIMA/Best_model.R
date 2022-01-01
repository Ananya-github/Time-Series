```{r include=FALSE}
#install.packages(("dplyr"))
library(dplyr)

#install.packages(("tsibble"))
library(tsibble)
#install.packages("xts")
library(xts)
#install.packages("ggplot2")
library(ggplot2)
library(reshape)
library(reshape2)
library(tidyr)
library(tidyverse)
library(data.table)
library(zoo)
library(bizdays)
library(ggeasy)
library(harrypotter)
library(bizdays)
library(timeDate)
library(astsa)
library(fpp2)

# Visualizations
library(ggplot2)
library(plotly)
library(MASS)
library(dplyr)

library(magrittr)
library(forecast)
library(feasts)
library(GGally)
library(fpp2)
require(stats)
library(uroot)
install.packages('fable')
library(fable)

library(lubridate)
library(zoo)
library(magrittr)
library(forecast)
#install.packages("feasts")
library(feasts)
#install.packages("GGally")
library(GGally)
library(fpp2)

```


# Exploratory Data Analysis

Overall we observe that the data from the period 2017 to 2019 tends to be seasonal with peaks during the mid-months.The number of applicants in 2017 and early 2019 tend to be comparatively low and inconsistent with the trend observed during the late-2017 and the year 2018, this may be due to the recovery from the demonetization in November-2016.This  inconsistent data may lead to noise while forecasting.
On dividing based on segments 
```{r include=FALSE}
#Reading the files for test and train
train = read.csv("train.csv")
test = read.csv("test.csv")

holiday=read.csv("indian_holidays.csv")
colnames(holiday)<-c("application_date","Holiday")
train<- train %>% left_join(holiday,by="application_date")
train$application_date=as.Date(train$application_date, "%d-%m-%Y")
summary(train)
```
```{r include= FALSE}
train
```

```{r include= FALSE}
train$Holiday <- train$Holiday %>% replace_na('0')

train
```


```{r include=FALSE}
#Converting application date into date format.
train$application_date<-as.Date(train$application_date,format = "%d-%m-%Y")
train$segment<-as.character(train$segment)
# # converting holiday data into date format
# train_holidays$application_date<-as.Date(train$application_date,format = "%d-%m-%Y")
# train_holidays$segment<-as.character(train$segment)
str(train)
```
```{r include=FALSE}
colnames(train)
```
```{r echo= FALSE, include=FALSE}
#Replacing values for certain columns 
# train$zone %>% replace_na("Not Specified")
# train$state %>% replace_na("Not Specified")
# train$branch_id %>% replace_na("Not Specified")
```


```{r include=FALSE}
#Adding additional columns of date, month and year 
train$Day<-format(train$application_date,"%d")
train$Month<-format(train$application_date,"%m")
train$Year<-format(train$application_date,"%Y")
train



# colSums(sapply(train, is.na))
# train <- na.omit(train)
# train %>% distinct()
# 
# #Converting to tsibble
# train <- train%>%
#   mutate(Quarter = yearquarter(Date)) %>%
#   select(-Date) %>%
#   as_tsibble(key = c(segment,branch_id,no_of_applicants),
#              index = Quarter)
# #Converting to time series object
# #train_ts <- xts(train$no_of_applicants,train$application_date)
# #train_ts
# #Null values ijn each column



```
```{r include=FALSE}
#Removing rows with any missing values 
#train <- train[complete.cases(train), ]
train
```
```{r include=FALSE}
dim(train)
```
```{r include=FALSE}
#Splitting the dataframe based on segment
Segment1 <- train%>% filter(segment== '1')
View(Segment1)
Segment2 <- train%>% filter(segment== '2')
View(Segment2)
```
```{r include=FALSE}
#Removing rows with any missing values 
train_ts <- train[complete.cases(train), ]
```

```{r include=FALSE}
#Changing the dataframe to s time series object
train_ts<- train_ts %>%
  mutate(Quarter = yearquarter(application_date)) %>%
  as_tsibble(key = c(segment,branch_id,state,zone,no_of_applicants,Day,Month,Year,Holiday),
             index = Quarter)


```
```{r include=FALSE}
# train_ts <- xts(train$no_of_applicants,train$Date)
class(train_ts)
dim(train_ts)
```


```{r warning=FALSE,include=FALSE}
fig <- plot_ly(train, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~application_date, y = ~no_of_applicants)%>%
  layout(showlegend = F, width=600)
fig <- fig %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 800)
```


```{r warning=FALSE, include=FALSE}
#fig
```
```{r include=FALSE}
# fig <- plot_ly()%>%
#   add_trace(data = train, type = 'scatter', mode = 'lines', fill = 'tozeroy', x = ~application_date, y = ~no_of_applicants, name = 'No.of applicants')%>%
#   layout(showlegend = F,
#           zerolinecolor = '#ffff',
#                       zerolinewidth = 2,
#                       gridcolor = 'ffff'),
#          xaxis = list(zerolinecolor = '#ffff',
#                       zerolinewidth = 2,
#                       gridcolor = 'ffff'),
#          plot_bgcolor='#e5ecf6')
# options(warn = -1)
# fig <- fig %>%
#   layout(
#          xaxis = list(zerolinecolor = '#ffff',
#                       zerolinewidth = 2,
#                       gridcolor = 'ffff'),
#          yaxis = list(zerolinecolor = '#ffff',
#                       zerolinewidth = 2,
#                       gridcolor = 'ffff'),
#          plot_bgcolor='#e5ecf6', width = 1000)
# 
# 
# fig
```

```{r include=FALSE}
#Count based on zone, state,segment and date

zone_count<- count(train,zone)
# View(zone_count)

zone_count_plot <- plot_ly(zone_count, x=~zone, y = ~n, type = 'bar',name = ~zone,
                           text = ~n, textposition = 'auto', width = 1.5)
zone_count_plot <- zone_count_plot  %>% layout(title = "ZONE COUNT",
                                               xaxis = list(title = "ZONE"),
                                               yaxis = list(title = "NO.OF APPLICANTS"))


#State

state_count<- count(train,state)
#View(state_count)

state_count_plot <- plot_ly(state_count, x=~state, y = ~n, type = 'bar',name = ~state,
                            text = ~n, textposition = 'auto', width = 1)
state_count_plot <- state_count_plot  %>% layout(title = "STATE COUNT",
                                                 xaxis = list(title = "STATE",tickangle = 90),
                                                 yaxis = list(title = "NO.OF APPLICANTS"))

#state_count_plot 
#Segment

segment_count<- count(train,segment)
#View(segment_count)

segment_count_plot <-plot_ly(segment_count, labels = ~segment, values = ~n, type = 'pie')
segment_count_plot <- segment_count_plot %>% layout(title = 'OVERALL SEGMENT DISTRIBUTION',
                                                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

We observe that majority of the applications belong to Segment 1 and the zone to receive highest number of applications is the East zone. It is observed that the state of Maharashtra has highest number of application across is branches from 2017-2019.
```{r}


segment_count_plot 
zone_count_plot
```
```{r warning=FALSE,include=FALSE}
ggplot(train, aes(x =train$application_date, y = train$no_of_applicants, color = ~segment)) +
  geom_area(colour = "black", size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues")
```

```{r warning=FALSE,include=FALSE}
fig <- plot_ly(train,x =train$application_date, y = train$no_of_applicants, color = ~segment) 
fig <- fig %>% add_lines()

fig
```
```{r include=FALSE}
train
```

```{r warning = FALSE, message= FALSE, echo=FALSE,include=FALSE}
#Group by data on the basis of application_date,segment and sum no_of_applicants.

data_grp<-train %>% group_by(application_date,segment,Holiday) %>% 
  summarise(no_of_applicants_sum  = mean(no_of_applicants))
View(data_grp)
```
```{r echo=FALSE,include=FALSE}
## Create two dataframes

#Filter data frames based on segment, then convert them into tsibble for segment 1
data_1<-data_grp %>% filter(segment %in% c(1))
data_1<-data_1 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)
```


```{r include=FALSE}
#For segment 1, fill gaps for missing values



hh<- data.frame(application_date=seq(min(data_1$application_date), max(data_1$application_date), by="days"))
data_1 <- merge(data_1,hh,by.x='application_date',by.y='application_date',all.x=T,all.y=T)
data_1$segment[is.na(data_1$segment)] <- 1
data_1$segment <- NULL
head(data_1)
data_1<-data_1 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)
is_tsibble(data_2)
```


```{r echo=FALSE, include=FALSE}
#For segment 1, interpolation for missing values of number of applicants

x <- zoo(data_1$no_of_applicants_sum,data_1$application_date)
x <- as.ts(round(x))
data_1$no_of_applicants_sum <- na.interp(round(x))
is_tsibble(data_1)


```
```{r include=FALSE}
#Filter dataframes based on segment, then convert them into tsibble for segment 2
data_2<-data_grp %>% filter(segment %in% c(2))
data_2<-data_2 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)

```


```{r include=FALSE}
#For segment 2, fill gaps for missing values


hh<- data.frame(application_date=seq(min(data_2$application_date), max(data_2$application_date), by="days"))
data_2 <- merge(data_2,hh,by.x='application_date',by.y='application_date',all.x=T,all.y=T)
data_2$segment[is.na(data_2$segment)] <- 1
data_2$segment <- NULL
head(data_2)
data_2<-data_2 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)
is_tsibble(data_2)
```



```{r include=FALSE}
#For segment 2, interpolation for missing values of number of applicants

x <- zoo(data_2$no_of_applicants_sum,data_2$application_date)
x <- as.ts(round(x))
data_2$no_of_applicants_sum <- na.interp(round(x))
is_tsibble(data_2)

```


```{r include= FALSE}
sum(is.na(data_1))
sum(is.na(data_2))
```
```{r}
#Filter data frames based on segment, then convert them into tsibble for segment 1
data_1<-data_grp %>% filter(segment %in% c(1))
data_1<-data_1 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)
```


```{r include=FALSE}
#For segment 1, fill gaps for missing values



hh<- data.frame(application_date=seq(min(data_1$application_date), max(data_1$application_date), by="days"))
data_1 <- merge(data_1,hh,by.x='application_date',by.y='application_date',all.x=T,all.y=T)
data_1$segment[is.na(data_1$segment)] <- 1
data_1$segment <- NULL
head(data_1)
data_1<-data_1 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)
is_tsibble(data_2)
```


```{r echo=FALSE, include=FALSE}
#For segment 1, interpolation for missing values of number of applicants

x <- zoo(data_1$no_of_applicants_sum,data_1$application_date)
x <- as.ts(round(x))
data_1$no_of_applicants_sum <- na.interp(round(x))
is_tsibble(data_1)


```
```{r include=FALSE}
#Filter dataframes based on segment, then convert them into tsibble for segment 2
data_2<-data_grp %>% filter(segment %in% c(2))
data_2<-data_2 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)

```


```{r include=FALSE}
#For segment 2, fill gaps for missing values


hh<- data.frame(application_date=seq(min(data_2$application_date), max(data_2$application_date), by="days"))
data_2 <- merge(data_2,hh,by.x='application_date',by.y='application_date',all.x=T,all.y=T)
data_2$segment[is.na(data_2$segment)] <- 1
data_2$segment <- NULL
head(data_2)
data_2<-data_2 %>%mutate(application_date = as.Date(application_date)) %>%
  as_tsibble(index = application_date)
is_tsibble(data_2)
```



```{r include=FALSE}
#For segment 2, interpolation for missing values of number of applicants

x <- zoo(data_2$no_of_applicants_sum,data_2$application_date)
x <- as.ts(round(x))
data_2$no_of_applicants_sum <- na.interp(round(x))
is_tsibble(data_2)

```


```{r include= FALSE}
sum(is.na(data_1))
sum(is.na(data_2))
```



```{r echo=FALSE}

# # 1. Time plots 
# 
autoplot(data_1, no_of_applicants_sum) +
 labs(y = "Total number of applications",
      title = "Number of applications in Neo bank for Segment 1")
```
```{r}
data_1%>%
  gg_season(no_of_applicants_sum, period="year") +
  labs(y = "Total number of applications",
       title = "Yearly Seasonal plot: Number of applications in Neo bank")
```







```{r}
 autoplot(data_2, no_of_applicants_sum) +
   labs(y = "Total number of applications",
       title = "Number of applications in Neo bank for Segment 2")
```


```{r echo=FALSE,warning=FALSE,message=FALSE, include=FALSE}
data_1 %>%
  gg_season(no_of_applicants_sum, period="week") +
  labs(y = "Total number of applications",
       title = "Weekly Seasonal plot: Number of applications in Neo bank")
```
```{r include=FALSE}
data_2 %>%
  gg_season(no_of_applicants_sum, period="week") +
  labs(y = "Total number of applications",
       title = "Weekly Seasonal plot: Number of applications in Neo bank")
```


```{r}
# b) Yearly seasonality

train_ts %>%
  gg_season(no_of_applicants, period="year") +
  labs(y = "Total number of applications",
       title = "Yearly Seasonal plot: Number of applications in Neo bank")
```


```{r}
data_2%>%
  gg_season(no_of_applicants_sum, period="year") +
  labs(y = "Total number of applications",
       title = "Yearly Seasonal plot: Number of applications in Neo bank")
```

```{r}
# # 3. Seasonal sub series plots
# 
# options(repr.plot.width=57 ,repr.plot.height= 10)
# data_1 %>%
#   gg_subseries(no_of_applicants_sum) +
#   labs(
#     y = "Total number of applications",
#     title = "Number of applications in Neo bank"
#   )

#Segment+ Zone 
ggplot(Segment1, aes(x = application_date, y = no_of_applicants)) +
geom_line() +
facet_wrap( ~ segment + zone, scales = "free_y", ncol = 5)+
theme(axis.text.x=element_text(angle=90))
```
.

```{r}
# data_2 %>%
#   gg_subseries(no_of_applicants_sum) +
#   labs(
#     y = "Total number of applications",
#     title = "Number of applications in Neo bank"
#   )

month_summary <-Segment1 %>% 
                    group_by(Month) %>%
                    summarise(no_of_applicants= sum(no_of_applicants),.groups='drop') 
month_summary %>%
         ggplot(aes(x = reorder(Month, no_of_applicants), y = no_of_applicants, fill = Month)) +
         geom_col() +
         scale_fill_hp_d(option = "RonWeasley") +
         scale_fill_brewer(palette = "Set3") +
         scale_y_continuous(limits = c(0, 300000), expand = c(0,0)) +
         labs(title = "Total number of applicants in  each month", x = "Month", y = "Total applicants") +
         theme_classic() 
```


```{r include=FALSE}
ggplot(train, aes(x=(Holiday), fill=(Holiday) )) +
geom_bar() +
scale_fill_brewer(palette = "Set3") +
labs(x='Holiday',y='Number of Applications') +
theme_minimal()+
theme(legend.position="none")
```

```{r}
data%>%
  pivot_wider(values_from=no_of_applicants, names_from=state)%>% 
  GGally::ggpairs(columns = 2:9)
```



# 5. Lag plots
```{r echo=FALSE,include=FALSE}
options(repr.plot.width=50 ,repr.plot.height= 10)
data_1 %>%
  gg_lag(no_of_applicants_sum, geom = "point") +
  labs(x = "lag(Total number of applications, k)")

```

```{r echo=FALSE,include=FALSE}
data_2 %>%
  gg_lag(no_of_applicants_sum, geom = "point") +
  labs(x = "lag(Total number of applications, k)")
```



```{r include=FALSE}
dcmp <- data_1%>%
  model(stl = STL(no_of_applicants_sum))
```


```{r echo=FALSE}
components(dcmp) %>% autoplot()

```


```{r include=FALSE}
dcmp <- data_2%>%
  model(stl = STL(no_of_applicants_sum))
```



```{r}
components(dcmp) %>% autoplot()
```


```{r echo=FALSE}
par(mfrow=c(2,2))
a=data_1 %>% tsibble::fill_gaps() %>%
  ACF(no_of_applicants_sum) %>%
  autoplot() +
  labs(title="ACF for Segment 1")

b=data_1 %>% tsibble::fill_gaps() %>%
  PACF(no_of_applicants_sum) %>%
  autoplot() +
  labs(title="PACF for Segment 1")

c=data_2 %>% tsibble::fill_gaps() %>%
  ACF(no_of_applicants_sum) %>%
  autoplot() +
  labs(title="ACF for Segment 2")

d=data_2 %>% tsibble::fill_gaps() %>%
  PACF(no_of_applicants_sum) %>%
  autoplot() +
  labs(title="PACFfor Segment 2")

```
```{r}
#install.packages('patchwork')
library(patchwork)

a+ b +c +d

```
# BEST MODEL



```{r echo=FALSE}
fore1<-fit %>% forecast(h=30)
df_s <- data.frame(id = c(1:60),
                   no_of_applicants = c(tail(fore$.mean,30), tail(fore1$.mean,30))
)
write.csv(df_s,"D:\\TimeSeries_Task\\arimawithoutseason_new.csv", row.names = FALSE)

```