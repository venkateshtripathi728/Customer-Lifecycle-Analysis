# Importing required libraries for data manipulation and data visualization

library(dplyr)
library(lubridate)
library(ggplot2)
library(Amelia)
#Question 1

setwd('C:/Users/venkatesh/Documents/CRM')
getwd()
# Importing sessions database and formatting date for summer season
summersess<-read.csv('summersesstrx.csv')
summersess$Date <- as.Date(summersess$Date,format = "%Y-%m-%d")
# Importing micro-transactions database and formatting date for summer season
summerfin<-read.csv('summerfintrx.csv')
summerfin$Date <- as.Date(summerfin$Date,format = "%Y-%m-%d")
# Importing sessions database and formatting date for fall season
fallsess <- read.csv('fallsesstrx.csv')
fallsess$Date <- as.Date(fallsess$Date,format = "%Y-%m-%d")
# Importing micro-transactions database and formatting date for fall season
fallfin <- read.csv('fallfintrx.csv')
fallfin$Date <- as.Date(fallfin$Date,format = "%Y-%m-%d")
# Database containing information like demography,age and gender at unique customer level
customerdata<-read.csv('customerdata.csv')


# Our history period is the summer period where we observe our independent variables
# in the past. The history period starts from 1st May 2018-31st August 2018
startdatehistory <- as.Date("01-05-2018",format="%d-%m-%Y")
enddatehistory <- as.Date("31-08-2018",format="%d-%m-%Y")

# Our forecast period is the fall period where we observe our dependent variable
# and observe the churning of customers from the summer period. It starts from 
# 1st September 2018-31st October 2018
startdateforecast <- as.Date("01-09-2018",format="%d-%m-%Y")
enddateforecast <- as.Date("31-10-2018",format="%d-%m-%Y")

# Creating a function createtablesession which helps in creating a recency,
# frequency, cohort and sessions metrics database at the unique user level. The database includes metrics
# like total raids, total pokemons captured,total duration spent and total distance
# covered by a unique user. This is recency and frequency for sessions played.The cohorts are based on 
# the first date the player starts playing.
createtablesession <- function(df1,startDate,endDate){
  
  df1 <- df1[df1$Date>= startDate & df1$Date<= endDate,]
  df1_grouped <- df1%>%group_by(CustomerID)%>%
    summarise(frequency=n(),recency=endDate-max(as.Date(Date,format="%Y-%m-%d")),
              cohort_date=min(Date)%>%day(),
              cohort_month=min(Date)%>%month(),
              cohort_final=paste(cohort_date,cohort_month,sep="-"),
              sum_pokestops=sum(Pokestops),
              sum_raids=sum(Raids),
              sum_gyms=sum(Gyms),
              sum_pokemons=sum(Pokemons),
              sum_distance=sum(Distance),
              sum_duration=sum(Duration))%>%
    ungroup()
  
  
} 

# Creating a recency, frequency and sessions metrics database for summer season and fall season by using createtablefunction
# Creating frequency and recency of sessions played
history_summer_sessions <- createtablesession(summersess,startdatehistory,enddatehistory) %>% rename(frequency_sessions = frequency, recency_sessions = recency)
forecast_fall_sessions <- createtablesession(fallsess,startdateforecast,enddateforecast) %>% rename(frequency_sessions = frequency, recency_sessions = recency)

# Creating a monetary value database at the unique user level so as to find out
# the total revenues from the microtransactions involved.Microtransactions are 
# important as the company wants to develop the insights into the patterns of 
# in game purchases by users. We create this database with the createtablefintransactions
# function.This database creates recency and frequency for the microtransactions involved
createtablefintransactions <- function(data, startDate, endDate){
  
  data <- data[data$Date>= startDate & data$Date<= endDate,]
  data <- data %>%
  mutate(monetaryvalue=ifelse(ProductID==1,2.99,ifelse(ProductID==2,4.99,ifelse(ProductID==3,9.99,ifelse(ProductID==4,25,ifelse(ProductID==5,99,0))))))
  data_grouped <- data%>%group_by(CustomerID)%>%summarise(sum_monetary=sum(monetaryvalue), frequency=n(), recency=endDate-max(as.Date(Date,format="%Y-%m-%d")))

}

# Creating two monetary databases for both summer and fall database. Creating frequency and recency of transactions
history_summer_trx <- createtablefintransactions(summerfin, startdatehistory, enddatehistory) %>% rename(frequency_transactions = frequency, recency_transactions = recency)
forecast_fall_trx <- createtablefintransactions(fallfin, startdateforecast, enddateforecast) %>% rename(frequency_transactions = frequency, recency_transactions = recency)

# Merging recency,frequency and sessions database with the monetary database
# for both the summer and the fall sessions. Merging both these databases would
# create missing values in the sum_monetary column because players playing may or 
# may not do transactions. We replace the NAs in this column by zero signifying
# no transaction made. Keeping all.x=True because we want all the players from
# the sessions database as these are active players. This gives us a recency,frequency
# and monetary database. We make frequency and recency transactions as zero wherever
# they are NAs
history_combined_sess_trx <- merge(history_summer_sessions,history_summer_trx,all.x=TRUE)
history_combined_sess_trx$sum_monetary[is.na(history_combined_sess_trx$sum_monetary)]=0
history_combined_sess_trx$frequency_transactions[is.na(history_combined_sess_trx$frequency_transactions)]=0
history_combined_sess_trx$recency_transactions <- as.numeric(history_combined_sess_trx$recency_transactions)
history_combined_sess_trx$recency_transactions[is.na(history_combined_sess_trx$recency_transactions)]=0
history_combined_sess_trx$frequency_sessions <- as.numeric(history_combined_sess_trx$frequency_sessions)
history_combined_sess_trx$recency_sessions <- as.numeric(history_combined_sess_trx$recency_sessions)

forecast_combined_sess_trx <- merge(forecast_fall_sessions,forecast_fall_trx,all.x=TRUE)
forecast_combined_sess_trx$sum_monetary[is.na(forecast_combined_sess_trx$sum_monetary)]=0
forecast_combined_sess_trx$frequency_transactions[is.na(forecast_combined_sess_trx$frequency_transactions)]=0
forecast_combined_sess_trx$recency_transactions <- as.numeric(forecast_combined_sess_trx$recency_transactions)
forecast_combined_sess_trx$recency_transactions[is.na(forecast_combined_sess_trx$recency_transactions)]=0
forecast_combined_sess_trx$frequency_sessions <- as.numeric(forecast_combined_sess_trx$frequency_sessions)
forecast_combined_sess_trx$recency_sessions <- as.numeric(forecast_combined_sess_trx$recency_sessions)
# Combining fall and summer databases from above to create a combined database of sessions 
# and monetary data
all_combined_sess_trx <- rbind(history_combined_sess_trx,forecast_combined_sess_trx)

# Merging the above two tables for fall and summer with the customer data information
# gives us a basetable at the unique user level for both the summer and fall sessions
# Keeping all the customers from the sessions and monetary table as it has all the 
# active users. This database contains recency, frequecy, monetary and demographic
# information
history_basetable <- merge(history_combined_sess_trx,customerdata,all.x=TRUE)
forecast_basetable <- merge(forecast_combined_sess_trx,customerdata,all.x=TRUE)

acquisition <- 0
t <- 12/3 # 1 year/3
d <- 0.027 # cost of capital
r <- 0.13 # churn rate is 0.87 as calculated in the end hence our retention rate is 0.13

# margin is the sum_monetary variable in the history_basetable
# CLV function
calc_clv <- function(margin,r,d,acquisition,t){
  
  clv <- acquisition
  for (i in 0:t){
    
    clv <- clv + ((r^i)*margin/(1+d)^i)
    
  }
  
  return(clv)
}

clv<-rep(0,nrow(history_basetable))#set a column to zero by default


history_basetable$clv<-apply(history_basetable[,c("frequency_sessions","sum_monetary")],1,function(x) calc_clv(x[2],0.13,0.027,0,4))
summary(history_basetable$clv)# Avg clv is 4.543

# Creating a frequency segment variable for sessions and microtransactions so as to divide the data
# into various segments of frequency. segm.freq.sess refers to frequency
# of the sessions played and segm.freq.trans refers to the the no. of microtransactions done at the user level
# for history and forecast data. segments for frequency sessions are:-
# Between 0 and 2 sessions per day,2 and 4 sessions per day,Between 4 and 6 sessions per day,
# Between 6 and 8 sessions per day,Between 8 and 10 sessions per day,Greater than 10 sessions.
# Segments for frequency transactions are:-
#>5 payments,4-5 payments,2-4 payments,0-2 payments
# We create the frequency segment variable with the frequency_function_sessions_transactions

frequency_function_sessions_transactions <- function(data){

data <-data%>%mutate(segm.freq.sess=ifelse(between(frequency_sessions,0,2),'0-2 sessions',ifelse(between(frequency_sessions,2,4),'2-4 sessions',
                                ifelse(between(frequency_sessions,4,6),'4-6 sessions',ifelse(between(frequency_sessions,6,8),"6-8 sessions",
                                ifelse(between(frequency_sessions,8,10),"8-10 sessions",'>10 sessions'))))))

data$segm.freq.sess <- factor(data$segm.freq.sess,levels=c('>10 sessions','8-10 sessions',
                                              '6-8 sessions','4-6 sessions',
                                              '2-4 sessions','0-2 sessions'))

data <-data%>%mutate(segm.freq.trans=ifelse(between(frequency_transactions,0,2),'0-2 payments',ifelse(between(frequency_transactions,2,4),'2-4 payments',
                                        ifelse(between(frequency_transactions,4,5),'4-5 payments',">5 payments"))))
    
data$segm.freq.trans <- factor(data$segm.freq.trans,levels=c('>5 payments','4-5 payments',"2-4 payments","0-2 payments"))
                                                               
    
  
return(data)
}

# Creating a recency segment variable for sessions and microtransactions so as to divide the data
# into various segments of recency. segm.rec.sess refers to recency 
# of the sessions played and seg.rec.trans refers to the recency of the microtransactions from the enddate
# for history and forecast data. segments for recency_sessions are:-
# 0-10 days,11-30 days,31-45 days,46-60 days,61-190 days,91-120 days,>120 days.
#segments for recency_transactions are:-
# 0-10 days,11-30 days,31-45 days,46-60 days,61-190 days,91-120 days,>120 days.
# We create the recency segment variable with the recency_function_sessions_transactions
recency_function_sessions_transactions <- function(data){

 

data <- data%>%mutate(segm.rec.sess=ifelse(between(recency_sessions,0,10),'0-10 days',ifelse(between(recency_sessions,11,30),'11-30 days',
                            ifelse(between(recency_sessions,31,45),'31-45 days',ifelse(between(recency_sessions,46,60),'46-60 days',
                            ifelse(between(recency_sessions,61,90),'61-90 days',ifelse(between(recency_sessions,91,120),'91-120 days','>120 days')))))))

data$segm.rec.sess <- factor(data$segm.rec.sess,levels=c('>120 days','91-120 days','61-90 days','46-60 days','31-45 days','11-30 days','0-10 days'))

  data <- data%>%mutate(segm.rec.trans=ifelse(between(recency_transactions,1,10),'0-10 days',ifelse(between(recency_transactions,11,30),'11-30 days',
                                       ifelse(between(recency_transactions,31,45),'31-45 days',ifelse(between(recency_transactions,46,60),'46-60 days',
                                      ifelse(between(recency_transactions,61,90),'61-90 days',ifelse(between(recency_transactions,91,120),'91-120 days',
                                      ifelse(recency_transactions==0,"Never Paid",">120 days"))))))))
    
  data$segm.rec.trans <- factor(data$segm.rec.trans,levels=c("Never Paid",'>120 days','91-120 days','61-90 days','46-60 days','31-45 days','11-30 days','0-10 days'))
  
  
return(data)

    

}


# Creating recency and frequency segments with frequency_function_sessions_transactions and recency_function_sessions_transactions functions
# for history_basetable(summer_basetable).This create frequency and recency segments for sessions and microtransactions
history_combined_sess_trx <- frequency_function_sessions_transactions(history_combined_sess_trx)
history_combined_sess_trx <- recency_function_sessions_transactions(history_combined_sess_trx)
# Creating a cohort based on the first time a user played the game.
# So for the whole summer session we make cohorts combining the week number
# and month number. So if a player plays the game for the first time at week 1 and 
# month 7 we have the cohort Month 7 week 1.So for four weeks and four months we 
# 16 cohorts on the first session date. This will help in seeing of the evolution
# of these cohorts in the lifecycle grids. We make cohorts using the cohort_formation_function
cohort_formation_function <- function(data){
data <- data%>%mutate(cohort_firstplay=ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(1:7))),paste('Month',cohort_month,'week',1,sep=" "),
                                     ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(8:15))),paste('Month',cohort_month,'week',2,sep=" "),
                                     ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(16:23))),paste('Month',cohort_month,'week',3,sep=" "),
                                     ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(24:31))),paste('Month',cohort_month,'week',4,sep=" "),'None')))))


return(data)
}

# We make cohorts for the history database which includes sessions and monetary inforamtion
# This is not the basetable we use to make cohorts
history_combined_sess_trx <- cohort_formation_function(history_combined_sess_trx)

# This is a grouping function to create groups for different
# combination of variables in order to study the respective lifecycle grids.
# For example if we use the group age and gender it shows the lifecycle grids
# and the purchasing patterns for each combination of age and gender
grouping_function <- function(data,...){
  
  group <- enquos(...)
  data <- data%>%
    group_by(!!!group)%>%
    summarise(total_count=n(),total_monetary_value=sum(sum_monetary),
              total_duration=sum(sum_duration))%>%
    ungroup()%>%
    
    mutate(av.monetary_value=round(total_monetary_value/total_count,2),
           av.duration=round(total_duration/total_count,2))
  
  return(data)
  
  
}


# Visualization_trans function created to help in visualizing lifecycle
# grids for different features or combination of features in terms of frequency 
# and recency segments for microtransactions(segm.freq.trans and segm.rec.trans)
#Visualisation_sess function created to help in visualising lifecycle grids
# for different features or combination of features in terms of frequency and 
# recency segments for sessions played(segm.freq.sess and segm.rec.sess)
visualisation_trans <- function(data,a,fill,b){
  ggplot(data,aes(x=a,fill=fill,y=b))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_bar(stat="identity",alpha=0.5)+
  geom_text(aes(y=b,label=round(b,0)),size=2,position = position_dodge(width = 2),
            inherit.aes = TRUE)+
  facet_grid(segm.freq.trans~segm.rec.trans)+
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain"))
}
library(ggrepel)
visualisation_sess<- function(data,a,fill,b){
  ggplot(data,aes(x=a,fill=fill,y=b))+
    theme_bw()+
    theme(panel.grid=element_blank())+
    geom_bar(stat="identity",alpha=0.5)+
    geom_text(aes(y=b,label=round(b,0)),size=2,position = position_dodge(width = 2),
              inherit.aes = TRUE)+
    facet_grid(segm.freq.sess~segm.rec.sess)+
    theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain"))
}

# Creating frequency segments, recency segments, and cohorts based on first play date
# using the functions created above. This is done for both history and forecast basetable
history_basetable <- frequency_function_sessions_transactions(history_basetable)
history_basetable <- recency_function_sessions_transactions(history_basetable)
history_basetable <- cohort_formation_function(history_basetable)
forecast_basetable <- frequency_function_sessions_transactions(forecast_basetable)
forecast_basetable <- recency_function_sessions_transactions(forecast_basetable)
forecast_basetable <- cohort_formation_function(forecast_basetable)

# Creating another cohort based on the year the players registered
# Creating a variable client to use it in the lifecycle grid
history_basetable<- history_basetable%>%mutate(cohort_registration_year=year(Registrationdate))
history_basetable <- history_basetable%>%mutate(client="client")
# Lifecycle databases for combination of frequency and recency transactions
# and combination of frequency and recency sessions
lcg.trans<- grouping_function(history_basetable,segm.freq.trans,segm.rec.trans)
lcg.sess<- grouping_function(history_basetable,segm.freq.sess,segm.rec.sess)
lcg.trans <- lcg.trans%>%mutate(client="client")
lcg.sess <- lcg.sess%>%mutate(client="client")
# creating the lifecycle grids taking total users, avg_monetary_value and avg_duration as the metrics
# and seeing the evolution of these metrics for the sessions and microtransactions databases
a <- visualisation_sess(lcg.sess,lcg.sess$client,lcg.sess$total_count,lcg.sess$total_count)
a$labels$x <- "client"
a$labels$y <- "total_count"
a

b <- visualisation_trans(lcg.trans,lcg.trans$client,lcg.trans$total_count,lcg.trans$total_count)
b$labels$x <- "client"
b$labels$y <- "total_count"
b

# Seeing avg monetary value for the sessions lifecycle grid. This gives avg monetary value earned
# per each combination of frequency and rececy segment for sessions.
c <- visualisation_sess(lcg.sess,lcg.sess$client,lcg.sess$av.monetary_value,lcg.sess$av.monetary_value)
c$labels$x <- "client"
c$labels$y <- "av.monetary_value"
c

# Seeing avg duration value for the transactions lifecycle grid. This gives avg duration value
# per each combination of frequency and rececy segment for transactions.For example we get the
# play duration for a combination of 4-5 payments frequency and 0-10 days recency for transactions  
d <- visualisation_trans(lcg.trans,lcg.trans$client,lcg.trans$av.monetary_value,lcg.trans$av.duration)
d$labels$x <- "client"
d$labels$y <- "av.duration"
d

# Creating the age_group_gender_playertype function to create various age buckets,
# player types like social raiders,catchers etc. and gender types of male and female
age_group_gender_playertype_function <- function(data){
  data%>%mutate(age_group=ifelse(Age<=15,'0-15 age group',ifelse(Age<=25,'16-25 group',
                                                                 ifelse(Age<=40,'26-40 age group',ifelse(Age<=50,'41-50 age group','Above 50')))),
                playertype=ifelse(CustomerType==1,"walker",ifelse(CustomerType==2,"miscellaneous",
                                                                  ifelse(CustomerType==3,"social_raider","catcher"))),
                
                gender=ifelse(Sex==0,"Male","Female"))
  
}

# Adding the age_group,playertype and gender variables to the history and forecast basetables using the age_group_gender_playertypr function
history_basetable <- age_group_gender_playertype_function(history_basetable)
forecast_basetable <- age_group_gender_playertype_function(forecast_basetable)
#Databases containing groups of gender, frequency and recency
# for both transactions and sessions database
lcg.sex.trans <- grouping_function(history_basetable,gender,segm.freq.trans,segm.rec.trans)
lcg.sex.sess <- grouping_function(history_basetable,gender,segm.freq.sess,segm.rec.sess)

e <- visualisation_trans(lcg.sex.trans,lcg.sex.trans$gender,lcg.sex.trans$gender,lcg.sex.trans$total_count)
e$labels$x <- "gender"
e$labels$y <- "total_count"
e

f <- visualisation_sess(lcg.sex.sess,lcg.sex.sess$gender,lcg.sex.sess$gender,lcg.sex.sess$total_count)
f$labels$x <- "gender"
f$labels$y <- "total_count"
f

# Lifecycle grids for combination of gender and frequency and recency transactions and choosing 
# avg duration as the metric.For eg it shows how much a male who pays 2-4 times and has
# recency of 0-10 days for transactions plays on average
g <- visualisation_trans(lcg.sex.trans,lcg.sex.trans$gender,lcg.sex.trans$gender,lcg.sex.trans$av.duration)
g$labels$x <- "gender"
g$labels$y <- "av.duration"
g

# Lifecycle grids for combination of gender and frequency and recency sessions and choosing 
# avg monetary value as the metric. For eg it shows how much a male who plays 2-4 times and has
# recency of 0-10 days for sessions spends on a microtransaction on average
h <- visualisation_sess(lcg.sex.sess,lcg.sex.sess$gender,lcg.sex.sess$gender,lcg.sex.sess$av.monetary_value)
h$labels$x <- "gender"
h$labels$y <- "av.monetary_value"
h

#Databases containing groups of customertypes, frequency and recency
# for both transactions and sessions database
lcg.custtype.trans <- grouping_function(history_basetable,playertype,segm.freq.trans,segm.rec.trans)
lcg.custtype.sess <- grouping_function(history_basetable,playertype,segm.freq.sess,segm.rec.sess)

i <- visualisation_trans(lcg.custtype.trans,lcg.custtype.trans$playertype,lcg.custtype.trans$playertype,lcg.custtype.trans$total_count)
i$labels$x <- "playertype"
i$labels$y <- "total_count"
i

j <- visualisation_sess(lcg.custtype.sess,lcg.custtype.sess$playertype,lcg.custtype.sess$playertype,lcg.custtype.sess$total_count)
j$labels$x <- "playertype"
j$labels$y <- "total_count"
j

# Lifecycle grids for combination of customertypes and frequency and recency transactions and choosing 
# avg duration as the metric. For eg it shows how much a male who pays 2-4 times and has
# recency of 0-10 days for transactions plays on average
k <- visualisation_trans(lcg.custtype.trans,lcg.custtype.trans$playertype,lcg.custtype.trans$playertype,lcg.custtype.trans$av.duration)
k$labels$x <- "playertype"
k$labels$y <- "av.duration"
k

# Lifecycle grids for combination of customertypes and frequency and recency transactions and choosing 
# total monetary value as the metric. For eg it shows how much a male who plays 2-4 times and has
# recency of 0-10 days for sessions spends on a microtransaction on total
l <- visualisation_sess(lcg.custtype.sess,lcg.custtype.sess$playertype,lcg.custtype.sess$playertype,lcg.custtype.sess$total_monetary_value)
l$labels$x <- "playertype"
l$labels$y <- "total.monetary_value"
l
# Creating lifecycle databases for the different combinationg of age,gender 
# and recency and frequency segments for both transactions and sessions databases
lcg.age.gender.trans <- grouping_function(history_basetable,age_group,gender,segm.freq.trans,segm.rec.trans)
lcg.age.gender.sess <- grouping_function(history_basetable,age_group,gender,segm.freq.sess,segm.rec.sess)
m <- visualisation_trans(lcg.age.gender.trans,lcg.age.gender.trans$age_group,lcg.age.gender.trans$gender,lcg.age.gender.trans$total_count) #can play with othe rmetrics than quantity by monetary for example
m$labels$x <- "age_group"
m$labels$y <- "total_count"
m

n <- visualisation_sess(lcg.age.gender.sess,lcg.age.gender.sess$age_group,lcg.age.gender.sess$gender,lcg.age.gender.sess$total_count)
n$labels$x <- "age_group"
n$labels$y <- "total_count"
n

# Lifecycle grids for combination of gender,age_group and frequency and recency transactions and choosing 
# avg duration as the metric. For eg it shows how much a male in one particular age who pays 2-4 times and has
# recency of 0-10 days for transactions plays on average
o <- visualisation_trans(lcg.age.gender.trans,lcg.age.gender.trans$age_group,lcg.age.gender.trans$gender,lcg.age.gender.trans$av.duration)
o$labels$x <- "age_group"
o$labels$y <- "av.duration"
o

# Lifecycle grids for combination of gender,age_group and frequency and recency sessions and choosing 
# avg monetary value as the metric. For eg it shows how much a male in one particular age group who plays 2-4 times and has
# recency of 0-10 days for sessions spends on a microtransaction on average
p <- visualisation_sess(lcg.age.gender.sess,lcg.age.gender.sess$age_group,lcg.age.gender.sess$gender,lcg.age.gender.sess$av.monetary_value)
p$labels$x <- "age_group"
p$labels$y <- "av.monetary_value"
p

# Creating lifecycle database for the different years of registration and frequency
# and recency segments for both sessions and transactions
lcg.registrationyear.sess <- grouping_function(history_basetable,cohort_registration_year,segm.rec.sess,segm.freq.sess)
lcg.registrationyear.trans <- grouping_function(history_basetable,cohort_registration_year,segm.rec.trans,segm.freq.trans)
q <- visualisation_sess(lcg.registrationyear.sess,lcg.registrationyear.sess$cohort_registration_year,lcg.registrationyear.sess$cohort_registration_year,lcg.registrationyear.sess$total_count)
q$labels$x <- "cohort_registration_year"
q$labels$y <- "total_count"
q

r <- visualisation_trans(lcg.registrationyear.trans,lcg.registrationyear.trans$cohort_registration_year,lcg.registrationyear.trans$cohort_registration_year,lcg.registrationyear.trans$total_count)
r$labels$x <- "cohort_registration_year"
r$labels$y <- "total_count"
r

# Lifecycle grids for combination of registration_year and frequency and recency sessions and choosing 
# avg monetary value as the metric. For eg it shows how much a male who plays 2-4 times and has
# recency of 0-10 days for sessions spends on a microtransaction on average
s1 <- visualisation_sess(lcg.registrationyear.sess,lcg.registrationyear.sess$cohort_registration_year,lcg.registrationyear.sess$cohort_registration_year,lcg.registrationyear.sess$av.monetary_value)
s1$labels$x <- "cohort_registration_year"
s1$labels$y <- "av.monetary_value"
s1

# Lifecycle grids for combination of registration_year and frequency and recency sessions and choosing 
# avg monetary value as the metric. For eg it shows how much a male who plays 2-4 times and has
# recency of 0-10 days for sessions spends on a microtransaction on total
s2 <- visualisation_sess(lcg.registrationyear.sess,lcg.registrationyear.sess$cohort_registration_year,lcg.registrationyear.sess$cohort_registration_year,lcg.registrationyear.sess$total_monetary_value)
s2$labels$x <- "cohort_registration_year"
s2$labels$y <- "total.monetary_value"
s2
# Lifecycle grids for combination of registration_year and frequency and recency transactions and choosing 
# avg duration as the metric. For eg it shows how much a male who pays 2-4 times and has
# recency of 0-10 days for transactions plays on average
t <- visualisation_trans(lcg.registrationyear.trans,lcg.registrationyear.trans$cohort_registration_year,lcg.registrationyear.trans$cohort_registration_year,lcg.registrationyear.trans$av.duration)
t$labels$x <- "cohort_registration_year"
t$labels$y <- "av.duration"
t

# Creating a new variable using combination of frequency segments, recency segments and age group segments
# This variable is just a new way to segment our data and see if they can prove to be useful
# We do this for our history basetable
history_basetable <- history_basetable%>%mutate(

  freq.segm.type=ifelse(segm.freq.sess %in% c("0-2 sessions","2-4 sessions"),
                        "Low-Moderate Users",ifelse(segm.freq.sess %in% c("4-6 sessions","6-8 sessions"),
                                                    "Medium-Heavy Users","Very-heavy-users")),
  
  rec.segm.type=ifelse(segm.rec.sess %in% c("0-10 days","11-30 days"),"Recent users",ifelse(segm.rec.sess %in% c("31-45 days","46-60 days"),
                        "Not so recent users","Lost users")),
  
  age.segm.type=ifelse(age_group %in% c("0-15 age group","16-25 age group"),"Young people",
                       ifelse(age_group %in% c("26-40 age group","41-50 age group"),"Adults",
                       "Old people")),
  
  new_segment=interaction(age.segm.type,rec.segm.type)
  
)

# Creating a lifecycle database for the new segment created above and 
# creating for both sessions and transactions databases
lcg.new_segment.sess <- grouping_function(history_basetable,new_segment,segm.freq.sess,segm.rec.sess)
lcg.new_segment.trans <- grouping_function(history_basetable,new_segment,segm.freq.trans,segm.rec.trans)
u <- visualisation_sess(lcg.new_segment.sess,lcg.new_segment.sess$new_segment,lcg.new_segment.sess$new_segment,lcg.new_segment.sess$total_count)
u$labels$x <- "new_segment"
u$labels$y <- "total_count"
u

v <- visualisation_trans(lcg.new_segment.trans,lcg.new_segment.trans$new_segment,lcg.new_segment.trans$new_segment,lcg.new_segment.trans$total_count)
v$labels$x <- "new_segment"
v$labels$y <- "total_count"
v


# Creating a new variable using combination of frequency segments, recency segments and age group segments
# This variable is just a new way to segment our data and see if they can prove to be useful
# We do this for our forecast basetable
forecast_basetable <- forecast_basetable%>%mutate(
  
  freq.segm.type=ifelse(segm.freq.sess %in% c("0-2 sessions","2-4 sessions"),
                        "Low-Moderate Users",ifelse(segm.freq.sess %in% c("4-6 sessions","6-8 sessions"),
                                                    "Medium-Heavy Users","Very-heavy-users")),
  
  rec.segm.type=ifelse(segm.rec.sess %in% c("0-10 days","11-30 days"),"Recent users",ifelse(segm.rec.sess %in% c("31-45 days","46-60 days"),
                                                                                       "Not so recent users","Lost users")),
  
  age.segm.type=ifelse(age_group %in% c("0-15 age group","16-25 age group"),"Young people",
                       ifelse(age_group %in% c("26-40 age group","41-50 age group"),"Adults",
                              "Old people")),
  
  new_segment=interaction(age.segm.type,rec.segm.type)
  
)

# Creating our churn variable by combining the sessions table for summer and transactions table
# for fall. Churn for us is if a player is active in the summer and does not do microtransactions in the
# fall then the player has churned. The churn variable we obtain from merging sessions and transaction table
# is joined back to the history basetable to see which players churned in the fall.
merged_summer_sess_fall_trx <- merge(history_summer_sessions,forecast_fall_trx,by="CustomerID",all.x=TRUE)
merged_summer_sess_fall_trx <- merged_summer_sess_fall_trx%>%mutate(churn=ifelse(is.na(sum_monetary),1,0))
data_retrieved <- merged_summer_sess_fall_trx[,c("CustomerID","churn")]
history_basetable <- merge(history_basetable,data_retrieved,by="CustomerID",all.x=TRUE)

# This is our avg churn rate around 87 percent and this is why in the beginning we took 13 percent as our retention rate 
mean(history_basetable$churn)

View(history_basetable)



# Using correlation matrix to drop numerical variables having high correlation with each other
history_basetable_numeric <- history_basetable%>%select(frequency_sessions,recency_sessions,frequency_transactions,recency_transactions,
                                                        sum_pokestops,sum_raids,sum_gyms,sum_pokemons,sum_distance,sum_duration,sum_monetary,clv,
                                                        Age)


library(corrplot)
corrplot(cor(history_basetable_numeric),method="number",number.cex=0.60,number.digits = 1)

history_basetable_without_numeric <- history_basetable%>%select(-c(sum_gyms,sum_duration,sum_pokemons,sum_pokestops,clv,
                                                                   CustomerID,freq.segm.type,rec.segm.type,
                                                                   age.segm.type,new_segment,client))

# Model building using glm function
full_model2 <- glm(churn ~. , family=binomial(link='logit'),data=history_basetable_without_numeric)
full_model3 <- glm(churn ~1,family=binomial(link='logit'),data=history_basetable_without_numeric)

# Model building using step function based on AIC criterion 
modeltrainingfull_3<-step(full_model3, scope=list(lower=full_model3, upper=full_model2),direction="forward")
modeltrainingfull_4<-step(full_model3,scope=list(lower=full_model3, upper=full_model2), direction="both")


# Selecting modeltrainingfull_4 as our final model and noting down the important variables 
summary(modeltrainingfull_4)

# Function to build graphs between different combination of categorical variables.
# For example building between churn and gender to find the quantity of churners in 
# each gender
grouping_factors <- function(data,vector){
  list1=vector("list",length(vector)) 
  list2=vector("list",length(vector)) 
  for (i in c(1:length(list1)))
  {
    data_new <- data%>%select(c("churn",vector[i]))
    colnames(data_new)[2]="quan"
    list1[[i]] <- data_new%>%group_by(across())%>%summarise(w=n())
    list2[[i]] <- ggplot(list1[[i]],aes(x=quan,y=w,fill=churn))+geom_bar(stat="identity")
  } 
  
  return(list2)
  
  
  
}


l=grouping_factors(history_basetable,c("Sex","fallbonus","segm.freq","segm.rec","age_group",
                                       "rec.segm.type","age.segm.type","freq.segm.type","new_segment"))

# Function to test if two categorical variables are dependent or not
chisqtestfunction<-function(data,vector){
  p_values_list=rep(0,length(vector))
  for(i in c(1:length(vector))){
    test <- chisq.test(table(data[,c(vector[i])],data[,c("churn")]),simulate.p.value = TRUE)
    p_values_list[i] <- test$p.value
    
    
  }
  names(p_values_list) <-c("Sex","fallbonus","segm.freq","segm.rec","age_group",
                           "rec.segm.type","age.segm.type","freq.segm.type","new_segment") 
  return(p_values_list)
  
}

p_values_list <- chisqtestfunction(history_basetable,c("Sex","fallbonus","segm.freq","segm.rec","age_group",
                                                       "rec.segm.type","age.segm.type","freq.segm.type","new_segment"))





