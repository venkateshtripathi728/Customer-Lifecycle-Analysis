#Question 1
#a
setwd('C:/Users/Clémentine/Documents/EDHEC/M2/Customer Analytics/PokemonGo')
# Importing sessions database and formatting date for summer season
summersess<-read.csv('C:/Users/venkatesh/Documents/CRM/summersesstrx.csv')
summersess$Date <- as.Date(summersess$Date,format = "%Y-%m-%d")
# Importing micro-transactions database and formatting date for summer season
summerfin<-read.csv('C:/Users/venkatesh/Documents/CRM/summerfintrx.csv')
summerfin$Date <- as.Date(summerfin$Date,format = "%Y-%m-%d")
# Importing sessions database and formatting date for fall season
fallsess <- read.csv('C:/Users/venkatesh/Documents/CRM/fallsesstrx.csv')
fallsess$Date <- as.Date(fallsess$Date,format = "%Y-%m-%d")
# Importing micro-transactions database and formatting date for fall season
fallfin <- read.csv('C:/Users/venkatesh/Documents/CRM/fallfintrx.csv')
fallfin$Date <- as.Date(fallfin$Date,format = "%Y-%m-%d")
# Database containing information like demography,age and gender at unique customer level
customerdata<-read.csv('C:/Users/venkatesh/Documents/CRM/customerdata.csv')
# Importing required libraries for data manipulation and data visualization
library(dplyr)
library(lubridate)
library(ggplot2)
library(corrgram)

basetable<-merge(customerdata,summersess,by="CustomerID",all.y = TRUE)

basetable$Registrationdate<-as.Date(basetable$Registrationdate)
basetable$Date<-as.Date(basetable$Date)
any(is.na(basetable))
library(Amelia)
missmap(basetable,col = c("yellow","red"))
#No NAs

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
# covered by a unique user
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
history_summer_sessions <- createtablesession(summersess,startdatehistory,enddatehistory)  
forecast_fall_sessions <- createtablesession(fallsess,startdateforecast,enddateforecast)


# Creating a monetary value database at the unique user level so as to find out
# the total revenues from the microtransactions involved.Microtransactions are 
# important as the company wants to develop the insights into the patterns of 
# in game purchases by users. We create this database with the createtablefintransactions
# function
createtablefintransactions <- function(data){
  
  data <-data%>%
  mutate(monetaryvalue=ifelse(ProductID==1,2.99,ifelse(ProductID==2,4.99,ifelse(ProductID==3,9.99,ifelse(ProductID==4,25,ifelse(ProductID==5,99,0))))))
  data_grouped <- data%>%group_by(CustomerID)%>%summarise(sum_monetary=sum(monetaryvalue))

}

# Creating two monetary databases for both summer and fall database
history_summer_trx <- createtablefintransactions(summerfin)
forecast_fall_trx <- createtablefintransactions(fallfin)

# Merging recency,frequency and sessions database with the monetary database
# for both the summer and the fall sessions. Merging both these databases would
# create missing values in the sum_monetary column because players playing may or 
# may not do transactions. We replace the NAs in this column by zero signifying
# no transaction made. Keeping all.x=True because we want all the players from
# the sessions database as these are active players. This gives us a recency,frequency
# and monetary database
history_combined_sess_trx <- merge(history_summer_sessions,history_summer_trx,all.x=TRUE)
history_combined_sess_trx$sum_monetary[is.na(history_combined_sess_trx$sum_monetary)]=0

forecast_combined_sess_trx <- merge(forecast_fall_sessions,forecast_fall_trx,all.x=TRUE)
forecast_combined_sess_trx$sum_monetary[is.na(forecast_combined_sess_trx$sum_monetary)]=0

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



#retained_or_not <- rep(0,nrow(history_basetable))
#history_basetable <- cbind(history_basetable,retained_or_not)



#history_basetable[history_basetable$CustomerID %in% forecast_basetable$CustomerID,]$retained_or_not <- 1



#avgretentionrate <- sum(history_basetable$retained_or_not)/nrow(history_basetable)
#str(history_basetable)


calc_clv <- function(margin,r,d,acquisition,t){
  
  clv <- acquisition
  for (i in c(0:t)){
    
    clv <- clv + ((r^i)*margin/(1+d)^i)
    
  }
  
  return(clv)
}




calc_clv_pred <- function(margin,d,acquisition,t,model,freq){
  
  clv <- acquisition
  r <- predict(model,newdata=data.frame(sum_monetary=margin, frequency=freq),type='response')[1]
  
  for (i in c(0:t)){
    
    clv <- clv+((r^i)*margin/(1+d)^i)
    
    
  }
  
  
 return(clv) 
}

acquisition <- 0
t <- 30 # in days
d <- 0.20 # cost of capital

# Creating a frequency segment variable so as to divide the data
# into various segments of frequency. Frequency refers to frequency
# of the sessions played or the no. of sessions played at the user level
# for history and forecast data. segments for frequency are:-
# Between 1 and 2 sessions per day,2 and 4 sessions per day,Between 4 and 6 sessions per day,
# Between 6 and 8 sessions per day,Between 8 and 10 sessions per day,Greater than 10 sessions.
# We create the frequency segment variable with the frequency_function

frequency_function <- function(data){
data <- data%>%mutate(segm.freq=ifelse(between(frequency,1,2),'Between 1 and 2 sessions per day',ifelse(between(frequency,2,4),'Between 2 and 4 sessions per day',
                                ifelse(between(frequency,4,6),'Between 4 and 6 sessions per day',ifelse(between(frequency,6,8),"Between 6 and 8 sessions per day",
                                ifelse(between(frequency,8,10),"Between 8 and 10 sessions per day",'Greater than 10 sessions'))))))

data$segm.freq <- factor(data$segm.freq,levels=c('Greater than 10 sessions','Between 8 and 10 sessions per day',
                                              'Between 6 and 8 sessions per day','Between 4 and 6 sessions per day',
                                              'Between 2 and 4 sessions per day','Between 1 and 2 sessions per day'))

return(data)
}

# Creating a recency segment variable so as to divide the data
# into various segments of recency. Recency refers to recency 
# of the sessions played or the last session played from the enddate
# for history and forecast data. segments for recency are:-
# 0-10 days,11-30 days,31-45 days,46-60 days,61-190 days,91-120 days.
# We create the recency segment variable with the recency_function
recency_function <- function(data){
data <- data%>%mutate(segm.rec=ifelse(between(recency,0,10),'0-10 days',ifelse(between(recency,11,30),'11-30 days',
                                                                ifelse(between(recency,31,45),'31-45 days',ifelse(between(recency,46,60),'46-60 days',
                                                                ifelse(between(recency,61,90),'61-90 days',ifelse(between(recency,91,120),'91-120 days','None')))))))

data$segm.rec <- factor(data$segm.rec,levels=c('None','91-120 days','61-90 days','46-60 days','31-45 days','11-30 days','0-10 days'))

return(data)
}

# Creating recency and frequency segments with frequency and recency functions
# for history_basetable(summer_basetable)
history_combined_sess_trx <- frequency_function(history_combined_sess_trx)
history_combined_sess_trx <- recency_function(history_combined_sess_trx)

# Creating a cohort based on the first time a user played the game.
# So for the whole summer session we make cohorts combining the week number
# and month number. So if a player plays the game for the first time at week 1 and 
# month 7 we have the cohort Month 7 week 1.So for four weeks and four months we 
# 16 cohorts on the first session date. This will help in seeing of the evolution
# of these cohorts in the lifecycle grids. We make cohorts using the cohort_formation_function
cohort_formation_function <- function(data){
data <- data%>%mutate(cohort_revised=ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(1:7))),paste('Month',cohort_month,'week',1,sep=" "),
                                     ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(8:15))),paste('Month',cohort_month,'week',2,sep=" "),
                                     ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(16:23))),paste('Month',cohort_month,'week',3,sep=" "),
                                     ifelse((cohort_month%in%c(5,6,7,8) & (cohort_date %in% c(24:31))),paste('Month',cohort_month,'week',4,sep=" "),'None')))))

return(data)
}

# We make cohorts for the history database which includes sessions and monetary inforamtion
# This is not the basetable
history_combined_sess_trx <- cohort_formation_function(history_combined_sess_trx)

# This is a grouping function to create groups for different
# combination of variables in order to study the respective lifecycle grids.
# For example if we use the group age and gender it shows the lifecycle grids
# and the purchasing patterns for each combination of age and gender
grouping_function <- function(data,...){
  
  group <- enquos(...)
  data <- data%>%
    group_by(!!!group)%>%
    summarise(quantity=n(),total_monetary_value=sum(sum_monetary),
              total_duration=sum(sum_duration))%>%
    ungroup()%>%
    
    mutate(av.monetary_value=round(total_monetary_value/quantity,2),
           av.duration=round(total_duration/quantity,2))
  
  return(data)
  
  
}

# Lifecycle grid database for the cohorts made above
lcg.coh <- grouping_function(history_combined_sess_trx,cohort_revised,segm.rec,segm.freq)

lcg.coh_revised <- lcg.coh%>%filter(segm.rec!='None')

# Visualization function created to help in visualizing lifecycle
# grids for different features or combination of features
visualisation <- function(data,a,fill,b){
  ggplot(data,aes(x=a,fill=fill,y=b))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_bar(stat="identity",alpha=0.5)+
  geom_text(aes(y=b,label=round(b,0)),size=2,position = position_dodge(width = 2),
            inherit.aes = TRUE)+
  facet_grid(segm.freq~segm.rec)+
  xlab("First interaction Cohort")+
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain"))
}

# creating the lifecycle grids taking total users, total_monetary_value and avg_duration as the metrics
# and seeing the evolution of these metrics for the different cohorts
visualisation(lcg.coh_revised,lcg.coh_revised$cohort_revised,lcg.coh_revised$cohort_revised,lcg.coh_revised$quantity)
visualisation(lcg.coh_revised,lcg.coh_revised$cohort_revised,lcg.coh_revised$cohort_revised,lcg.coh_revised$total_monetary_value)
visualisation(lcg.coh_revised,lcg.coh_revised$cohort_revised,lcg.coh_revised$cohort_revised,lcg.coh_revised$avg_duration)

# Creating frequency segments, recency segments, and cohorts based on first play date
# using the functions created above. This is done for both history and forecast basetable
history_basetable <- frequency_function(history_basetable)
history_basetable <- recency_function(history_basetable)
history_basetable <- cohort_formation_function(history_basetable)
forecast_basetable <- frequency_function(forecast_basetable)
forecast_basetable <- recency_function(forecast_basetable)
forecast_basetable <- cohort_formation_function(forecast_basetable)

# Lifecycle database for every combination of cohort and gender taking no. of users as the metric to study
lcg.coh.gender <- grouping_function(history_basetable,cohort_revised,Sex,segm.freq,segm.rec)
lcg.coh.gender <- lcg.coh.gender%>%filter(segm.rec!='None')

# Visualizing the above as a lifecycle grid and seeing the evolution of the no. of users for each combination
# of gender and cohort
visualisation(lcg.coh.gender,lcg.coh.gender$cohort_revised,lcg.coh.gender$Sex,lcg.coh.gender$quantity)

# Creating the age_group function to create various age buckets
age_group_function <- function(data){
  data%>%mutate(age_group=ifelse(Age<=15,'0-15 age group',ifelse(Age<=25,'16-25 group',
                          ifelse(Age<=40,'26-40 age group',ifelse(Age<=50,'41-50 age group','Above 50')))))
  
}

# Adding the age_group variable to the history and forecast basetables using the age_group_function
history_basetable <- age_group_function(history_basetable)
forecast_basetable <- age_group_function(forecast_basetable)

# Creating lifecycle database for the different age segments chosing no. of users as the metric
# and then visualizing this lifecycle grid
lcg.age <-grouping_function(history_basetable,age_group,segm.rec,segm.freq)
visualisation(lcg.age,lcg.age$age_group,lcg.age$age_group,lcg.age$quantity)

# Creating lifecycle database for the different genders choosing no. of users as the metric
# and then visualizing this lifecycle grid
lcg.sex <- grouping_function(history_basetable,Sex,segm.rec,segm.freq)
visualisation(lcg.sex,lcg.sex$Sex,lcg.sex$Sex,lcg.sex$quantity)

# Creating lifecycle database for the different combinationg of age and gender choosing no. of users as the metric
# and then visualizing this lifecycle grid
lcg.age.gender <- grouping_function(history_basetable,age_group,Sex,segm.freq,segm.rec)
visualisation(lcg.age.gender,lcg.age.gender$age_group,lcg.age.gender$Sex,lcg.age.gender$quantity)


# Creating a new variable using combination of frequency segments, recency segments and age group segments
# This variable is just a new way to segment our data and see if they can prove to be useful
# We do this for our history basetable
history_basetable <- history_basetable%>%mutate(

  freq.segm.type=ifelse(segm.freq %in% c("Between 1 and 2 sessions per day","Between 2 and 4 sessions per day"),
                        "Low-Moderate Users",ifelse(segm.freq %in% c("Between 4 and 6 sessions per day","Between 6 and 8 sessions per day"),
                                                    "Medium-Heavy Users","Very-heavy-users")),
  
  rec.segm.type=ifelse(segm.rec %in% c("0-10 days","11-30 days"),"Recent users",ifelse(segm.rec %in% c("31-45 days","46-60 days"),
                        "Not so recent users","Lost users")),
  
  age.segm.type=ifelse(age_group %in% c("0-15 age group","16-25 age group"),"Young people",
                       ifelse(age_group %in% c("26-40 age group","41-50 age group"),"Adults",
                       "Old people")),
  
  new_segment=interaction(age.segm.type,rec.segm.type)
  
)

# Creating a lifecycle database for the new segment created above and using no. of users as the metric
# see the evolution of this metric for the different segments inside a lifecycle grid
lcg.new_segment <- grouping_function(history_basetable,new_segment,segm.freq,segm.rec)
visualisation(lcg.new_segment,lcg.new_segment$new_segment,lcg.new_segment$new_segment,lcg.new_segment$quantity)

# Creating a new variable using combination of frequency segments, recency segments and age group segments
# This variable is just a new way to segment our data and see if they can prove to be useful
# We do this for our forecast basetable
forecast_basetable <- forecast_basetable%>%mutate(
  
  freq.segm.type=ifelse(segm.freq %in% c("Between 1 and 2 sessions per day","Between 2 and 4 sessions per day"),
                        "Low-Moderate Users",ifelse(segm.freq %in% c("Between 4 and 6 sessions per day","Between 6 and 8 sessions per day"),
                                                    "Medium-Heavy Users","Very-heavy-users")),
  
  rec.segm.type=ifelse(segm.rec %in% c("0-10 days","11-30 days"),"Recent users",ifelse(segm.rec %in% c("31-45 days","46-60 days"),
                                                                                       "Not so recent users","Lost users")),
  
  age.segm.type=ifelse(age_group %in% c("0-15 age group","16-25 age group"),"Young people",
                       ifelse(age_group %in% c("26-40 age group","41-50 age group"),"Adults",
                              "Old people")),
  
  new_segment=interaction(age.segm.type,rec.segm.type)
  
)



total_combined_table <- rbind(history_basetable,forecast_basetable)
history_summer_sessions
forecast_fall_trx

# Creating our churn variable by combining the sessions table for summer and transactions table
# for fall. Churn for us is if a player is active in the summer and does not do microtransactions in the
# fall then the player has churned. The churn variable we obtain from mergin sessions and transaction table
# is joined back to the history basetable to see which players churned in the fall.
merged_summer_sess_fall_trx <- merge(history_summer_sessions,forecast_fall_trx,by="CustomerID",all.x=TRUE)
merged_summer_sess_fall_trx <- merged_summer_sess_fall_trx%>%mutate(churn=ifelse(is.na(sum_monetary),1,0))
data_retrieved <- merged_summer_sess_fall_trx[,c("CustomerID","churn")]
history_basetable <- merge(history_basetable,data_retrieved,by="CustomerID",all.x=TRUE)
mean(history_basetable$churn)
View(history_basetable)



history_basetable$recency <- as.numeric(history_basetable$recency)
history_basetable_numeric <- history_basetable%>%select(frequency,recency,sum_pokestops,sum_raids,sum_gyms,sum_pokemons,sum_distance,sum_duration,sum_monetary)

install.packages("corrplot")
library(corrplot)
corrplot(cor(history_basetable_numeric),method="number")

history_basetable_without_numeric <- history_basetable%>%select(-c(sum_gyms,sum_duration,sum_pokemons,sum_pokestops,
                                                                   CustomerID,Registrationdate,freq.segm.type,rec.segm.type,
                                                                   age.segm.type,new_segment,Age))
full_model2 <- glm(churn ~. , family=binomial(link='logit'),data=history_basetable_without_numeric)
full_model3 <- glm(churn ~1,family=binomial(link='logit'),data=history_basetable_without_numeric)

modeltrainingfull_2<-step(full_model2, direction="backward")
modeltrainingfull_3<-step(full_model3, scope=list(lower=full_model3, upper=full_model2),direction="forward")
modeltrainingfull_4<-step(full_model2, direction="both")
modeltrainingfull_5<-step(full_model3,scope=list(lower=full_model3, upper=full_model2), direction="both")

str(history_basetable)



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


history_basetable_without_numeric_factors <- history_basetable%>%select(-c(sum_gyms,sum_duration,sum_pokemons,sum_pokestops,
                                                                   CustomerID,Registrationdate,Sex,age_group,age.segm.type))


full_model5 <- glm(churn ~. , family=binomial(link='logit'),data=history_basetable_without_numeric_factors)
full_model6 <- glm(churn ~1 , family=binomial(link='logit'),data=history_basetable_without_numeric_factors)
modeltrainingfull_6 <- step(full_model5,direction = "backward")
modeltrainingfull_7 <- step(full_model5,direction = "both")
modeltrainingfull_8 <- step(full_model6,scope=list(lower=full_model6, upper=full_model5),direction="forward")
modeltrainingfull_9 <- step(full_model6,scope=list(lower=full_model6, upper=full_model5),direction="both")

model_list1 <- list(modeltrainingfull_6,modeltrainingfull_7,modeltrainingfull_8,modeltrainingfull_9)
model_list1

ROCInfonew(history_basetable_without_numeric_factors,model_list1,"churn")
model_list1[[4]]

history_basetable_without_numeric_factors$churn <- history_basetable_without_numeric_factors$churn%>%as.factor()

history_basetable_without_numeric_factors$churn%>%class()
