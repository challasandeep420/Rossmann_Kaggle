library(weatherData)

# FEATURE ENGINEERING

# extracting months, year and week no from train dataset
dateparts <- function (train)
    {
        train$Date=as.POSIXlt(train$Date)
        train$Week.no=as.numeric(strftime(train$Date,format='%W'))       
        train$Year=strftime(train$Date,format='%Y')
        train$Month=strftime(train$Date,format='%m')        
        first.week.month=as.numeric(strftime(as.POSIXlt(paste(train$Year,train$Month,'01',sep="-")),format="%W."))
        train$Week.Of.Month=train$Week.no-first.week.month+1
        train$Day=strftime(train$Date,format='%d')
        train['Date']=as.Date(train$Date)

         return(train)
    }


new.feature.promo <- function(data)
    {
#creating a new feature promo2.status indicating whether promo2 was running for the date
# Promo2 runs continously and gets renewed in the months specified by promo2 interval
# each run consists of 3 months and then gets renewed

        data$Promo2.Status=0
        data$Promo2.Status[(data['Year']>data['Promo2SinceYear']) | (data['Year']==data['Promo2SinceYear'] & data['Week.no']>data['Promo2SinceWeek'])]=1


# creating a new feature promo2.firstmonth indicating whether the date was in first month of the promo2 interval. 
# assumption is that whenever  a promo gets renewed people might be tempted to buy more,
#the rush could be only to the first month

        months=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept',
            'Oct','Nov','Dec')

        promo2.firstmonth <- function(x){
            if (x['Promo2']==1)
                {
                    if (grepl(months[as.numeric(x['Month'])],x['PromoInterval']))
                        return(1)
                    else
                        return(0)
                }
            return (0)
        }

        data$Promo2.FirstMonth=apply(data,1,promo2.firstmonth)

        return(data)
    }

new.feature.comp <- function(data)
    {
   ### a new feature to determine a competetitor was present at the time.
        data$Competitor.Present=0
        data$Competitor.Present[data$Date>as.Date(paste(data$CompetitionOpenSinceYear,data$CompetitionOpenSinceMonth,'01',sep="-"))]=1
        # if year is missing and distance is present keep the flag as 1
        data$Competitor.Present[!is.na(data$CompetitionDistance) & is.na(data$CompetitionOpenSinceYear)]=1
        # for the above condition giving year as 1900, making the year irrelevant
        data$CompetitionOpenSinceYear[is.na(data$CompetitionOpenSinceYear)]=1900
        data$CompetitionOpenSinceMonth[is.na(data$CompetitionOpenSinceMonth)]=1
   ### calculating weeks since opening
        data$Weeks.Since.Competitor=as.numeric(data$Date-as.Date(paste(data$CompetitionOpenSinceYear,data$CompetitionOpenSinceMonth,'01',sep="-")))/7
        # if competitior absent assigning high value for weeks
        data$Weeks.Since.Competitor[data$Competitor.Present==0]=999
        data$Weeks.Since.Competitor[data$Weeks.Since.Competitor<0]=999
   ### if distance missing, giving a high distance as value
        data$CompetitionDistance[is.na(data$CompetitionDistance)]=99999
        

        return(data)
    }

new.feature.sale <- function(data)
    {

        ## adding sales trends
        sales_yr=tapply(data$Sales[data$Month <8]/100,data$Year[data$Month <8],sum)
        data$trend=1
        data$trend[data$Year==2014]=sales_yr[2]/sales_yr[1]
        data$trend[data$Year==2015]=sales_yr[3]/sales_yr[1]

        return(data)
    }

new.feature.weather <- function(data.)
    {  codes <- read.csv('states_airport_codes.csv')
       print (codes)
       strt.dt=min(data.$Date)
       end.dt=max(data.$Date)
       print (strt.dt)
       print (end.dt)
       for (i in (1:nrow(codes)))
           { weather=getSummarizedWeather(codes[i,'IATA'],strt.dt,end.dt,
                                          opt_custom_columns=TRUE,
                                          custom_columns=c(3,9,18,20,21,22))
             weather$State=codes[i,'State']
             if (i==1)
                 weather.data=weather
             weather.data=rbind(weather.data,weather)
         }
       data.=merge(data.,weather.data,by=c('State','Date'),all.x=T)
       return (data.)
   }


months.list=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept',
            'Oct','Nov','Dec')
new.feature.weeks.after.promo.lm <- function(x)
   {
     if (x['PromoInterval']!='' & x['Promo2.Status']==1)
     {
        s=unlist(strsplit(x['PromoInterval'],","))
        w=vector()    
        for (i in 1:length(s))
            {  week=as.numeric(strftime(as.POSIXlt(paste(x['Year'],which(months.list==s[i]),'01',sep="-")),format="%W"))
               w=c(w,as.numeric(x['Week.no'])-week)
           }
        w[w<0]=w+53
        if (min(w) %in% c(0,4,5,9,11))
            return (1)
        else return(0)
    }
     else return(0)
 }

new.feature.weeks.after.promo <- function(x)
   { if (x['PromoInterval']!='' & x['Promo2.Status']==1)
     {
        s=unlist(strsplit(x['PromoInterval'],","))
        w=vector()    
        for (i in 1:length(s))
            {  week=as.numeric(strftime(as.POSIXlt(paste(x['Year'],which(months.list==s[i]),'01',sep="-")),format="%W"))
               w=c(w,as.numeric(x['Week.no'])-week)
           }
        w[w<0]=w+53
        return (min(w))
    }
     else return(-1)
 }

        


















