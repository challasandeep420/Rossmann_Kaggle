train <- read.csv('train.csv')
test <- read.csv('test.csv')
store <- read.csv('store.csv')
states <- read.csv('store_states.csv')

# extracting month,week and year
train=dateparts(train)
test=dateparts(test)


#merging with store,states data
data=merge(train,store,by='Store',all.x=T)
test=merge(test,store,by='Store',all.x=T)


#data=merge(data,states,by='Store',all.x=T)
#test=merge(test,states,by='Store',all.x=T)

# adding new features
data=new.feature.promo(data)
data=new.feature.comp(data)
data=new.feature.sale(data)
#wt=new.feature.weather(data)

test=new.feature.promo(test)
test=new.feature.comp(test)
test$trend=0.6
    
# there are some days in which the shop is open, but no sales.
# maybe due to repairs or some test run in case of new shops
# would be ideal to remove these entries- 54 entries
data=data[!(data$Sales==0 & data$Open==1),]
data=data[data$Sales>0,]

# adding new features for counting weeks after most recent promo2 reset
data.lm <- data
test.lm <- test
data.lm$Weeks.After.Promo2=apply(data.lm,1,new.feature.weeks.after.promo.lm)
test.lm$Weeks.After.Promo2=apply(test.lm,1,new.feature.weeks.after.promo.lm)

# function to select variables and also convert to factors for linear regression

var_sel_factors.lm <- function ()
    {
        vars_factor=read.csv('variables_lm.csv')
        rownames(vars_factor)=vars_factor[,'var']
        var.sel=intersect(colnames(data.lm),as.character(vars_factor[vars_factor$inc=='yes','var']))        
        data.lm<<-data.lm[,var.sel]
        var.sel=var.sel[!(var.sel %in% c('Sales'))]
        var.sel=c(var.sel,'Id')
        test.lm<<-test.lm[,var.sel]
        cols=intersect(colnames(data.lm),colnames(test.lm))
        print (cols)
        for (i in 1:length(cols))
            {
            if (vars_factor[cols[i],'is_factor']=='yes')
               {
                levels=unique(c(as.vector(unique(data.lm[,cols[i]])),as.vector(unique(test.lm[,cols[i]]))))
                data.lm[cols[i]]<<-factor(data.lm[,cols[i]],levels=levels)
                test.lm[cols[i]]<<-factor(test.lm[,cols[i]],levels=levels)
            }
            else
                {
                data.lm[cols[i]]<<-as.numeric(data.lm[,cols[i]])
                test.lm[cols[i]]<<-as.numeric(test.lm[,cols[i]])
                 }
                
        }
        
        
        return(0)
    }
b=var_sel_factors.lm()

# counting weeks after the most recent promo2 reset
data$Weeks.After.Promo2=apply(data,1,new.feature.weeks.after.promo)
test$Weeks.After.Promo2=apply(test,1,new.feature.weeks.after.promo)

# function to select variables and also convert to factors

var_sel_factors <- function ()
    {
        vars_factor=read.csv('variables.csv')
        rownames(vars_factor)=vars_factor[,'var']
        var.sel=intersect(colnames(data),as.character(vars_factor[vars_factor$inc=='yes','var']))        
        data<<-data[,var.sel]
        var.sel=var.sel[!(var.sel %in% c('Sales'))]
        var.sel=c(var.sel,'Id')
        test<<-test[,var.sel]
        cols=intersect(colnames(data),colnames(test))
        print (cols)
        for (i in 1:length(cols))
            {
            if (vars_factor[cols[i],'is_factor']=='yes')
               {
                levels=unique(c(as.vector(unique(data[,cols[i]])),as.vector(unique(test[,cols[i]]))))
                data[cols[i]]<<-factor(data[,cols[i]],levels=levels)
                test[cols[i]]<<-factor(test[,cols[i]],levels=levels)
            }
            else
                {
                data[cols[i]]<<-as.numeric(data[,cols[i]])
                test[cols[i]]<<-as.numeric(test[,cols[i]])
                 }
                
        }
        
        
        return(0)
    }

# variable selection and factorization training and test data
a=var_sel_factors()

data$log.Sales<-log1p(data$Sales)
data.lm$log.Sales <- log1p(data.lm$Sales)
data<-data.matrix(data)
test<-data.matrix(test)
