---
title: "Final Poject---H1B"
output: pdf_document
---
**1. Data_cleaning.R**
```{r}
data <- read.csv("H-1B_FY14_Q4.csv")
colnames(data)
## Select the variables we want.
rawdata <- data.frame(data$STATUS,data$LCA_CASE_SUBMIT,
              data$DECISION_DATE,data$VISA_CLASS,
              data$LCA_CASE_EMPLOYMENT_START_DATE,
              data$LCA_CASE_EMPLOYMENT_END_DATE,
              data$LCA_CASE_EMPLOYER_NAME,data$LCA_CASE_EMPLOYER_STATE,
              data$LCA_CASE_SOC_NAME,data$LCA_CASE_JOB_TITLE,
              data$LCA_CASE_WAGE_RATE_FROM,data$LCA_CASE_WAGE_RATE_TO,
              data$LCA_CASE_WAGE_RATE_UNIT,data$FULL_TIME_POS)
## Change the names of data frame to its original form.
colnames(rawdata) <- c("STATUS", "LCA_CASE_SUBMIT",
                       "DECISION_DATE", "VISA_CLASS",
                       "LCA_CASE_EMPLOYMENT_START_DATE",
                       "LCA_CASE_EMPLOYMENT_END_DATE",
                       "LCA_CASE_EMPLOYER_NAME", "LCA_CASE_EMPLOYER_STATE",
                       "LCA_CASE_SOC_NAME", "LCA_CASE_JOB_TITLE",
                       "LCA_CASE_WAGE_RATE_FROM", "LCA_CASE_WAGE_RATE_TO",
                       "LCA_CASE_WAGE_RATE_UNIT", "FULL_TIME_POS")

unique(rawdata$STATUS)
## Only keep the status we are concerned about like "CERTIFIED-WITHDRAWN",
## "CERTIFIED" and "DENIED" and make sure that visa class is H-1B.
status1 <- which(rawdata$STATUS=='CERTIFIED-WITHDRAWN'|rawdata$STATUS=='CERTIFIED')
status0 <- which(rawdata$STATUS=='DENIED')
newdata <- rawdata[sort(c(status0,status1)),]
newdata <- newdata[which(newdata$VISA_CLASS=='H-1B'),]

## Apply complete case analysis on missing data.
newdata <- newdata[!is.na(newdata[,"LCA_CASE_WAGE_RATE_FROM"]),]
wage_to <- which(!is.na(newdata[,"LCA_CASE_WAGE_RATE_TO"]))
newdata <- data.frame(newdata,LCA_CASE_WAGE_RATE_AVERAGE 
                      =newdata[,"LCA_CASE_WAGE_RATE_FROM"])
## Take the means of wage intervals as average wages.
newdata[wage_to,"LCA_CASE_WAGE_RATE_AVERAGE"] <- (newdata[wage_to,
"LCA_CASE_WAGE_RATE_FROM"]+newdata[wage_to,"LCA_CASE_WAGE_RATE_TO"])/2

unique(newdata[,"LCA_CASE_WAGE_RATE_UNIT"])
## Find out wage of other units except for year.
hour <- which(newdata[,"LCA_CASE_WAGE_RATE_UNIT"]=="Hour")
week <- which(newdata[,"LCA_CASE_WAGE_RATE_UNIT"]=="Week")
month <- which(newdata[,"LCA_CASE_WAGE_RATE_UNIT"]=="Month")
bi_weekly <- which(newdata[,"LCA_CASE_WAGE_RATE_UNIT"]=="Bi-Weekly")

newdata <- data.frame(newdata,LCA_CASE_WAGE_RATE_AVERAGE_YEAR 
                      =newdata[,"LCA_CASE_WAGE_RATE_AVERAGE"])
## Standardize the unit of wage into salary per year.
newdata[hour,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"] <- newdata[hour,
"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"]*8*20*12
newdata[week,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"] <- newdata[week,
"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"]*4*12
newdata[month,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"] <- newdata[month,
"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"]*12
newdata[bi_weekly,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"] <- newdata[bi_weekly,
"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"]*2*12
save(newdata, file = "d:/newdata.Rdata")
```




**2. Descriptive Statistics**\newline
```{r}
library(tm)
library(stringr)
load("/Users/YY/Downloads/newdata1.Rdata")

#Make a Corpus for counting word frequency in the job name column

#Turn the interested columns into a long vector of words  
all_text_SOC <- paste(newdata$LCA_CASE_SOC_NAME, collapse = " ")
all_text_JOB <- paste(newdata$LCA_CASE_JOB_TITLE, collapse = " ")
all_text_EMPR <- paste(newdata$LCA_CASE_EMPLOYER_NAME, collapse = " ")

#Turn into corpus
corpus_SOC <- Corpus(VectorSource(all_text_SOC))
corpus_JOB <- Corpus(VectorSource(all_text_JOB))
corpus_EMPR <- Corpus(VectorSource(all_text_EMPR))

#Clean the corpus
#lower case
corpus_SOC <- tm_map(corpus_SOC, content_transformer(tolower))
#remove punctuation
corpus_SOC <- tm_map(corpus_SOC, removePunctuation)
#remove numbers
corpus_SOC <- tm_map(corpus_SOC, removeNumbers)
#remove redundant white space
corpus_SOC <- tm_map(corpus_SOC, stripWhitespace)
#remove stopwords in English
corpus_SOC <- tm_map(corpus_SOC, removeWords,stopwords("en"))
###
#To ensure that all documents are encoded in UTF-8
#http://tm.r-forge.r-project.org/faq.html
corpus_JOB <-tm_map(corpus_JOB, content_transformer(function(x) iconv(enc2utf8(x),
                                                                      sub = "byte")))
corpus_JOB <- tm_map(corpus_JOB, content_transformer(tolower))
corpus_JOB <- tm_map(corpus_JOB, removePunctuation)
corpus_JOB <- tm_map(corpus_JOB, removeNumbers)
corpus_JOB <- tm_map(corpus_JOB, stripWhitespace)
corpus_JOB <- tm_map(corpus_JOB, removeWords,stopwords("en"))

corpus_EMPR <-tm_map(corpus_EMPR, content_transformer(function(x) iconv(enc2utf8(x),
                                                                      sub = "byte")))
corpus_EMPR <- tm_map(corpus_EMPR, content_transformer(tolower))
corpus_EMPR <- tm_map(corpus_EMPR, removePunctuation)
corpus_EMPR <- tm_map(corpus_EMPR, removeNumbers)
corpus_EMPR <- tm_map(corpus_EMPR, stripWhitespace)
corpus_EMPR <- tm_map(corpus_EMPR, removeWords,stopwords("en"))

#creating DTM for these corpuses
dtm_SOC <- as.matrix(DocumentTermMatrix(corpus_SOC))
dtm_JOB <- as.matrix(DocumentTermMatrix(corpus_JOB))
dtm_EMPR <- as.matrix(DocumentTermMatrix(corpus_EMPR))

#Count the word frequency
freq_SOC <- sort(colSums(dtm_SOC),decreasing = T)
freq_JOB <- sort(colSums(dtm_JOB),decreasing = T)
freq_EMPR <- sort(colSums(dtm_EMPR),decreasing = T)



cs_soc1 <- apply(as.matrix(newdata$LCA_CASE_SOC_NAME),1,function(x) str_detect(x,"Computer"))
cs_soc1_num <- as.numeric(cs_soc1)
cs_soc1_index <- which(cs_soc1_num=='1')

cs_soc2 <- apply(as.matrix(newdata$LCA_CASE_SOC_NAME),1,function(x) str_detect(x,"Systems"))
cs_soc2_num <- as.numeric(cs_soc2)
cs_soc2_index <- which(cs_soc2_num=='1')

cs_soc3 <- apply(as.matrix(newdata$LCA_CASE_SOC_NAME),1,function(x) str_detect(x,"Software"))
cs_soc3_num <- as.numeric(cs_soc3)
cs_soc3_index <- which(cs_soc3_num=='1')

cs_soc4 <- apply(as.matrix(newdata$LCA_CASE_SOC_NAME),1,function(x) str_detect(x,"Programmers"))
cs_soc4_num <- as.numeric(cs_soc4)
cs_soc4_index <- which(cs_soc4_num=='1')

cs <- unique(c(cs_soc1_index,cs_soc2_index,cs_soc3_index,cs_soc4_index))


fin_soc1 <- apply(as.matrix(newdata$LCA_CASE_SOC_NAME),1,function(x) str_detect(x,"Financial"))
fin_soc1_num <- as.numeric(fin_soc1)
fin <- which(fin_soc1_num=='1')


other <- c(1:length(newdata$LCA_CASE_SOC_NAME))[-(c(cs,fin))]

classification_soc <- 1:length(newdata$LCA_CASE_SOC_NAME)
classification_soc[cs] <- "cs"
classification_soc[fin] <- "finance"
classification_soc[other] <- "other"
newdata$classification_soc <- classification_soc



stu1 <- apply(as.matrix(newdata$LCA_CASE_EMPLOYER_NAME),1,function(x) str_detect(x,toupper("university")))
stu1_num <- as.numeric(stu1)
stu1_index <- which(stu1_num=='1')

stu2 <- apply(as.matrix(newdata$LCA_CASE_EMPLOYER_NAME),1,function(x) str_detect(x,toupper("health")))
stu2_num <- as.numeric(stu2)
stu2_index <- which(stu2_num=='1')

stu3 <- apply(as.matrix(newdata$LCA_CASE_EMPLOYER_NAME),1,function(x) str_detect(x,"INSTITUTES"))
stu3_num <- as.numeric(stu3)
stu3_index <- which(stu3_num=='1')

stu4 <- apply(as.matrix(newdata$LCA_CASE_EMPLOYER_NAME),1,function(x) str_detect(x,toupper("medical")))
stu4_num <- as.numeric(stu4)
stu4_index <- which(stu4_num=='1')

academic <- unique(c(stu1_index,stu2_index,stu3_index,stu4_index)) 

classification_academic <- 1:length(newdata$LCA_CASE_SOC_NAME)
classification_academic[academic] <- "academic"
classification_academic[-academic] <- "non-academic"
newdata$classification_academic <- classification_academic

northeast <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
midwest <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
south <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS",
           "TN","AR","LA","OK","TX")
west <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")
northeast_index <- which(as.numeric(newdata$LCA_CASE_EMPLOYER_STATE%in%northeast)
                         =="1")
midwest_index <- which(as.numeric(newdata$LCA_CASE_EMPLOYER_STATE%in%midwest)=="1")
south_index <- which(as.numeric(newdata$LCA_CASE_EMPLOYER_STATE%in%south)=="1")
west_index <- which(as.numeric(newdata$LCA_CASE_EMPLOYER_STATE%in%west)=="1")

classification_location <- 1:length(newdata$LCA_CASE_EMPLOYER_STATE)
classification_location[northeast_index] <- "northeast"
classification_location[midwest_index] <- "midwest"
classification_location[south_index] <- "south"
classification_location[west_index] <- "west"
newdata$classification_location <- classification_location

save(newdata, file = "d:/newdata.Rdata")




#Check the words with the biggest frequencies
head(freq_SOC,30)
head(freq_JOB,30)
head(freq_EMPR,30)

plot(freq_SOC,xlab = "Words Index", ylab = "Frequency", 
     main = "Word Frequency for Varible LCA_CASE_SOC_NAME")
plot(freq_JOB,xlab = "Words Index", ylab = "Frequency", 
     main = "Word Frequency for Varible LCA_CASE_JOB_TITLE")
plot(freq_EMPR,xlab = "Words Index", ylab = "Frequency", 
     main = "Word Frequency for Varible LCA_CASE_EMPLOYER_NAME")

#Describe statistics function
Describ_Stat=function(G){ 
suppressMessages(library(pastecs,quietly = T))
des <- round(stat.desc(G,basic=FALSE),4)
describe <- data.frame(t(des),
"Percentile_25th"=quantile(G,0.25),
"Percentile_75th"=quantile(G,0.75),
"Percentile_5th"=quantile(G,0.05),
"Percentile_95th"=quantile(G,0.95),
"Max"=max(G),
"Min"=min(G))
return(describe) 
}
```
```{r,echo=FALSE,results='asis'}
library(xtable)
xtabledata_SOC <- t(Describ_Stat(freq_SOC))
colnames(xtabledata_SOC) = "SOC"
xtabledata_JOB <- t(Describ_Stat(freq_JOB))
colnames(xtabledata_JOB) = "JOB"
print(xtable(cbind.data.frame(xtabledata_SOC,xtabledata_JOB),
             caption="SOC and JOB Word Frequency Comparision",digits = 2),comment=FALSE)
```



```{r}
#Location
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(magrittr))
GEO = data.frame(newdata$STATUS,newdata$LCA_CASE_EMPLOYER_STATE)
GEO$recode =  newdata$STATUS == "CERTIFIED" | newdata$STATUS == "CERTIFIED-WITHDRAWN"
head(GEO) 

GEO1_certi =aggregate(GEO$recode, by=list(Category=GEO$newdata.LCA_CASE_EMPLOYER_STATE),
                FUN=sum)
GEO2 =aggregate(GEO$newdata.LCA_CASE_EMPLOYER_STATE,
                by=list(Category=GEO$newdata.LCA_CASE_EMPLOYER_STATE),
                FUN=NROW)
GEO3 = data.frame("Category"=GEO1_certi[,1],"Certified"=GEO1_certi[,2],"Total"=GEO2[,2])
GEO3 = GEO3[-1,]
GEO3$prop = GEO3$Certified/GEO3$Total

H1B_heat = gvisGeoChart(GEO1_certi[-1,], locationvar='Category', colorvar = 'x',
                   options=list(region='US',
displayMode="regions", resolution="provinces",colorAxis="{colors:['yellow'
                   ,'red']}",backgroundColor="cornflowerblue",
                   width=700, height=430))
plot(H1B_heat)
```

```{r}
#Take the time difference
Time_dif = difftime(strptime(newdata$DECISION_DATE, format = "%Y/%m/%d"),
         strptime(newdata$LCA_CASE_SUBMIT, format = "%Y/%m/%d"),units="days")
barplot(as.numeric(Time_dif),xlab="number of days",ylab="frequency")
Describ_Stat(Time_dif)
#Substract month frequency
monthi = format(strptime(newdata$DECISION_DATE, format = "%Y/%m/%d"), "%b")
monthiusse =aggregate(monthi, by=list(Category=monthi),FUN=NROW)
monthiusse
par(cex.axis=0.9)
barplot(monthiusse$x,names.arg=monthiusse$Category)
```

```{r}
#Log the salary
salary = log(newdata$LCA_CASE_WAGE_RATE_AVERAGE_YEAR)
Describ_Stat(salary)
hist(salary)
```



**3.construct models (code_model.R)**
```{r}
require(monomvn)
require(glmnet)
require(truncnorm)
require(MASS)

# load data
load("newdata.Rdata")

# since some locations are not in the 50 states, we exclude them in our analysis 
ind <- which(newdata[,"classification_location"] %in% c("west", "south", "northeast", "midwest"))
newdata <- newdata[ind, ]

# select only certified and denied
indcd <- which(newdata[,"STATUS"] %in% c("DENIED", "CERTIFIED"))
newdata <- newdata[indcd,]



## predictors
# scale wage per year
wage.test <- log(newdata[,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"])
boxplot(wage.test)
abline(h = 15, col = 2)
indw <- which(wage.test > 15)
newdata <- newdata[-indw,]
boxplot(log(newdata[,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"]))
## y
status <- as.numeric(newdata[,"STATUS"] != "DENIED")

wage <- log(newdata[,"LCA_CASE_WAGE_RATE_AVERAGE_YEAR"])
# days between submmiting date and decision date
Decision_Date <- as.Date(newdata[,"DECISION_DATE"])
Submit_Date <- as.Date(newdata[,"LCA_CASE_SUBMIT"])
days <- scale(as.numeric(difftime(Decision_Date, Submit_Date, units = "days")))

# months of submit
months_sub <- as.factor(months(Submit_Date))
year_sub <- as.numeric(format(Submit_Date, "%Y"))
months_sub <- relevel(months_sub, "January")
company <- as.factor(newdata[,"classification_soc"])
company <- relevel(company, "other")
academic <- as.factor(newdata[,"classification_academic"])
academic <- relevel(academic, "non-academic")
location <- as.factor(newdata[,"classification_location"])
fulltime <- as.factor(newdata[,"FULL_TIME_POS"]=="Y")

## store data
data <- data.frame(status = status, wage = wage, days = days, 
                   months_sub = months_sub, company = company, 
                   academic = academic, location = location, fulltime = fulltime)

## Stratified sampling
# select rejected records
ind1 <- which(data[,"status"] == 0)
set.seed(123)
indr <- sample(ind1, size = (9/10)*length(ind1), replace = FALSE)
rejectdata <- data[indr,]
# sample certified records
ind2 <- which(data[,"status"] == 1)
ind3 <- sample(ind2, size = 0.5*length(indr), replace = FALSE)
selectdata <- data[ind3,]
data2 <- rbind(rejectdata, selectdata)


## test data
indr.test <- ind1[-which(indr %in% ind1)]
reject.test <- data[indr.test,]
ind.cert <- ind2[-which(ind3 %in% ind2)]
cert.index <- sample(ind.cert, size = 0.5*length(indr.test), replace = FALSE)
cert.test <- data[cert.index,]
data.test <- rbind(reject.test, cert.test)
X.test <- model.matrix(~wage + days + months_sub + company + academic + location + fulltime, data = data.test)
X1.test <- model.matrix(~wage + days + months_sub + company + academic + 
                     location + fulltime + wage*days + wage*months_sub + 
                     wage*company + wage*academic + wage*location + wage*fulltime + 
                     days*months_sub + days*company + days*academic + days*location + 
                     days*fulltime + months_sub*company + months_sub*academic + 
                     months_sub*location + months_sub*fulltime + company*academic + 
                     company*location + company*fulltime + academic*location + 
                     academic*fulltime + location*fulltime, data = data.test)


## use in Lasso
# without interaction
X <- model.matrix(~wage + days + months_sub + company + academic + location + fulltime, data = data2)
# x with interaction
X1 <- model.matrix(~wage + days + months_sub + company + academic + 
                     location + fulltime + wage*days + wage*months_sub + 
                     wage*company + wage*academic + wage*location + wage*fulltime + 
                     days*months_sub + days*company + days*academic + days*location + 
                     days*fulltime + months_sub*company + months_sub*academic + 
                     months_sub*location + months_sub*fulltime + company*academic + 
                     company*location + company*fulltime + academic*location + 
                     academic*fulltime + location*fulltime, data = data2)

## use in nBayesian
# transform all character value into number
data_1 <- data.frame(status = data2$status, wage=data2$wage, days=data2$days, 
                     months_sub = as.numeric(data2$months_sub),   
                     company = as.numeric(data2$company),
                     academic = as.numeric(data2$academic),
                     location = as.numeric(data2$location),
                     fulltime = as.numeric(data2$fulltime))

# univariate 
par(mfrow = c(3,4),mar = c(2,2,3,1))
for(i in 1:ncol(data_1)){
  hist(data_1[,i], main = names(data_1)[i], xlab = "")
}


### Freq ###
testset <- data2
x <- X[,-1]
x.test <- X.test[,-1]

## model
model <- glm(status~., data = testset, family = binomial)
summary(model)

# without interaction
backward <- step(model, direction = "backward")
summary(backward)
coef_F <- summary(backward)$coef[,1]
formula(backward)


interaction.plot(data2$months_sub, data2$location, data2$status)
interaction.plot(data2$academic, data2$company, data2$status)


# with interaction
x1 <- X1[,-1]
x1.test <- X1.test[,-1]
null <- glm(status~1, data = testset, family = binomial)
up <- glm(status~(.)^2, data = testset, family = binomial)
forward <- step(null, scope = list(lower = formula(null), upper = formula(up)), direction = "forward")
summary(forward)
coef_F_inter <- summary(forward)$coef[,1]
formula(forward)

# interaction full model
selection <- glm(formula(forward), data = testset, family = binomial)
summary(selection)

## predict 
testset2 <- data2
# Without interaction
pred <- predict(backward, newdata=testset2, type="response")
mis_F <- sum(round(pred) != testset2[,1])/length(pred)
# With interaction
pred <- predict(selection, newdata=testset2, type="response")
mis_F_inter <- sum(round(pred) != testset2[,1])/length(pred)

## Out of Sample testing
# Without interaction
pred <- predict(backward, newdata=data.test, type="response")
mis_F.test <- sum(round(pred) != data.test[,1])/length(pred)
# With interaction
pred <- predict(selection, newdata=data.test, type="response")
mis_F_inter.test <- sum(round(pred) != data.test[,1])/length(pred)


### Lasso ###
## model
# without interaction
cv.lasso <- cv.glmnet(x = x, y = data2$status, family = "binomial", standardize = FALSE)
lasso <- glmnet(x = x, y = data2$status, family = "binomial", standardize = FALSE, lambda = cv.lasso$lambda.min)
coef_lasso <- c(lasso$a0, as.vector(lasso$beta)) 
names(coef_lasso) <- c("(Intercept)", rownames(lasso$beta))

# with interaction
cv.lasso1 <- cv.glmnet(x = x1, y = data2$status, family = "binomial", standardize = FALSE)
lasso1 <- glmnet(x = x1, y = data2$status, family = "binomial", standardize = FALSE, lambda = cv.lasso$lambda.min)
coef_lasso1 <- c(lasso1$a0, as.vector(lasso1$beta)) 
names(coef_lasso1) <- c("(Intercept)", rownames(lasso1$beta))

## predict
# without interaction
pred <- 1*lasso$a0 + x%*%lasso$beta
prob <- exp(pred)/(1+exp(pred))
mis_L <- sum(round(prob) != data2$status)/length(data2$status)

# with interaction
pred <- 1*lasso1$a0 + x1%*%lasso1$beta
prob <- exp(pred)/(1+exp(pred))
mis_L_inter <- sum(round(prob) != data2$status)/length(data2$status)


## Out of Sample testing
# without interaction
pred <- 1*lasso$a0 + x.test%*%lasso$beta
prob <- exp(pred)/(1+exp(pred))
mis_L.test <- sum(round(prob) != data.test$status)/length(data.test$status)

# with interaction
pred <- 1*lasso1$a0 + x1.test%*%lasso1$beta
prob <- exp(pred)/(1+exp(pred))
mis_L_inter.test <- sum(round(prob) != data.test$status)/length(data.test$status)


### Bayesian ###
y = as.matrix(data_1[,1])
n = length(y)

# With interaction
# initial value
beta = rep(1, dim(X)[2])
ystar = rnorm(n, X %*% beta,1)
x_sum <- t(X) %*% X
Ip <- diag(dim(X)[2])

# Gibbs sampling
sn = 5000
GSample <- matrix(NA, sn, n + dim(X)[2])
GSample[1,] <- c(beta, ystar)
for(i in 2:sn){
  # y*
  a = b= rep(0,n)
  a[which(y==0)] <- -Inf
  b[which(y==1)] <- Inf
  ystar = rtruncnorm(n, a, b, as.matrix(X) %*% beta,1)
  # beta
  sigma = solve(x_sum + Ip)
  mu = sigma %*% colSums(ystar * X)
  beta = mvrnorm(1,mu,sigma)
  GSample[i,] <- c(beta, ystar)
  # if((i %/% 100) == (i/100)) print(i)
}

# trace plot
par(mfrow=c(5,4),mar=c(2,2,2,2),oma=c(0,0,2,0))
for(i in 2:dim(X)[2]){
  plot(GSample[1:1000,i],type="l", ylab = "", main = colnames(X)[i])
}
title(main = "Trace Plot", outer = TRUE)

# burn-in point
B = 100

# coefficient
beta_p <- apply(GSample[(B+1):sn,1:dim(X)[2]], 2, mean)
names(beta_p) <- colnames(X)
coef_B <- beta_p 

# Without interaction
# initial value
beta = rep(1, dim(X1)[2])
ystar = rnorm(n, X1 %*% beta,1)
X1_sum <- t(X1) %*% X1
Ip <- diag(dim(X1)[2])

# Gibbs sampling
sn = 5000
GSample <- matrix(NA, sn, n + dim(X1)[2])
GSample[1,] <- c(beta, ystar)
for(i in 2:sn){
  # y*
  a = b= rep(0,n)
  a[which(y==0)] <- -Inf
  b[which(y==1)] <- Inf
  ystar = rtruncnorm(n, a, b, as.matrix(X1) %*% beta,1)
  # beta
  sigma = solve(X1_sum + Ip)
  mu = sigma %*% colSums(ystar * X1)
  beta = mvrnorm(1,mu,sigma)
  GSample[i,] <- c(beta, ystar)
  # if((i %/% 100) == (i/100)) print(i)
}

# trace plot
par(mfrow=c(5,4),mar=c(2,2,2,2),oma=c(0,0,2,0))
for(i in 2:dim(X1)[2]){
  plot(GSample[1:1000,i],type="l", ylab = "", main = colnames(X1)[i])
}
title(main = "Trace Plot", outer = TRUE)

# burn-in point
B = 100

# coefficient
beta_p.inter <- apply(GSample[(B+1):sn,1:dim(X1)[2]], 2, mean)
names(beta_p.inter) <- colnames(X1)
coef_B.inter <- beta_p.inter


# misclassification
mis_B <- sum(as.numeric((X %*% beta_p) >0) !=y)/length(y)
mis_B.inter <- sum(as.numeric((X1 %*% beta_p.inter) >0) !=y)/length(y)

# Out of sample
mis_B.test <- sum(as.numeric((X.test %*% beta_p) >0) != data.test$status)/length(data.test$status)
mis_B.test.inter <- sum(as.numeric((X1.test %*% beta_p.inter) >0) != data.test$status)/length(data.test$status)



Coef_F <- rep(0, dim(X)[2])
names(Coef_F)<- colnames(X)
Coef_F[names(coef_F)] <- coef_F

Coef_lasso <- rep(0, dim(X)[2])
names(Coef_lasso)<- colnames(X)
Coef_lasso[names(coef_lasso)] <- coef_lasso

Coef_B <- rep(0, dim(X)[2])
names(Coef_B)<- colnames(X)
Coef_B[names(coef_B)] <- coef_B

# coef of Bayesian
library(xtable)

# comparison of coefficients for three methods
table1 <- cbind(Coef_F, Coef_lasso, Coef_B)
colnames(table1) <- c("Frequentist", "LASSO", "Bayesian")
library(xtable)
print(xtable(table1, digits = 4, caption = "Coefficients of All Methods"), comment = FALSE)

# exponential of coefficients
table2 <- exp(table1)
print(xtable(table2, digits = 4, caption = "Exponentiated Coefficients of All Methods"), comment = FALSE)


table4 <- matrix(c(mis_F,mis_L,mis_B,mis_F_inter,mis_L_inter,0), ncol = 3, byrow = TRUE)
colnames(table4) <- c("Frequentist", "LASSO", "Bayesian")
rownames(table4) <- c("Without Interaction", "With Interaction")
print(xtable(table4, digits = 4, caption = "Misclassification Rates for Out Data"), comment = FALSE)

table5 <- matrix(c(mis_F.test,mis_L.test,mis_B.test,mis_F_inter.test,mis_L_inter.test,0), ncol = 3, byrow = TRUE)
colnames(table5) <- c("Frequentist", "LASSO", "Bayesian")
rownames(table5) <- c("Without Interaction", "With Interaction")
print(xtable(table5, digits = 4, caption = "Misclassification Rates for Test Data"), comment = FALSE)


```









