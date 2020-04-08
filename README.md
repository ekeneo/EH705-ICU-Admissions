# EH705-ICU-Admissions
EH705 Term Project. Data analysis for ICU Admissions.

## Detection of outliers of Int Variables 
```{r}
par(mfrow=c(1,3))
boxplot(ICU$Age, main="Boxplot of Age")
boxplot(ICU$Systolic, main="Boxplot of Systolic Blood Pressure")
boxplot(ICU$HeartRate, main="Boxplot of Heart Rate")
```
As we can see above, the outliers are present in systolic blood pressure and heart rate. To see the values of the outliers, please see below, where the first row includes the outliers for systolic blood pressure, and the second includes outliers for heart rate.
```{r}
systolic_outlier<-boxplot.stats(ICU$Systolic)
heartrate_outlier<-boxplot.stats(ICU$HeartRate)
systolic_outlier$out
heartrate_outlier$out
```


## Crosstabs (relations between categorical variables)
```{r}
sjt.xtab(ICU$Status, ICU$Sex, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Race, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Service, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Cancer, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Renal, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Infection, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$CPR, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Previous, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Type, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Fracture, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$PO2, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$PH, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$PCO2, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Bicarbonate, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Creatinine, show.col.prc = TRUE)
sjt.xtab(ICU$Status, ICU$Consciousness, show.col.prc = TRUE)
```
Statistically significant crosstabs include: Creatinine, Type, CPR, Infection,Renal, Service. 

## Correlation 
```{r}
pacman::p_load(Hmisc)
ICUnums<-select(ICU, Status, Age, HeartRate)
rcorr(as.matrix(ICU), type="pearson")
```
## Logistic Regression 
Prior to conducting a logistic regression, we would like to break down the systolic blood pressure into levels. We will use clinically defined levels for hypo, normal, elevated and hyper. Ranges below 90 are considered hypo, whereas ranges from 90 to 120 are considered normal, ranges between 120 and 129 are considered elevated, and 130 plus are considered hyper. 

```{r}
ICU$Systolic<-cut(ICU$Systolic, breaks=c(0,89,119,129,256))
levels(ICU$Systolic)<-c("hypo", "normal", "elevated", "hyper")
```

Below is the logistic regression model with all the predictors in the dataset.
As we can see, key predictors that are shown to be statistically significant are age, cancer, systolic pressure, fracture, arterial blood gas concentration, type, and consciousness. We have some confidence in our model due to the small difference between the null and residual deviance. Some of our confidence intervals have zero in them, leading us to have some doubt in our model. 
```{r}
ICU.m<-glm(formula=Status~Age+Sex+Race+Service+Cancer+Renal+Infection+CPR+Systolic+HeartRate+Previous+Type+Fracture+PO2+PH+PCO2+Bicarbonate+Creatinine+Consciousness, family=binomial(logit), data=ICU)
summary(ICU.m)
confint(ICU.m)
exp(coef(ICU.m))
```










###Final model 

```{r}
ICU.final.m<-glm(formula=Status~Age+Cancer+Systolic+PCO2+Consciousness+Type, family=binomial(logit),data=ICU)
summary(ICU.final.m)
confint(ICU.final.m)
exp(coef(ICU.final.m))
```

```{r}
pacman::p_load(rsample)
set.seed(78)
train_test_split <- initial_split(ICU)
train <- training(train_test_split)
test <- testing(train_test_split)
(samp <- dim(train_test_split))
```

### Testing & training the model 
```{r}
predicted.val<-predict(ICU.tr, newdata=test)
head(predicted.val)

predicted.prob <- predict(ICU.tr, test, type = "response")
head( predict( ICU.tr, test, type="response") )

predicted.classes <- ifelse( predicted.prob > 0.5, "Lived", "Dead" )
head(predicted.classes)
```


```{r}
ICU.tr<-glm(Status~Age+Cancer+Systolic+PCO2+Consciousness+Type, family=binomial(logit),data=train)
summary(ICU.tr)
```

```{r}
pacman::p_load(car)
car::vif(ICU.tr)
```

```{r}
fit0<-glm(Status~1, family=binomial(logit), data=train)
summary(fit0)
```
```{r}
fitall<-glm(Status~Age+Cancer+Systolic+PCO2+Consciousness+Type, family=binomial(logit), data=train)
summary(fitall)
```
```{r}
anova(fitall, fit0, test="Chisq")
```

```{r}
pacman::p_load(survey)
regTermTest(ICU.tr, "Age")
regTermTest(ICU.tr, "Cancer")
regTermTest(ICU.tr, "Systolic")
regTermTest(ICU.tr, "Type")
regTermTest(ICU.tr, "PCO2")
regTermTest(ICU.tr, "Consciousness")
```

## Classification and Regression Trees 
```{r}
pacman::p_load(rpart, rpart.plot, rattle, dplyr)
set.seed(2715)

#final model 
icutree=rpart(Status~Age+Cancer+Systolic+Type+PCO2+Consciousness+Type+Fracture+Sex+Race+Service+Renal+Infection+CPR+HeartRate+Previous+PO2+PH+Bicarbonate+Creatinine+Consciousness, method="class", data=ICU)
#rpart.plot(icutree,
           #extra = 104, # show fitted class, probs, percentages
           #box.palette = "GnYlRd", # color scheme
           #prefix = "Status\n",
           #branch.lty = 1, # solid branch lines, 3 = dotted
           #shadow.col = "gray", # shadows under the node boxes
           #split.prefix = "is ", # put "is " before split text
           #split.suffix = "?", # put "?" after split text
           #nn = TRUE, # display the node numbers
           #tweak = 1.2)
fancyRpartPlot(icutree)
printcp(icutree)

predicted.classes <- icutree %>% predict(, data=icu, type = "class")
head(predicted.classes, 12)

mean(predicted.classes == ICU$Status)
plotcp(icutree)

```
