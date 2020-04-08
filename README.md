# EH705-ICU-Admissions
EH705 Term Project. Data analysis for ICU Admissions.

# Statistical Analysis 

```{r echo=FALSE}
xray:: distributions(ICU)
```
Using the xray package, general trends in the dataset were identified. 

1) There are more male observations in the dataset than females.

2) Many of the admissions to the ICU were emergencies, with a about a quarter of admissions being elective. This could relate to elective surgical procedures where morbidity could have been high or complications occurred. It is unclear whether people would be preemptively admitted to the ICU for high-risk procedures of if these elective admissions could be thought of as unforeseen or emergencies in themselves.

3) Looking at Status, more than 75% of observations lived after their ICU admission. 

4) While the numbers are roughly even, slightly more procedures were surgical. 

5) More admitted patients had no chronic renal failure, previous admissions to the ICU, fracture, CPR or cancer when admitted. Most admitted patients had  PO2 above or equal to 60, blood pH above or equal to 7.25, PCO2 below or equal to 45, and creatinine below or equal to 2.

6) Most patients admitted were conscious, with slighly more patients being comatose than in a deep stupor if unconscious. 

7) Looking at histograms of the three numerical variables in the dataset, which were Age, Systolic and HeartRate, it could be guess that if any of the distributions were to be normal, they would be Systolic and HeartRate. Age is very obviously not normally distributed. Systolic looks like it is centered around 140, and the mean confirms this at 132 with an SD of 33. HeartRate seems to be centered around 100, and the mean is calculated to be about 99 with an SD of 26. 

## Relationships with Age 

The mean of Age, referring to the descriptive statistics for this attributes, is about 58 years. Visually, it can be seen that the distribution of Age has two peaks, one around 20 years of age and one around 70 years of age. The majority of subjects admitted to the ICU seem to be between 40-80 years old. 

```{r}
densityPlot( ~ Age, data=ICU, bw=bw.SJ,adjust=1, kernel=dnorm, 
  method="adaptive")
```

Overlaying Status and Age, density plots show that deaths after admission follow the larger Age peak that contains middle aged to elderly individuals. 
```{r}
densityPlot(Age~Status, data=ICU, bw=bw.SJ, adjust=1, kernel=dnorm, method="adaptive")
```

While the plot of Age and death follow the same pattern for older individuals, the association between Cancer and Age is less clear. There are three peaks for of Ages where ICU admissions involved cancer. These are around 20, 50 and 70 years of age. Given the relatively small number of individuals who had cancer involvement with their admission, and the variability in ages associated, cancer might not be the best predictor of ICU admission. 

```{r}
densityPlot(Age~Cancer, data=ICU,  bw=bw.SJ, adjust=1, kernel=dnorm,   method="adaptive")
```

ICU admissions that involved infection centered around 60-65 years of age. This associations is relatively clear visually looking at the plot below. 
```{r}
densityPlot(Age~Infection, data=ICU,  bw=bw.SJ, adjust=1, kernel=dnorm,   method="adaptive")
```

Another association that was explored was between Age and Consiousness. All three categories for Consciousness overlap with peaks between 50 and 75 years of age. There is a distinct peak at around 50 for deep stupor, indicating that this age might be associated with deep stupor when related to ICU admissions. However, deep stupor also has a secondary peak at around 75, which make it less clearly how this consciousness category relates to age in this dataset. 

```{r}
densityPlot(Age~Consciousness, data=ICU,  bw=bw.SJ, adjust=1, kernel=dnorm,   method="adaptive")
```

Both admission Types, elective and emergency, happen more frequently with advancing age. However, elective admissions happen more frequently with older age. This might suggest that older individuals are more likely to have complications during surgical or other procedures that would require admissions to an intensive care unit. 

```{r}
densityPlot(Age~Type, data=ICU,  bw=bw.SJ, adjust=1, kernel=dnorm,   method="adaptive")
```
## Relationships with HeartRate

Using a density estimate plot, the distribution of HeartRate was produced. The main peak here is at about 90 beats per minute. There appears to be secondary peak around 130 beats per minute. The mean for HeartRate calculated by Rcmdr is 99. Looking at the data, this mean might be said to not accurately portray the frequency of values for HearRate. 

```{r}
densityPlot( ~ HeartRate, data=ICU, 
  bw=bw.SJ, adjust=1, kernel=dnorm, 
  method="adaptive")
```

Both status categories have similar peaks when plotted on a density estimate plot of HeartRate. This indicates that there might not be a difference in the Status response category based on HeartRate. People who lived did seem to have a higher density of HeartRate values around 90 than those who died. A normal adult heart rate rangest between 60 and 100 beats per minute. This graph might suggest that people who lived more often had heartrates within this range, whereas those who died seemed more likely to have higher heartrates. This could inform further analysis or research. 

```{r}
densityPlot(HeartRate~Status, data=ICU, bw=bw.SJ, adjust=1, kernel=dnorm,method="adaptive")
```

## Relationships with Systolic

Below is a density plot of Systolic blood pressure in mmHg. A peak appears around 130-140mmHg. The peak is quite narrow and distinct compared to the density plots of Age and HeartRate, suggesting that the majority of individuals had blood pressure readings that are reflective of this plot. The mean for this attribute was 132mmHg, which seems to be more valid than the means of the other numeric variables, based on the distribution of the frequency of values. 

```{r}
densityPlot( ~ Systolic, data=ICU, 
  bw=bw.SJ, adjust=1, kernel=dnorm, 
  method="adaptive")
```

The distribution of Systolic values for those who died seem to be concentrated around 75 mmHg and 140mmHg. A normal Systolic blood pressure can vary widely but the American Heart Association states that blood pressure below 120mmHg is normal. However, excessively low blood pressure could be a result of bleeding, for example, and can result in insufficient blood flow to critical organs. Medications used to restore blood pressure are used when blood pressure becomes too low. This might explain the peak around 75mmHg in the death curve in the graph below. 

```{r}
densityPlot(Systolic~Status, data=ICU, bw=bw.SJ, adjust=1, kernel=dnorm,method="adaptive")
```

# Uniform distribution   

The distribution of the ICU attributes will be explored below. Many of the attributes chosen to be tested here relate to the history of the individuals in the data set or the circumstances of the admission, rather than lab tests on admission. It would be interesting to explore  Most of the categorical attributes in this dataset have two categories, making the degrees of freedom 1 for chi squared distribution. 

```{r echo=FALSE, fig.cap= "Theoretical chi square distribution with 1 degrees of freedom", out.width="70%" }
knitr::include_graphics("C:/Users/cathy/OneDrive/Desktop/eHealth 705 - Stats/eHealth 705 statkey chisq 1df.png")
```
For Categorical variables with two categories:    
_Null Hypothesis_: the attribute is distributed uniformally, the chi squared value does not exceed the hypothesized  value of 3.841 (p = 0.05)    
_Alternative Hypothesis_: the attribute is not distiributed uniformally, The chi squared value exceeds the hypothesized value of 3.841    

Chi squared test - Status:  

The chi squared test for Status returns a chi squared value above the null hypothesized value with a very small p value (<0.05), indicating that there is a very low risk of Type I error if the null hypothesis is rejected. It can be concluded that Status is not uniformally distributed and that statistically more individuals lived than died on admission to the ICU.  

```{r}
local({
  .Table <- with(ICU, table(Status))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.5,0.5) 
  chisq.test(.Table, p=.Probs)
})
```

Chi squared test - CPR:   

Similarly, the chi squared test for CPR returns a very high chi squared value at 151, and a low p value, allowing us to reject the null hypothesis that this attribute is uniformally distributed. More individuals did not have CPR than did before ICU admission. In fact, as seen from the table below, few individuals received CPR at all, which might make it a poor predictor of ICU outcomes.

```{r}
local({
  .Table <- with(ICU, table(CPR))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.5,0.5) 
  chisq.test(.Table, p=.Probs)
})
```

```{r}
table(ICU$CPR, ICU$Status)
```

Chi squared test - Infection:   

The chi squared value produced from this test is only slightly higher than the null hypothesized value, with a p value of 0.024, indicating a 2.4% chance of a Type I error if the null hypothesis is rejected. While the distribution can be concluded to not be uniform based on the threshold set with the null hypothesis, the uniformity of Infection among patients admitted to the ICU could be explored in further research. It should be noted that in the group of individuals who died, more had infections than not. 

```{r}
local({
  .Table <- with(ICU, table(Infection))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.5,0.5) 
  chisq.test(.Table, p=.Probs)
})
```

```{r}
table(ICU$Infection, ICU$Status)
```

Chi squared test - Previous:   

The chi squared value here exceeds the null hypothesized value with a p value much smaller than 0.05, allowing us to reject the null hypothesis that this attribute is uniformally distributed. More individuals in both status categories did not have a previous ICU admission. 

```{r}
local({
  .Table <- with(ICU, table(Previous))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.5,0.5) 
  chisq.test(.Table, p=.Probs)
})
```

Chi squared test - Sex:   

The chi squared value returned from this test is 11.52, which is not as far from the null hypothesized value as some of the other values generated from other tests. The value and p value of 0.00069 still indicated that the null hypothesis should be reject and that Sex is not uniformally distributed. There are more men in this dataset than females. 
```{r}
local({
  .Table <- with(ICU, table(Sex))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.5,0.5) 
  chisq.test(.Table, p=.Probs)
})
```
In both sex categories, despite the inequality in the number of observations in each group, there are still more individuals who lived than died. 
```{r}
with(ICU, Barplot(Sex, by=Status, 
  style="divided", legend.pos="above", 
  xlab="Sex", ylab="Frequency"))
```

Chi squared test - Type:

The distribution of Type can be concluded to be non-uniform as the null hypothesis should be rejected based on the chi squared value produced and low p value. As seen in exploratory data analysis, the there are more individuals who are had emergency admissions than elective admissions. This difference in distribution is statistically significant. Fewer individuals died when they were admitted to the ICU on an elective basis.

```{r}
local({
  .Table <- with(ICU, table(Type))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.5,0.5) 
  chisq.test(.Table, p=.Probs)
})
```

```{r}
with(ICU, Barplot(Type, by=Status, 
  style="divided", legend.pos="above", 
  xlab="Type", ylab="Frequency"))
```

For Categorical variables with three categories:    
_Null Hypothesis_: the attribute is distributed uniformally, the chi squared value does not exceed the hypothesized  value of 5.992 (p = 0.05)    
_Alternative Hypothesis_: the attribute is not distiributed uniformally, The chi squared value exceeds the hypothesized value of 5.992 (p=0.05)    

```{r echo=FALSE, fig.cap= "Theoretical chi square distribution with 2 degrees of freedom", out.width="70%" }
knitr::include_graphics("C:/Users/cathy/OneDrive/Desktop/eHealth 705 - Stats/eHealth 705 statkey chisq 2df.png")
```

Chi squared test - Consciousness

The null hypothesis for this attribute's distribution is that an equal number of observations will be found in each category of Consciousness. The chi squared value produced by this test exceeds the null hypothesized value with a low p value, indicating that the observations in this category are statistically not uniform. This could have been guessed from counts of observations in each category, where most individuals were in the conscious category. 
```{r}
local({
  .Table <- with(ICU, 
  table(Consciousness))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 
  2))
  .Probs <- c(0.333333333333333,
  0.333333333333333,0.333333333333333) 
  chisq.test(.Table, p=.Probs)
})
```

## Normal Distribution

Distribution of the three numerical variables in the ICU dataset was evaluated using quantile-quantile plots and the Shapiro-Wilk's test for normality. Confidence intervals will be produced for the means of each attribute.  

_Null Hypothesis_: Age, HeartRate and Systolic are normally distributed   
_Alternative Hypothesis_: Age, HeartRate and Systolic are not normally distributed 

Tests of normality - Age:     

Recall that a quantile-quantile plot should produce a nearly linear plot using dataset values, with the intercept going through zero, if the null hypothesis is satisfied. Below, the plotted points of Age do not conform well to the line of best fit, and are frequently outside of the confidence bands. This supports what might have been hypothesized when the histogram of Age was produced: this distribution is not clearly centered around a mean. This plot suggests that the null hypothesis that Age is normally distributed should be rejected. 
```{r}
with(ICU, qqPlot(Age, dist="norm", id=list(method="y", n=2, labels=rownames(ICU)), main="QQ plot of Age"))
```

Using the Shapiro-Wilk normality test, the conclusions drawn from the QQ plot can be supported, with a very low risk of Type I error, base on this p value. The Shapiro Wilk test is sensitive when used on larger datasets, so this result should be taken into account with the other tests used. 

```{r}
normalityTest(~Age, test="shapiro.test", data=ICU)
```

Using the t-test, given the confidence interval for this attribute is between 54.75 and 60.34, an interval that does not contain 0. (choosing not to add this as I do not have a test mean - need to refresh on what a true mean is )

Tests of normality - HeartRate: 

The QQ plot of HeartRate appears to adhere well to the line of best fit, despite several points in the middle of the graph lying along one side of the confidence band. Most of the points here fall within the confidence band. There is a positive skew of this data towards the lower values of HeartRate. Visually, one might conclude that this data is normally distributed. 
```{r}
with(ICU, qqPlot(HeartRate, dist="norm", id=list(method="y", n=2, labels=rownames(ICU)), main="QQ plot of HeartRate"))
```
The results of the Shapiro-Wilk test very narrowly allow one to reject the null hypothesis of normal distribution. However the p value is very close to the threshold p value of 0.05, which decreases the confidence that might be had to reject normality based on this test and the sample size used. 
```{r}
normalityTest(~HeartRate, test="shapiro.test", data=ICU)
```

Test of normality - Systolic:

Similar to the plot of HeartRate, the QQ plot of Systolic displays points that line closely on the line of best fit. The confidence bands here are quite narrow, and it is only along the ends oof the plot the points very clearly exit the confidence bands. One might conclude that the null hypothesis could be rejected for the normal distribution of Systolic based on this plot. There is a very slight negative skew here towards higher values of Systolic.   
```{r}
with(ICU, qqPlot(Systolic, dist="norm", id=list(method="y", n=2, labels=rownames(ICU)), main="QQ Plot of Systolic"))
```
While more statistically significant compared to the Shapiro Wilk test for normality of the HeartRate distribution, the p value here is not far from 0.05, indicating a 2% risk of Type I error. Based on this test, the null hypothesis could be rejected. The distribution of Systolic appears to be non-normal. 
```{r}
normalityTest(~Systolic, test="shapiro.test", data=ICU)
```

In reference to Ian Fellows' comments about tests for normality, these tests for normality do not add much more to understanding of the ICU dataset and the contributions of these three numerical variables to the determination of Status, beyond what is provided by the xray package.
