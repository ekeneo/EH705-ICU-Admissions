# EH705-ICU-Admissions
EH705 Term Project. Data analysis for ICU Admissions.
# Statistical Analysis 

```{r echo=FALSE}
xray:: distributions(icu)
```

## Distribution
```{r}
with(icu, qqPlot(Systolic, dist="norm", id=list(method="y", n=2, 
  labels=rownames(uci.rc)), main="Systolic"))
```

```{r}
normalityTest(~Systolic, test="shapiro.test", data=icu)
```

```{r}
with(icu, qqPlot(HeartRate, dist="norm", id=list(method="y", n=2, 
  labels=rownames(uci.rc)), main="Heartrate"))
```

```{r}
normalityTest(~HeartRate, test="shapiro.test", data=icu)
```

```{r}
with(icu, qqPlot(Age, dist="norm", id=list(method="y", n=2, 
  labels=rownames(uci.rc)), main="Age"))
```

```{r}
normalityTest(~Age, test="shapiro.test", data=icu)
```
