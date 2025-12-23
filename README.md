---
title: "Tongue Fat vs SPPB: Linear Regression Analysis"
author: "Ishaanee Roy"
date: "`r Sys.Date(`"
output:
  html_document:
    toc: true
    theme: united
    highlight: textmate
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r, echo=FALSE,message=FALSE,warning=FALSE}
library(tidyverse) #Always the first step
```

<h4 style="text-align: center;">Term Project Description</h4>
This document is a final project for the UCSD Course BILD 5.
My project builds upon the study referenced below. I have proposed a potential follow-up experiment to extend the study's findings. The project utilizes simulated data, structured and analyzed as if collected from a real experiment. While the outcomes do not provide scientific evidence, they showcase my skills in formulating and addressing relevant biological questions through statistics, experimental design, and programming.


<h4 style="text-align: center;">Article Citation</h4>

Ogawa, M., Satomi-Kobayashi, S., Yoshida, N., Tsuboi, Y., Komaki, K., Nanba, N., Izawa, K. P., Sakai, Y., Akashi, M., & Hirata, K. (2021). Relationship between oral health and physical frailty in patients with cardiovascular disease. Journal of Cardiology, 77(2), 131–138. https://doi.org/10.1016/j.jjcc.2020.07.016

<h4 style="text-align: center;">Research Question</h4>

Does tongue fat in individuals affect their Short Physical Performance Battery score?

<h4 style="text-align: center;">Null Hypothesis</h4>

There is no linear relationship between tongue fat and the Short Physical Performance Battery score (SPPBs) β1 = θ

<h4 style="text-align: center;">Alternate Hypothesis</h4>

There is a linear relationship between tongue fat and the Short Physical Performance Battery score (SPPBs)  β1 != θ

<h4 style="text-align: center;">Test Performed</h4>
Linear regression


:::: {style="display: grid; grid-template-columns: 1fr 1fr; grid-column-gap: 10px;"}

::: {0.5}

```{r Section2 instructions, include=FALSE}

#In the first code chunk, read in your data table, and make it tidy (or add deltas) if you need to. Save your tidy table to the data object TIDY_DF. This code chunk will run, but it will not show up in the final html knit.

#Do not change anything in the next code chunk! This is to make sure that your data are now tidy.

#Before the next code chunk is space for you to explain what sort of checking of the data you will be doing to make sure the data meet the assumptions of the model. Using a short paragraph, explain the steps you are going to take to check that your data meet the assumptions of your analysis. For example, maybe you need to make a model and build a histogram residuals? Maybe you need to do a ks test on something? Show me you know the correct steps.

#Enter your code for all figures needed for this Exploratory Data Analysis (EDA) in the next code block. Any figures you make should be labeled appropriately. It should be understandable to someone that does not have access to read your code.

#The next code chunk is for any tests for normality you want to perform. If there are multiple, it should be clear which column of data each is testing.

#After the code block, write a paragraph discussing your EDA results and if you had to do anything regarding outliers or data transformations. Remember to justify all of your decisions in writing and reference the work you just performed. You will make any changes in the next section.

```
#### Import and Tidy the Data
```{r, echo=FALSE,results='hide'}
#This is the code chunk where you will read in the data and make a tidy table.
untidy_DF<-read.csv("isroy.csv")

TIDY_DF<-untidy_DF[c("TongueFat","SPPB")]

```

```{r,echo=FALSE}
#Do not change anything in this code chunk. It is to grade your tidy table.
str(TIDY_DF)
```

#### Exploratory Data Analysis
For this linear regression, I will perform three tests of normality which are Histograms for each of the variables, KS test for each of the variables and a cooks distance plot. I will visualize the data with a scatter plot to see if fitting a line makes sense in this case. 

As my sample size is very small (12 data points), the results of the three normality test might differ. In fact, visually looking at the histogram alone will not be enough or even suggested because it does not contain enough data points to show an accurate distribution. Therefore, even if the KS test has a p value > 0.05, the cooks plot might have different results, so I will have to  check if there are any outliers and remove them (cooks distance > 0.5) because it significantly influences the data. 


```{r,echo=FALSE}
#In this section, make any figures you need to perform to examine the data. All figures need to be made in ggplot

Histogram_TongueFat<-ggplot(data = TIDY_DF, aes(x=TongueFat))+
  geom_histogram(binwidth = 1, color= "black", fill= "lightblue")+
  theme_bw()+
  labs(x = " Tounge Fat ",
  title = "Histogram for Tongue Fat")
Histogram_TongueFat

Histogram_SPPB<-ggplot(data = TIDY_DF, aes(x=SPPB))+
  geom_histogram(binwidth = 1, color= "black", fill= "lightpink")+
  theme_bw()+
  labs (x = " SPPB ",
  title = "Histogram for SPPB score")
Histogram_SPPB


lm_TIDY_DF<-lm(formula =SPPB ~ TongueFat, data = TIDY_DF)

Row_number<-c(1:12)
cooks_dist<-cooks.distance (lm_TIDY_DF)
cooks_DF<-data.frame(Row_number,cooks_dist)
cooks_plot<-cooks_DF%>%
  ggplot(aes(x= Row_number, y=cooks_dist))+
  geom_point()+
  labs(x= "Row number", 
        y = "Cooks distance",
       title = "Cooks Plot")+
  theme_bw()
cooks_plot

scatter<-TIDY_DF %>%
  ggplot(aes(x = TongueFat, y = SPPB))+
  geom_point()+
  theme_bw()+
  labs(x= "Tongue Fat", 
        y = "SPPB",
       title = "Scatter plot")
scatter

```

```{r,warning=FALSE}

ks.test_TongueFat<- ks.test(TIDY_DF$TongueFat, pnorm,mean(TIDY_DF$TongueFat), sd(TIDY_DF$TongueFat))
ks.test_TongueFat

ks.test_SPPB<- ks.test( TIDY_DF$SPPB, pnorm,mean(TIDY_DF$SPPB), sd(TIDY_DF$SPPB))
ks.test_SPPB

```


 *** 
 
#### Description of Current Data and Plan for Fixes if Needed

The null hypothesis with the normality test is that the data are normally distributed. The Histogram gives a vague distribution as expected with the small sample size . The KS test has a p value of 0.7755 for Tongue Fat and 0.7238 for SPPB score, which are both > 0.05 and therefore we fail to reject the null hypothesis. The KS test results mean that each of the variables are normally distributed. However, the cooks plot shows that there is a data point in row 10 which has a cooks distance of > 0.5 which means it is an outlier and needs to be removed because it influences the linear regression model.  
:::

::: {}
#### Correct Data Issues and Build Final Data Table.
```{r Section3 instructions, include=FALSE}

#Before the first code chunk, use a single sentence to describe your plan for data correction if needed. If not, simply state that the data already meet the assumptions of the model.

#In the first code chunk, you will fix any issues and make a new tidy table. If you have nothing to fix, just assign your original tidy table to this new object called Corrected_TIDY_DF. 

#Do not alter any code in the final code chunk of this section.

```

I will remove the outlier (row 10) and make a histogram and do a ks test on each of the variable again to check for normality on corrected data


```{r, echo=FALSE,results='hide'}
#This is the code chunk where you will fix any issues and make a new tidy table. If you have nothing to fix, just assign your original tidy table to this new object. 
outlier_removed<- TIDY_DF[-10,]
Corrected_TIDY_DF<-data.frame(outlier_removed)

```

```{r,echo=FALSE}
#Do not change any thing in this code chunk. It is to grade your new, possibly corrected tidy table.
str(Corrected_TIDY_DF)
```

#### Demonstrate That Your New Data Fit the Assmuptions of the Model
```{r Section4 instructions, include=FALSE}
#Enter your code for all figures needed to demonstrate that the corrected data meet the assumptions of the model in the first code block. Any figures you make should be labeled appropriately. It should be understandable to someone that does not have access to read your code. If your data were already meeting the assumptions of your model in section 2, you may leave this code chunk blank.

#The code chunk following your figures is for any tests for normality you want to perform. If there are multiple, it should be clear which column of data each is testing. If your data were already meeting the assumptions of your model in section 2, you may leave this code chunk blank.

```

```{r,echo=FALSE}

Corrected_Histogram_TongueFat<-ggplot(data = Corrected_TIDY_DF, aes(x=TongueFat))+
  geom_histogram(binwidth = 1, color= "black", fill= "lightblue")+
  theme_bw()+
  labs(x = " Tounge Fat ",
  title = "Histogram for Tongue Fat")
Corrected_Histogram_TongueFat

Corrected_Histogram_SPPB<-ggplot(data = Corrected_TIDY_DF, aes(x=SPPB))+
  geom_histogram(binwidth = 1, color= "black", fill= "lightpink")+
  theme_bw()+
  labs (x = " SPPB ",
  title = "Histogram for SPPB score")
Corrected_Histogram_SPPB

3
```

```{r, warning=FALSE}
Corrected_ks.test_TongueFat<- ks.test(Corrected_TIDY_DF$TongueFat, pnorm,mean(Corrected_TIDY_DF$TongueFat), sd(Corrected_TIDY_DF$TongueFat))
Corrected_ks.test_TongueFat

Corrected_ks.test_SPPB<- ks.test(Corrected_TIDY_DF$SPPB, pnorm,mean(Corrected_TIDY_DF$SPPB), sd(Corrected_TIDY_DF$SPPB))
Corrected_ks.test_SPPB
```

 *** 
  
  
#### Test the Null Hypothesis of Your Research Question
```{r Section5 instructions, include=FALSE}
#In the code block below, enter all code you need to test the null hypothesis of your research question. If you need to perform any post-hoc analysis, do so in this section as well.

#Hint! In this section, make sure we can see your results. Look back at past homeworks for ways I have had you do this.
```

```{r}
lm_Corrected_TIDY_DF<-lm(formula =SPPB ~ TongueFat, data = Corrected_TIDY_DF)
summary(lm_Corrected_TIDY_DF)

```


#### Build the Figure That Best Tells the Story of Your Model
```{r Section6 instructions, include=FALSE}
#In the code block below, build a high quality figure that tells the story of your results. Plots should have: axis labels that make sense, the correct geometry, good use of space, and color. I have given the IAs permission to give a bonus point for very high quality or creative figures that still meet specifications.  
```

```{r,echo=FALSE}

figure<- Corrected_TIDY_DF %>%
  ggplot(aes(TongueFat,SPPB))+
  geom_point(color = "black", shape = 16, size = 3)+
  geom_smooth(method = "lm", color = "blue")+
  labs( x= "Tongue Fat", 
        y = "SPPB score",
        title="Linear Regression of Tongue Fat and SPPB")+
  theme_bw()+
  theme(
    plot.title = element_text(face = "bold", size=14),
    axis.title.x = element_text(face = "bold",size=14),
    axis.title.y = element_text(face = "bold",size=14)
  )
figure

```

#### Figure Caption

```{r Section7 instructions, include=FALSE}
#In the space below these instructions, but above the three colon mark(:::), write a summary of your findings by way of a figure caption. Walk the reader through your findings highlighting the important values from your analysis. Yes, the actual numbers should be in this paragraph!
```

The formula of the linear model is SPPBs = (Tongue Fat * 0.3801) - -5.4533. The Multiple R-squared value of 0.07107 means that 7.1% of the variation in Short Physical Performance Battery score can be explained by Tongue Fat. As the p value is 0.4281 > 0.05, we fail to reject the null hypothesis meaning there is no linear relationship between tongue fat and the Short Physical Performance Battery score. 

My sample size was small because measuring tongue fat firstly requires minimizing movement of head and tongue which is difficult for individuals with cognitive impairment and secondly needs an MRI of the tongue which is expensive to conduct. 

In the research paper, there was a positive correlation between the number of teeth and the SPPB score. In detail, poor occlusion was associated with significantly lower SPPB scores. Therefore, I wanted to answer whether there was a relationship between Tongue fat and SPPB scores. 

Citations 

GeeksforGeeks. (2022, December 27). Assumptions of Linear Regression. GeeksforGeeks. https://www.geeksforgeeks.org/machine-learning/assumptions-of-linear-regression/

Interpret the key results for Histogram. (n.d.). Support.minitab.com. https://support.minitab.com/en-us/minitab/help-and-how-to/graphs/histogram/interpret-the-results/key-results/

Humbert, I. A., Reeder, S. B., Porcaro, E. J., Kays, S. A., Brittain, J. H., & Robbins, J. (2008). Simultaneous estimation of tongue volume and fat fraction using IDEAL‐FSE. Journal of Magnetic Resonance Imaging, 28(2), 504–508. https://doi.org/10.1002/jmri.21431

ggplot2 axis ticks : A guide to customize tick marks and labels - Easy Guides - Wiki - STHDA. (2024). Sthda.com. https://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
:::

::::

