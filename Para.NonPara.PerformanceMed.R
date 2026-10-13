####  STAT 523 Project 1  #######################################

library(readxl)
library(ggpubr)
library(stats)
Project1 <- read.csv("survey_clean.csv")
View(Project1)

#### Preliminary checks, Verifying all data is present ##########

length(Project1$Age)
length(Project1$KmBefore)
length(Project1$KgBefore)
length(Project1$TimeBefore)
length(Project1$KmAfter)
length(Project1$KgAfter)
length(Project1$TimeAfter)
## Confirmed no missing data.

#### General exploratory data analysis ##############
summary(Project1$Age)
mean(Project1$Age)
sd(Project1$Age)
ggboxplot(Project1$Age, ylab="Age (years)", xlab=FALSE)
hist(Project1$Age)

# Nice colored histograms for slides
hist(Project1$Age,        col = "#E0BBE4", main = "Age",        xlab = "Years", cex.main = 4)
hist(Project1$KgBefore,   col = "#BDE0FE", main = "Weight Before (kg)",   xlab = "kg",    cex.main = 4)
hist(Project1$KmBefore,   col = "#CDEAC0", main = "Run dist. before (km)", xlab = "km",    cex.main = 4)
hist(Project1$TimeBefore, col = "#FFD6A5", main = "Run time before (min)",  xlab = "time",  cex.main = 3.8)

# Nice colored boxplots for slides
boxplot(Group_med1$KmBefore, Group_med1$KmAfter, col = "#CDEAC0", main = "Distance Before, After")
boxplot(Group_med1$KgBefore, Group_med1$KgAfter, col = "#BDE0FE", main = "Weight Before, After")
boxplot(Group_med1$TimeBefore, Group_med1$TimeAfter, col = "#FFD6A5", main = "Run Time Before, After")

# QQ norm graphs for slides, preintervention distributions
qqnorm(Project1$Age, main = "Q-Q Plot: Age", cex.main=4)
qqline(Project1$Age)

qqnorm(Project1$KmBefore, main = "Q-Q Plot: KmBefore", cex.main=4)
qqline(Project1$KmBefore)

qqnorm(Project1$KgBefore, main = "Q-Q Plot: KgBefore", cex.main=4)
qqline(Project1$KgBefore)

qqnorm(Project1$TimeBefore, main = "Q-Q Plot: Run time before (min)", cex.main=2.6)
qqline(Project1$TimeBefore)

# ~~~~~~~~~~~~~~~~~~~ NO MEDICINE GROUP, CONTROL  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Group_nomed <- Project1[Project1$Medicine1 == "No" & Project1$Medicine2 == "No" & Project1$Medicine3 == "No", ]
km_d_nomed=Group_nomed$KmAfter-Group_nomed$KmBefore

# Nice colored boxplots for slides
boxplot(Group_nomed$KmBefore, Group_nomed$KmAfter, col = "#CDEAC0", main = "Distance Before, After", cex.main=3)
boxplot(Group_nomed$KgBefore, Group_nomed$KgAfter, col = "#BDE0FE", main = "Weight Before, After", cex.main=3)
boxplot(Group_nomed$TimeBefore, Group_nomed$TimeAfter, col = "#FFD6A5", main = "Run Time Before, After", cex.main=3)

#### Testing difference in KmAfter - KmBefore ########

summary(km_d_nomed)
hist(km_d_nomed, col=2)
qqnorm(km_d_nomed, col=4, lwd=2)
qqline(km_d_nomed)
shapiro.test(km_d_nomed)
# Difference in km's ran (After-Before) is normally distributed

## These T-tests are testing whether (Km ran Before)-(Km ran After) is positive or negative.
# A positive value indicates distance ran decreased (Before > After).
# A negative value indicates distance ran increased (After > Before).
t.test(Group_nomed$KmBefore, Group_nomed$KmAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0
# mean difference: -1.166013 

t.test(Group_nomed$KmBefore, Group_nomed$KmAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0.
# mean difference: -1.166013 

t.test(Group_nomed$KmBefore, Group_nomed$KmAfter, paired=T, alternative="greater")
# p-value =1, fail to reject null. Mean difference not greater than 0.

# Given these t-tests, we conclude that the mean difference in Kms ran (Before-After) is less than 0.
# Distance ran increased.

wilcox.test(Group_nomed$KmBefore, Group_nomed$KmAfter, paired=T, alternative="less") 
# p-value < 0.05, reject null. Median difference in Kms ran is less than 0.

# Non-parametric test agrees with t-test. Distance ran increased in the no-medicine group.


#### Testing difference in KgAfter - KgBefore ##########

kg_d_nomed=Group_nomed$KgAfter-Group_nomed$KgBefore
summary(kg_d_nomed)
hist(kg_d_nomed, col=2)
qqnorm(kg_d_nomed, col=4, lwd=2)
qqline(kg_d_nomed)
shapiro.test(kg_d_nomed)
# Difference in Kg (After - Before) is normally distributed

## These T-tests are testing whether (Kg lifted Before) - (Kg lifted After) is positive or negative.
# A positive value indicates weight decreased (Before > After).
# A negative value indicates weight increased (After > Before).

t.test(Group_nomed$KgBefore, Group_nomed$KgAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -10.07516 

t.test(Group_nomed$KgBefore, Group_nomed$KgAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0.
# mean difference: -10.07516 

t.test(Group_nomed$KgBefore, Group_nomed$KgAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. Mean difference not greater than 0.

# Given these t-tests, we conclude that the mean difference in Kg (Before - After) is less than 0.
# Weight increased in the no-medicine group.

wilcox.test(Group_nomed$KgBefore, Group_nomed$KgAfter, paired=T, alternative="less") 
# p-value < 0.05, reject null. Median difference in Kg is less than 0.
# Non-parametric test agrees with t-test. Weight increased in the no-medicine group.

#### Testing difference in TimeAfter - TimeBefore ##########

time_d_nomed = Group_nomed$TimeAfter - Group_nomed$TimeBefore
summary(time_d_nomed)
hist(time_d_nomed, col=2)
qqnorm(time_d_nomed, col=4, lwd=2)
qqline(time_d_nomed)
shapiro.test(time_d_nomed)
# Difference in Time (After - Before) is normally distributed

## These T-tests are testing whether (Time Before) - (Time After) is positive or negative.
# A positive value indicates time decreased (Before > After).
# A negative value indicates time increased (After > Before).
t.test(Group_nomed$TimeBefore, Group_nomed$TimeAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -13.90523 

t.test(Group_nomed$TimeBefore, Group_nomed$TimeAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0.
# mean difference: -13.90523 

t.test(Group_nomed$TimeBefore, Group_nomed$TimeAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. Mean difference not greater than 0.

# Given these t-tests, we conclude that the mean difference in Time (Before - After) is less than 0.
# Time increased in the no-medicine group.

wilcox.test(Group_nomed$TimeBefore, Group_nomed$TimeAfter, paired=T, alternative="less") 
# p-value < 0.05, reject null. Median difference in Time is less than 0.
# Non-parametric test agrees with t-test. Time increased in the no-medicine group.

# ~~~~~~~~~~~~~~~~~~~ MEDICINE 1 GROUP  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Group_med1 <- Project1[Project1$Medicine1 == "Yes" & 
                       Project1$Medicine2 == "No" & 
                       Project1$Medicine3 == "No", ]


#### Testing difference in KmAfter - KmBefore ##########


km_d_med1 = Group_med1$KmAfter - Group_med1$KmBefore
summary(km_d_med1)
hist(km_d_med1, col=2)
qqnorm(km_d_med1, col=4, lwd=2)
qqline(km_d_med1)
shapiro.test(km_d_med1)
# Difference in Km (After - Before) is normally distributed

# Nice colored boxplots for slides
boxplot(Group_med1$KmBefore, Group_med1$KmAfter, col = "#CDEAC0", main = "Distance Before, After", cex.main=3)
boxplot(Group_med1$KgBefore, Group_med1$KgAfter, col = "#BDE0FE", main = "Weight Before, After", cex.main=3)
boxplot(Group_med1$TimeBefore, Group_med1$TimeAfter, col = "#FFD6A5", main = "Run Time Before, After", cex.main=3)

## These T-tests are testing whether (Km Before) - (Km After) is positive or negative.
# A positive value indicates distance decreased (Before > After).
# A negative value indicates distance increased (After > Before).
t.test(Group_med1$KmBefore, Group_med1$KmAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -1.178246

t.test(Group_med1$KmBefore, Group_med1$KmAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Distance ran increased in the Medicine 1 group.
# mean difference: -1.178246

t.test(Group_med1$KmBefore, Group_med1$KmAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med1$KmBefore, Group_med1$KmAfter, paired=T, alternative="less") 
# p-value < 0.05, reject null. Median difference in Km is less than 0.
# Non-parametric test agrees with t-test. Distance ran increased.


#### Testing difference in KgAfter - KgBefore ##########

kg_d_med1 = Group_med1$KgAfter - Group_med1$KgBefore
summary(kg_d_med1)
hist(kg_d_med1, col=2)
qqnorm(kg_d_med1, col=4, lwd=2)
qqline(kg_d_med1)
shapiro.test(kg_d_med1)
# p-value > 0.05. Difference in Kg (After - Before) is normally distributed

## These T-tests are testing whether (Kg Before) - (Kg After) is positive or negative.
# A positive value indicates weight decreased (Before > After).
# A negative value indicates weight increased (After > Before).

t.test(Group_med1$KgBefore, Group_med1$KgAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -9.03333

t.test(Group_med1$KgBefore, Group_med1$KgAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Weight increased in the Medicine 1 group.
# mean difference: -9.03333

t.test(Group_med1$KgBefore, Group_med1$KgAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med1$KgBefore, Group_med1$KgAfter, paired=T, alternative="less") 
# p-value < 0.05, reject null. Median difference in Kg is less than 0.
# Non-parametric test agrees with t-test. Weight increased.


#### Testing difference in TimeAfter - TimeBefore ##########

time_d_med1 = Group_med1$TimeAfter - Group_med1$TimeBefore
summary(time_d_med1)
hist(time_d_med1, col=2)
qqnorm(time_d_med1, col=4, lwd=2)
qqline(time_d_med1)
shapiro.test(time_d_med1)
# p-value > 0.05. Difference in Time (After - Before) is normally distributed

## These T-tests are testing whether (Time Before) - (Time After) is positive or negative.
# A positive value indicates time decreased (Before > After).
# A negative value indicates time increased (After > Before).
t.test(Group_med1$TimeBefore, Group_med1$TimeAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -16.01579

t.test(Group_med1$TimeBefore, Group_med1$TimeAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Time increased in the Medicine 1 group.
# mean difference: -16.01579

t.test(Group_med1$TimeBefore, Group_med1$TimeAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med1$TimeBefore, Group_med1$TimeAfter, paired=T, alternative="less") 
# p-value < 0.05, reject null. Median difference in Time is less than 0.
# Non-parametric test agrees with t-test. Time increased in the Medicine 1 group.

# ~~~~~~~~~~~~~~~~~~~ MEDICINE 2 GROUP  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Group_med2 <- Project1[Project1$Medicine1 == "No" & 
                       Project1$Medicine2 == "Yes" & 
                       Project1$Medicine3 == "No", ]
km_d_med2 = Group_med2$KmAfter - Group_med2$KmBefore

# Nice colored boxplots for slides — Medicine 2
boxplot(Group_med2$KmBefore,  Group_med2$KmAfter,  col = "#CDEAC0",main = "Distance Before, After", cex.main = 3)
boxplot(Group_med2$KgBefore,  Group_med2$KgAfter,  col = "#BDE0FE", main = "Weight Before, After",   cex.main = 3)
boxplot(Group_med2$TimeBefore,Group_med2$TimeAfter,col = "#FFD6A5", main = "Run Time Before, After", cex.main = 3)

#### Testing difference in KmAfter - KmBefore ##########

summary(km_d_med2)
hist(km_d_med2, col=2)
qqnorm(km_d_med2, col=4, lwd=2)
qqline(km_d_med2)
shapiro.test(km_d_med2)
# p-value < 0.05, there is evidence against normality in km differnce.

## These T-tests are testing whether (Km Before) - (Km After) is positive or negative.
# A positive value indicates distance decreased (Before > After).
# A negative value indicates distance increased (After > Before).
t.test(Group_med2$KmBefore, Group_med2$KmAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -0.8614173 

t.test(Group_med2$KmBefore, Group_med2$KmAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Distance ran increased in the Medicine 2 group.
# mean difference: -0.8614173 

t.test(Group_med2$KmBefore, Group_med2$KmAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med2$KmBefore, Group_med2$KmAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Median difference in Km is less than 0.
# Non-parametric test agrees with t-test. Distance ran increased.


#### Testing difference in KgAfter - KgBefore ##########

kg_d_med2 = Group_med2$KgAfter - Group_med2$KgBefore
summary(kg_d_med2)
hist(kg_d_med2, col=2)
qqnorm(kg_d_med2, col=4, lwd=2)
qqline(kg_d_med2)
shapiro.test(kg_d_med2)
# Difference in Kg (After - Before) is normally distributed

## These T-tests are testing whether (Kg Before) - (Kg After) is positive or negative.
# A positive value indicates weight decreased (Before > After).
# A negative value indicates weight increased (After > Before).
t.test(Group_med2$KgBefore, Group_med2$KgAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -9.211024 

t.test(Group_med2$KgBefore, Group_med2$KgAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# mean difference: -9.211024 
# Weight increased in the Medicine 2 group.

t.test(Group_med2$KgBefore, Group_med2$KgAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med2$KgBefore, Group_med2$KgAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Median difference in Kg is less than 0.
# Non-parametric test agrees with t-test. Weight increased.


#### Testing difference in TimeAfter - TimeBefore ##########

time_d_med2 = Group_med2$TimeAfter - Group_med2$TimeBefore
summary(time_d_med2)
hist(time_d_med2, col=2)
qqnorm(time_d_med2, col=4, lwd=2)
qqline(time_d_med2)
shapiro.test(time_d_med2)
# Difference in Time (After - Before) is not normally distributed (p < 0.05)

## These T-tests are testing whether (Time Before) - (Time After) is positive or negative.
# A positive value indicates time decreased (Before > After).
# A negative value indicates time increased (After > Before).
t.test(Group_med2$TimeBefore, Group_med2$TimeAfter, paired=T, alternative="two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -15.3189 

t.test(Group_med2$TimeBefore, Group_med2$TimeAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Time increased in the Medicine 2 group.
# mean difference: -15.3189 

t.test(Group_med2$TimeBefore, Group_med2$TimeAfter, paired=T, alternative="greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med2$TimeBefore, Group_med2$TimeAfter, paired=T, alternative="less")
# p-value < 0.05, reject null. Median difference in Time is less than 0.
# Non-parametric test agrees with t-test. Time increased in the Medicine 2 group. 


# ~~~~~~~~~~~~~~~~~~~ MEDICINE 3 GROUP  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Group_med3 <- Project1[Project1$Medicine1 == "No" & 
                       Project1$Medicine2 == "No" & 
                       Project1$Medicine3 == "Yes", ]
                       km_d_med3 = Group_med3$KmAfter - Group_med3$KmBefore
summary(km_d_med3)
                       
# Nice colored boxplots for slides — Medicine 3
boxplot(Group_med3$KmBefore,  Group_med3$KmAfter,  col = "#CDEAC0", main = "Distance Before, After", cex.main = 3)
boxplot(Group_med3$KgBefore,  Group_med3$KgAfter,  col = "#BDE0FE", main = "Weight Before, After",   cex.main = 3)
boxplot(Group_med3$TimeBefore,Group_med3$TimeAfter,col = "#FFD6A5", main = "Run Time Before, After", cex.main = 3)

#### Testing difference in KmAfter - KmBefore ##########

hist(km_d_med3, col=2)
qqnorm(km_d_med3, col=4, lwd=2)
qqline(km_d_med3)
shapiro.test(km_d_med3)
# Difference in Km (After - Before) is normally distributed (p > 0.05)

## These T-tests are testing whether (Km Before) - (Km After) is positive or negative.
# A positive value indicates distance decreased (Before > After).
# A negative value indicates distance increased (After > Before).
t.test(Group_med3$KmBefore, Group_med3$KmAfter, paired = TRUE, alternative = "two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -0.99152 

t.test(Group_med3$KmBefore, Group_med3$KmAfter, paired = TRUE, alternative = "less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Distance ran increased in the Medicine 3 group.
# mean difference: -0.99152 

t.test(Group_med3$KmBefore, Group_med3$KmAfter, paired = TRUE, alternative = "greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med3$KmBefore, Group_med3$KmAfter, paired = TRUE, alternative = "less")
# p-value < 0.05, reject null. Median difference in Km is less than 0.
# Non-parametric test agrees with t-test. Distance ran increased.


#### Testing difference in KgAfter - KgBefore ##########

kg_d_med3 = Group_med3$KgAfter - Group_med3$KgBefore
summary(kg_d_med3)
hist(kg_d_med3, col=2)
qqnorm(kg_d_med3, col=4, lwd=2)
qqline(kg_d_med3)
shapiro.test(kg_d_med3)
# Difference in Kg (After - Before) is approximately normally distributed (p > 0.05)

## These T-tests are testing whether (Kg Before) - (Kg After) is positive or negative.
# A positive value indicates weight decreased (Before > After).
# A negative value indicates weight increased (After > Before).
t.test(Group_med3$KgBefore, Group_med3$KgAfter, paired = TRUE, alternative = "two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -10.5072 

t.test(Group_med3$KgBefore, Group_med3$KgAfter, paired = TRUE, alternative = "less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Weight increased in the Medicine 3 group.
# mean difference: -10.5072 

t.test(Group_med3$KgBefore, Group_med3$KgAfter, paired = TRUE, alternative = "greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med3$KgBefore, Group_med3$KgAfter, paired = TRUE, alternative = "less")
# p-value < 0.05, reject null. Median difference in Kg is less than 0.
# Non-parametric test agrees with t-test. Weight increased.


#### Testing difference in TimeAfter - TimeBefore ##########

time_d_med3 = Group_med3$TimeAfter - Group_med3$TimeBefore
summary(time_d_med3)
hist(time_d_med3, col=2)
qqnorm(time_d_med3, col=4, lwd=2)
qqline(time_d_med3)
shapiro.test(time_d_med3)
# Difference in Time (After - Before) is normally distributed (p > 0.05)

## These T-tests are testing whether (Time Before) - (Time After) is positive or negative.
# A positive value indicates time decreased (Before > After).
# A negative value indicates time increased (After > Before).
t.test(Group_med3$TimeBefore, Group_med3$TimeAfter, paired = TRUE, alternative = "two.sided")
# p-value < 0.05, reject null. Mean difference is not equal to 0.
# mean difference: -16.46 

t.test(Group_med3$TimeBefore, Group_med3$TimeAfter, paired = TRUE, alternative = "less")
# p-value < 0.05, reject null. Mean difference is less than 0 (After > Before).
# Time increased in the Medicine 3 group.
# mean difference: -16.46 

t.test(Group_med3$TimeBefore, Group_med3$TimeAfter, paired = TRUE, alternative = "greater")
# p-value = 1, fail to reject null. No evidence that mean difference > 0 (Before > After).

wilcox.test(Group_med3$TimeBefore, Group_med3$TimeAfter, paired = TRUE, alternative = "less")
# p-value < 0.05, reject null. Median difference in Time is less than 0.

# Distance ran increased. 
# Non-parametric test agrees with t-test. Time increased in the Medicine 3 group.








# ~~~~~~~~~~~~~~ SUMMARY FIGURES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summarize_group_min <- function(dat, label) {
  outcomes <- c("Km", "Kg", "Time")
  do.call(rbind, lapply(outcomes, function(outc) {
    before <- dat[[paste0(outc, "Before")]]
    after  <- dat[[paste0(outc, "After")]]
    idx <- stats::complete.cases(before, after)
    before <- before[idx]; after <- after[idx]

    d_mean <- mean(before - after)  # Before - After

    tt <- t.test(before, after, paired = TRUE)  # two-sided, 95% CI
    wt <- suppressWarnings(
      wilcox.test(before, after, paired = TRUE, alternative = "two.sided", exact = FALSE)
    )

    data.frame(
      Group = label,
      Outcome = outc,
      MeanDiff_BeforeAfter = d_mean,
      Ttestp_value = tt$p.value,
      CI_low = tt$conf.int[1],
      CI_high = tt$conf.int[2],
      p_Wilcox = wt$p.value
    )
  }))
}

# Build the table named 'Summary'
Summary <- do.call(rbind, list(
  summarize_group_min(Group_nomed, "Control"),
  summarize_group_min(Group_med1,  "Medicine 1"),
  summarize_group_min(Group_med2,  "Medicine 2"),
  summarize_group_min(Group_med3,  "Medicine 3")
))

# Optional: neat rounding
Summary$Mean_Diff_BeforeMinusAfter <- round(Summary$Mean_Diff_BeforeMinusAfter, 3)
Summary$CI_low  <- round(Summary$CI_low, 3)
Summary$CI_high <- round(Summary$CI_high, 3)
Summary$t_p_value <- signif(Summary$t_p_value, 3)
Summary$p_wilcox  <- signif(Summary$p_wilcox, 3)

# View the table
Summary



# ~~~~~~~~~~~ Did any of the 3 Medicines Improve physical performance more than the control? ~~~~~~~



####### One-sided comparisons: Each medicine vs Control ########

# Split dataset by group
control <- subset(Project1, Group=="Control")
med1    <- subset(Project1, Group=="Med1")
med2    <- subset(Project1, Group=="Med2")
med3    <- subset(Project1, Group=="Med3")

### Medicine 1 vs Control
t.test(med1$KmChange,   control$KmChange,   alternative="greater", var.equal=TRUE)
t.test(med1$KgChange,   control$KgChange,   alternative="greater", var.equal=TRUE)
t.test(med1$TimeChange, control$TimeChange, alternative="greater", var.equal=TRUE)

### Medicine 2 vs Control
t.test(med2$KmChange,   control$KmChange,   alternative="greater", var.equal=TRUE)
t.test(med2$KgChange,   control$KgChange,   alternative="greater", var.equal=TRUE)
t.test(med2$TimeChange, control$TimeChange, alternative="greater", var.equal=TRUE)

### Medicine 3 vs Control
t.test(med3$KmChange,   control$KmChange,   alternative="greater", var.equal=TRUE)
t.test(med3$KgChange,   control$KgChange,   alternative="greater", var.equal=TRUE)
t.test(med3$TimeChange, control$TimeChange, alternative="greater", var.equal=TRUE)

### Medicine 1 vs Control
wilcox.test(med1$KmChange,   control$KmChange,   alternative="greater", exact=FALSE)
wilcox.test(med1$KgChange,   control$KgChange,   alternative="greater", exact=FALSE)
wilcox.test(med1$TimeChange, control$TimeChange, alternative="greater", exact=FALSE)

### Medicine 2 vs Control
wilcox.test(med2$KmChange,   control$KmChange,   alternative="greater", exact=FALSE)
wilcox.test(med2$KgChange,   control$KgChange,   alternative="greater", exact=FALSE)
wilcox.test(med2$TimeChange, control$TimeChange, alternative="greater", exact=FALSE)

### Medicine 3 vs Control
wilcox.test(med3$KmChange,   control$KmChange,   alternative="greater", exact=FALSE)
wilcox.test(med3$KgChange,   control$KgChange,   alternative="greater", exact=FALSE)
wilcox.test(med3$TimeChange, control$TimeChange, alternative="greater", exact=FALSE)
