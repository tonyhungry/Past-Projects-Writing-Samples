#### Brexit Exit Polls ####
# Tony Hung # 

# LEGEND #
# is a comment
## means sub-section
#### means a new section

# We are using a 1% significance level throughout this whole project

# Load required packages and set up data for analysis
# install.packages("dslabs")
# install.packages("viridis") 
# install.packages("tidyverse")

library("viridis")
require(ggplot2)
require("tidyverse")
require(dslabs)
df <- brexit_polls
?brexit_polls

# This dataset includes the poll outcomes for 127 polls from Janaury 2016 to the referendum date on June 23, 2016.
# This project aims to do basic data exploration for this dataset, with basic statistics, graphs, and statistical inference methods.
# The Brexit Referendum asked the citizens of the UK if the UK should remain in the European Union or not.

#### 1. Descriptive Statistics ####
summary(df)
# In this dataset, we have four categorical variables, and five numerical variables. There are no missing values in this dataset.
# The first start date of the poll is on the 8th January 2016 and the last start date of the poll is on the 23rd June 2016. On average, the start date of the poll is on the 16th April 2016.
# The first end date of the poll is on the 10th January 2016 and the last end date of the poll is on the 23rd of June 2016. On average the end date of the poll rests on the 18th April 2016.
# There are many different pollsters which conducted this survey. The most was from ICM, then YouGov. 
# 85 polls were online polls and 42 polls were telephone polls in this survey.
# There are various sample sizes within each poll. The smallest sample size was 497 and the biggest sample size was 4772. On average, the sample size was 1694.
# On average, 44.00% of the people said that they would like to remain in the EU.
# On average, 42.23% of the people said that they would like to leave the EU.
# On average, 12.65% of the people said that they are undecided about this issue.
# On average, the spread of this poll is 2%. This means that there are 2% more people who would like to remain in the EU than leave the EU.

#### 2. How many people (respondents) responded per category?####

sum(df$undecided*df$samplesize)/127 
# The total number of undecided respondents divided by 127 polls. On average there are 210.93 undecided respondants per poll.
sum(df$leave*df$samplesize)/127
# On average, there are 723.49 people who voted for leave per poll.
sum(df$remain*df$samplesize)/127 
# On average, there are 742.23 people who voted for remain per poll.

# Thus we can summarize that the polls predicted there will be more people voted for remain than leave.

#### 3. Exit Day: 23rd June 2016 ####
# If we specifically look at the exit polls conducted by YouGov on the 23rd June 2016, the polls showed that 52% of the people surveyed voted for remain and 48% of the people surveyed voted for leave. In contrast to the real results of the referendum, 51.9% of the people voted for leave and 48.1% of the people voted for remain. This showed that there is a misspecification with the people surveyed on the day of the referendum.

#### 4. Evolution of Voters ####

ed <- df %>%
  select(enddate, leave, remain, undecided) %>%
  gather(key = "variable", value = "value", -enddate)
head(ed)

ggplot(ed, aes(x = enddate, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Evolution of Opinions About Brexit") + 
  xlab("Month") +
  ylab("Proportion of Population")

# From the graph produced, it is hard to say about the general trend of voters. However, we can see that towards the end of the poll (June), the concentration of voters voting leave is much more concentrated in comparison to the other months. We can say that there is some linear positive relationship between leave and the end dateof the polls. However, from mid-April to the end of June, we can see a clear positive relationship between the two variables.
# We can also see that the undecided population fluctuates a lot, starting in January until mid-May. It is also interesting to see that although the undecided corresponds to the leave and remain variables, but the fluctuation get heavier with the leave population. This is particularly evident at the beginning of June. 

ggplot(df, aes(x = enddate, y = spread)) +
  geom_point(aes(color = enddate)) +
  ggtitle("Evolution of Spread") + 
  xlab("Month") +
  ylab("Proportion of Population")

# This graph shows the spread of the population, for the 6 months before the referendum happened. The spread is calculated as the difference between remain and leave. This shows exactly how much more people would like to leave or remain with each poll. If the variable is positive, it means that more people would like to remain in the EU than leave. If the variable is negative, it means that there are more people would like to leave the EU than reamin in the EU. 

#### 5. Is there a correlation between remain, leave and undecided? ####
cor(df$leave,df$remain)
# The Pearson correlation coefficient is 0.18 which is very low, thus the correlation is rather negligible.
cor(df$leave,df$undecided)
# The Pearson correlation coefficient is -0.69 which is rather high.
cor(df$remain,df$undecided)
# The Pearson correlation coefficient is -0.78 which is rather high. 

# The above correlation coefficients further proves our suspicion from the graph, where we saw that there is some movement between undecided and leave and undecided and remain. This shows that the opinions of of the poeple surveyed are highly volatile and is subject to change.

#### 6. Different opinoins vs pollster #### 

ggplot(df, aes(x=enddate, y=leave, group=pollster, color = pollster)) +
  geom_line() + 
  geom_point() + 
  xlab("Month") + 
  ylab("Proportion of people voting for leave") + 
  ggtitle("Proportion of People Voting for Leave vs The Polling Company")

ggplot(df, aes(x=enddate, y=remain, group=pollster, color = pollster)) +
  geom_line() + 
  geom_point() + 
  xlab("Month") + 
  ylab("Proportion of people voting for remain") + 
  ggtitle("Proportion of People Voting for Remain vs The Polling Company")

ggplot(df, aes(x=enddate, y=undecided, group=pollster, color = pollster)) +
  geom_line() + 
  geom_point() + 
  xlab("Month") + 
  ylab("Proportion of people voted undecided") + 
  ggtitle("Proportion of People Voted Undecided vs The Polling Company")

# As we can see from the graphs above, some of pollsters revealed to be in a constant high or low with certain opinions. For example, Opinium has a consistently high proportion of people voting for leave. Likewise, TNS has a consistenly high proportion of people who voted undecided in their polls. 

#### 7. What is the average percentage of opinions by pollster? ####

aggregate(df$leave, by = list(Category=df$pollster), FUN = mean)
aggregate(df$undecided, by = list(Category=df$pollster), FUN = mean)
aggregate(df$remain, by = list(Category=df$pollster), FUN = mean)

# From these aggregations, we can see the average percentage of different opinions of voters by each survey company. The ORB surveys produced the highest proportion of people voted for leave. The TNS surveys produced the highest proportion of people voted for undecided. The Populus surveys produced the highest proportion of people voted for undecided.

#### 8. Different opinoins vs poll type #### 

ggplot(df, aes(x = enddate, y = leave)) + 
  geom_point(aes(color = poll_type)) + 
  xlab("Month") + 
  ylab("Proportion of people voting for leave") + 
  ggtitle("Polling Method vs The Proportion of People voted Leave") 

ggplot(df, aes(x = enddate, y = remain)) + 
  geom_point(aes(color = poll_type))  + 
  xlab("Month") + 
  ylab("Proportion of people voting for remain") + 
  ggtitle("Polling Method vs The Proportion of People voted Remain")

ggplot(df, aes(x = enddate, y = undecided)) + 
  geom_point(aes(color = poll_type))  + 
  xlab("Month") + 
  ylab("Proportion of people voted undecided") + 
  ggtitle("Polling Method vs the proportion of people voted Undecided")

# As we can see from the the three graphs above, there seems to be a difference in the opinions between the two polling methods. Polls conducted online seems to have a lower percentage of people who voted remain in comparison to people who were surveyed through the telephone. There seems to be a higher proportion of people who voted for undecided with the web method than through the telephone. However, with people who voted for leave, it seems to be a mix result between the two polling methods.

## Spread of poll opinions
ggplot(df, aes(enddate, spread, color = poll_type)) +
  geom_hline(aes(yintercept = mean(df$spread), color = "Average Spread")) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() + ylab("Spread between Remain and Leave") + xlab("End Date") +
  ggtitle("Spread vs Poll Type")

# This graph shows the dispersion of the spread between poll types throughout the months. As we can see, the spread between the two polling types are quite distinct. Generally, the online polls would have a negative spread and the telephone polls generally have a positive spread. This means that there are more people who voted for remain in the telephone surveys and more people whoe voted for leave in the online surveys. At the beginning of June 2016, both polling methods are negative, which means that in both polling methods, it indicated that there are more people voting for leave than remain. 

#### 9. What is the average percentage of opinions by poll type? ####

aggregate(df$leave, by = list(Category=df$poll_type), FUN = mean)
aggregate(df$undecided, by = list(Category=df$poll_type), FUN = mean)
aggregate(df$remain, by = list(Category=df$poll_type), FUN = mean)

# The above aggregations corresponds to the graphs in section 8 where we could see visually a difference between the polling methods. The above calculated the average proportion in each variable, by poll type.

#### 10. Correlation between variables ####

cor(df$samplesize, df$leave)
# There is a relationship between sample size and the proportion of respondents voting leave. The Pearson correlation value is 0.24, which means that there is a slight positive relationship between the two variables. This should not be a concern because the sample size is rather small.
cor(df$samplesize, df$remain)
# The Pearson correlation is -0.19, which means that there is a slight negative relationship between the two variables.
cor(df$samplesize, df$undecided)
# The Pearson correlation is -0.06, which means that there is almost no relationship between the two variables.

#### 11. Is there a difference in porpotion between poll types? ####

## Boxplots
ggplot(df, aes(x = poll_type, y = remain, fill=poll_type)) + 
  geom_boxplot() + ylab("Proportion of People Voting Remain") + xlab("Poll Type") +
  ggtitle("People Voting Remain by Poll Type")
# On average, people who did their poll on the telephone has a higher proportion of remaining than poeple who did the online poll.
ggplot(df, aes(x = poll_type, y = undecided, fill=poll_type)) + 
  geom_boxplot() + ylab("Proportion of People Voting Undecided") + xlab("Poll Type") +
  ggtitle("People Voting Undecided by Poll Type")
# On average, people online have a higher proportion of being undecided about Brexit than people who did their poll through the telephone.
ggplot(df, aes(x = poll_type, y = leave, fill=poll_type)) + 
  geom_boxplot()+ ylab("Proportion of People Voting Leave") + xlab("Poll Type") +
  ggtitle("People Voting Leave by Poll Type")
# On average, there are almost no difference between people who did their poll online or through the phone

## Normality checks with histograms
# Formatting omitted since it's purely to check the normality before doing the t-test of two proportions
ggplot(df, aes(remain)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=poll_type))
ggplot(df, aes(undecided)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=poll_type))
ggplot(df, aes(leave)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=poll_type))
ggplot(df, aes(remain)) + 
  geom_histogram(aes(fill=pollster))
ggplot(df, aes(undecided)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=poll_type))
ggplot(df, aes(leave)) + scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=poll_type))
# All seems to be ok, not exactly normal. Also, since the sample size is bigger or equal to 30, thus it should converge to normal, thus normality is assumed.

## Further normality tests
shapiro.test(df$remain)
shapiro.test(df$leave)
shapiro.test(df$undecided)
# From the Shapiro-Wilk tests, the remain variable violates the normality assumption, because it rejected HO. However, the two-sample t-test will still be carried out. 

## Two-Sample t test
ttest1 <- df %>%
  filter(poll_type == "Online" | poll_type == "Telephone") %>%
  select(poll_type, remain, leave, undecided)
t.test(remain ~ poll_type, data = ttest1)
# HO: There is no difference between the means in remain of the two polling types.
# HA: There is a difference between the means in remain of the two polling types.
# Because the p-value is less than 0.01, thus we reject HO and accept HA and state that there is a difference between the means in remain of the two polling types.
t.test(leave ~ poll_type, data = ttest1)
# HO: There is no difference between the means in leave of the two polling types.
# HA: There is a difference between the means in leave of the two polling types.
# Because the p-value is bigger than 0.01, thus we fail to reject HO and stat that there is no difference between the two polling types.
t.test(undecided ~ poll_type, data = ttest1)
# HO: There is no difference between the means in undecided of the two polling types.
# HA: There is a difference between the means in undecided of the two polling types.
# Because the p-value is less than 0.01, thus we reject HO and accept HA and state that there is a difference between the means in undecided of the two polling types.

# Just to be sure, I've run the non-paramteric version of the test for the remain variable, because normality was not met. 
kruskal.test(remain ~ poll_type, data = ttest1)
# HO: There is no difference between the means in remain of the two polling types.
# HA: There is a difference between the means in remain of the two polling types.
# Because the p-value is less than 0.01, thus we reject HO and accept HA and state that there is a difference between the means in remain of the two polling types.
# Thus this reconfirms the results from the two sample t-test.

#### 12. Conclusion ####

# This project has mainly explored the elementary data analysis of the different polls conducted 6 months before the Brexit Referendum (including the exit poll). The data has revealed that although with extensive surveys and polling, the polls could not capture the true population parameter and thus failed to predict the results of the referendum. There are only four instances in this dataset where the polls indicated that the UK will leave the EU (if we do not count the undecided). In these four instances, three out of the four polls were conducted by ORB, and all has a high sample size. In this project, I have used summative statistics, graphs, and statistical inference to explore, probe, and analyze, this dataset.

# However, this is not the end for this data exploration exercise. Further questions are raised from these explorations. For example, why is there a dip in both polling types when we are looking at the spread variable? Why are some polling companies consistently scoring high in on of the three main variables? These questions requires further thought and research in order answer it in its entirety. I feel that for further explorations, cluster and factor analysis could be used to group the pollsters. One technique such as the dendrogram would be particularly useful for this. Another idea for further analysis would be that all statistical analysis performed are done with weights, where we use the sameplsize variable as weights to give some polls more credit than others. 