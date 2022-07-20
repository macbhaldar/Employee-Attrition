# Employee Attrition

# Import required libraries
library(tidyverse)
library(caTools)
library(caret)
library(randomForest)
library(e1071)
library(forcats)
library(reshape2)
library(skimr)
library(magrittr)
library(rattle)
library(plotly)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(GGally)
library(ggcorrplot)
library(ggpubr)
library(ggrepel)
library(corrgram)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(rpart)
library(rpart.plot)
library(psych)
library(scales)
library(repr)
library(partykit)
library(tree)
library(treemap)
library(treemapify)
library(lightgbm)
library(xgboost)
library(h2o)

options(repr.plot.width=8, repr.plot.height=6)
options(warn=-1)

# import data
df <- read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
head(df)

original_df <- df


# Using an insightful summary with skim and kable
df %>% glimpse()


# Distribution of our Labels

attritions_number <- df %>% group_by(Attrition) %>% summarise(Count=n()) %>%
  ggplot(aes(x=Attrition, y=Count)) + 
  geom_bar(stat="identity", fill="#f0902a", color="grey40") + 
  theme_minimal_hgrid() + 
  geom_text(aes(x=Attrition, y=0.01, label= Count),
            hjust=0.5, vjust=-1, size=5, 
            colour="black", fontface="bold", angle=360) + 
  labs(title="Employee Attrition (Amount)", 
       x="Employee Attrition", 
       y="Amount") + 
  theme(plot.title=element_text(hjust=0.5))

attrition_percentage <- df %>% group_by(Attrition) %>% summarise(Count=n()) %>% 
  mutate(pct=round(prop.table(Count),2) * 100) %>% 
  ggplot(aes(x=Attrition, y=pct)) + 
  geom_bar(stat="identity", fill = "#0a85e5", color="grey40") + 
  geom_text(aes(x=Attrition, y=0.01, label= sprintf("%.0f%%", pct)),
            hjust=0.5, vjust=-1, size=5, 
            colour="black", fontface="bold") + 
  theme_minimal_hgrid() + 
  labs(x="Employee Attrition", y="Percentage") + 
  labs(title="Employee Attrition (%)") + 
  theme(plot.title=element_text(hjust=0.5))

plot_grid(attritions_number, attrition_percentage, align="h", ncol=2)




# Gender Analysis

avg.age <- df %>% select(Gender, Age) %>% group_by(Gender) %>% summarize(avg=mean(Age))
avg.age

# Age Distribution by Gender

dat_text <- data.frame(
  label = c("Mean = 37.33 \n Years Old", "Mean = 36.65 \n Years Old"),
  Gender   = c("Female", "Male"))

gender.dist <- df %>% select(Gender, Age) %>% filter(Gender == 'Male' 
                                                     | Gender== "Female") %>% 
  filter(!is.na(Age)) %>% group_by(Gender) %>% 
  
  ggplot(aes(x=Age)) + 
  geom_density(aes(fill=Gender), alpha=0.8, show.legend=FALSE) + 
  facet_wrap(~Gender) + 
  theme_minimal_grid() + 
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) + 
  labs(title="Age Distribution") + 
  theme(plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("#c4a4c9", "#b3dcda")) + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = 45, y = 0.03, label = label),
    hjust   = -0.1,
    vjust   = -1)

overall.dist <- df %>% select(Gender, Age) %>% filter(!is.na(Age)) %>% 
  ggplot(data=df, mapping=aes(x=Age)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(Age)),
             color="red", linetype="dashed", size=1) +  
  theme_minimal_grid() + labs(x="Overall Age") + 
  annotate("text", label = "Mean = 36.92 Years Old", 
           x = 50, y = 0.03, color = "black")

plot_grid(gender.dist, overall.dist, nrow=2)




# Distribution of Job Satisfaction

box.satisfaction <- df %>% select(Attrition, JobSatisfaction, Gender) %>% 
  ggplot(aes(x=Attrition, y=JobSatisfaction, fill=Attrition)) + 
  geom_boxplot() + 
  theme_minimal_grid() + 
  facet_wrap(~Gender) + 
  scale_color_manual(values=c("#f0902a", "#0a85e5")) +
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) + 
  labs(title="Distribution of Job Satisfaction")
box.satisfaction


# Average Income and Presence by Department

gender.income <- df %>% select(Gender, MonthlyIncome) %>% group_by(Gender) %>% 
  summarise(avg_income=round(mean(MonthlyIncome), 2)) %>%
  ggplot(aes(x=Gender, y=avg_income, color=Gender, fill=Gender)) + 
  geom_bar(stat="identity", width=0.5) + 
  scale_fill_manual(values=c("#0a85e5", "#f0902a")) +  
  scale_color_manual(values=c("#0a85e5", "#f0902a")) +
  geom_text(aes(x=Gender, y=0.01, label= paste0("$ ", avg_income)),
            hjust=-0.5, vjust=0.5, size=5, 
            colour="black", angle=90) + 
  labs(title="Average Salary by Gender", 
                              x="Gender",y="Salary") + 
  theme_minimal_grid() + 
  theme(plot.title=element_text(size=14, hjust=0.5))

# Monthly Income by Gender
box.income <- ggplot(df, aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#0a85e5", "#f0902a")) +  
  theme_minimal_grid() + 
  scale_color_manual(values=c("#0a85e5", "#f0902a")) +
  labs(title="Monthly Income by Gender")

plot_grid(gender.income, box.income, ncol=2)


# How many people work in each department by gender
gender.department <- df %>% group_by(Department, Gender) %>% summarise(amount=n()) %>%
  ggplot(aes(x=reorder(Department, -amount), y=amount, fill=Gender)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_minimal() + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("#0a85e5", "#f0902a")) + 
  labs(title="Number of Employees \n by Department", x="Department", y="Number of employees")
gender.department




# Analysis by Generation and Education


# Distribution of Number of Companies Worked by Attrition and Age

# First we must create categoricals variables based on Age
df$Generation <- ifelse(df$Age<37,"Millenials",
                        ifelse(df$Age>=38 & df$Age<54,"Generation X",
                               ifelse(df$Age>=54 & df$Age<73,"Boomers","Silent")))

# Let's see the distribution by generation now
gen.dist <- df %>% select(Generation, NumCompaniesWorked, Attrition) %>% 
  ggplot() + 
  geom_boxplot(aes(x=reorder(Generation, NumCompaniesWorked, FUN=median), 
                              y=NumCompaniesWorked, fill=Generation)) + 
  theme_minimal_grid() + 
  facet_wrap(~Attrition) + 
  scale_fill_brewer(palette="Blues") + 
  coord_flip() + 
  labs(title="Knowing Past Generations", 
       x="Generation", 
       y="Number of Companies Previously Worked") + 
  theme(legend.position="right", 
        legend.background = element_blank()) + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black", size = 16), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black"))

# Behavioral Difference between Generations
overall.avg <- df %>% select(Generation, NumCompaniesWorked) %>% 
  summarize(avg_ov=mean(NumCompaniesWorked))

# Let's find the Average Numbers of Companies worked by Generation
avg.comp <- df %>% select(Generation, NumCompaniesWorked, Attrition) %>% 
  group_by(Generation, Attrition) %>%
  summarize(avg=mean(NumCompaniesWorked)) %>% 
  ggplot(aes(x=Generation, y=avg, color=Attrition)) + 
  geom_point(size=5) + 
  theme_minimal_grid() + 
  geom_segment(aes(x=Generation, xend=Generation, 
                   y=min(avg), yend=max(avg)),
               # linetype="dashed", 
               size=0.01, color="grey") +  
  labs(title="", 
       subtitle="Behavioral Difference between Generations", 
       x="Generation", 
       y="Average Number of Companies worked for") +  
  coord_flip() + 
  scale_color_manual(values=c("green", "red")) + 
  theme(legend.position="bottom", 
        legend.background = element_blank()) + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(hjust=0.5, color="black", size = 16), 
        plot.background=element_rect(fill="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black"))

plot_grid(gen.dist, avg.comp, nrow=2)




# Attrition by Educational Level

# Give names for the different education levels.
df$Educational_Levels <-  ifelse(df$Education == 1, "Without College Degree.",
                                 ifelse(df$Education == 2 , "College Degree.",
                                        ifelse(df$Education == 3, "Bachelors Degree.",
                                               ifelse(df$Education == 4, "Masters Degree.", "Phd D."))))

# I want to know in terms of proportions if we are loosing key talent here.
edu.level <- df %>% select(Educational_Levels, Attrition) %>% group_by(Educational_Levels, Attrition) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=fct_reorder(Educational_Levels,n), y=n, fill=Attrition, color=Attrition)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~Attrition) + 
  coord_flip() + 
  scale_fill_manual(values=c("#f0902a" , "#0a85e5")) + 
  scale_color_manual(values=c("#f0902a" , "#0a85e5")) + 
  geom_label(aes(label=n, fill = Attrition), 
             colour = "#00008b", hjust=0.5, vjust=0.5, size=4, 
            colour="black", fontface="bold") + 
  labs(x="", y="Number of Employees", xlim=600,
       title="Attrition by Educational Level") + 
  theme_minimal_vgrid() + 
  theme(legend.position="none", plot.title=element_text(hjust=0.5, size=14))

edu.level


edu.pct <- df %>% select(Educational_Levels, Attrition) %>% 
  group_by(Educational_Levels, Attrition) %>% 
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% arrange(desc(pct)) %>%
  
  ggplot(aes(x=fct_reorder(Educational_Levels,pct), y=pct, 
             fill=Attrition, color=Attrition)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~Attrition) + 
  coord_flip() + 
  geom_text(aes(label=paste0(pct, "%"), fill = Attrition), 
             colour = "black", fontface = "italic", hjust=0.1) + 
  scale_fill_manual(values=c("#f0902a" , "#0a85e5")) + 
  scale_color_manual(values=c("#f0902a" , "#0a85e5")) + 
  labs(x="", y="Number of Employees (%)", 
       title="Attrition by Educational Level", 
       subtitle="Percentage (%) by Employee") + 
  theme_minimal_hgrid() + 
  theme(legend.position="none", 
        plot.title=element_text(hjust=0.5, size=14), 
        plot.subtitle=element_text(hjust=0.5, size=12))
edu.pct


# Impact of Income towards Attrition

# Average Income by Department

avg.income <- df %>% select(Department, MonthlyIncome, Attrition) %>% 
  group_by(Attrition, Department) %>%
  summarize(avg.inc=mean(MonthlyIncome)) %>%
  
  ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Attrition) + 
  theme_minimal_vgrid() + 
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) + 
  labs(y="Average Income", 
       x="Department", 
       title="Average Income by Department \n and Attrition Status") + 
  geom_text(aes(x=Department, y=0.01, label= paste0("$ ", round(avg.inc,2))),
            hjust=-0.1, vjust=0.5, size=5, 
            colour="black", angle=0)
avg.income


# Determining Satisfaction by Income

df$JobSatisfaction <- as.factor(df$JobSatisfaction)

high.inc <- df %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% 
  group_by(JobSatisfaction, Attrition) %>%
  summarize(med=median(MonthlyIncome)) %>%
  
  ggplot(aes(x=fct_reorder(JobSatisfaction, -med), y=med, fill=Attrition)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Attrition) + 
  labs(title="Satisfaction by Income", 
       y="Median Income",
       x="Level of Job Satisfaction") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title=element_text(hjust=0.5), 
        strip.background = element_blank(),
        strip.text = element_blank()) + 
  coord_flip() + 
  theme_minimal_hgrid() + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) + 
  scale_color_manual(values=c("#f0902a", "#0a85e5")) + 
  geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med,2))),
            hjust=-0.2, vjust=0.5, size=5, 
            colour="black", angle=0)

high.inc



# Income and the Level of Attrition

per.sal <- df %>% select(Attrition, PercentSalaryHike, MonthlyIncome) %>% 
  ggplot(aes(x=PercentSalaryHike, y=MonthlyIncome)) + 
  geom_jitter(aes(col=Attrition), alpha=0.5) + 
  theme_minimal_hgrid() + theme(legend.position="none") + 
  scale_color_manual(values=c("#f0902a", "#0a85e5")) + 
  labs(title="Income and its Impact on Attrition") + 
  theme(plot.title=element_text(hjust=0.5, color="black"),
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black"))

perf.inc <- df %>% select(PerformanceRating, MonthlyIncome, Attrition) %>% 
  group_by(factor(PerformanceRating), Attrition) %>% 
  ggplot(aes(x=factor(PerformanceRating), y=MonthlyIncome, fill=Attrition)) + 
  geom_violin() + 
  facet_wrap(~Attrition) + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) + 
  theme_minimal_grid() + 
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"),
        axis.title=element_text(colour="black"), 
        legend.text=element_text(color="black")) + 
  labs(x="Performance Rating",y="Monthly Income") 

plot_grid(per.sal, perf.inc, nrow=2)



# Average and Percent Difference of Daily Rates

daily_r <- df %>% select(JobRole, Attrition, DailyRate) %>% 
  group_by(Attrition, JobRole) %>%
  
  ggplot(aes(x=JobRole, y=DailyRate, color=Attrition)) + 
  facet_wrap(~Attrition) + 
  coord_flip() + 
  theme_minimal_hgrid() + 
  theme(axis.text.x = element_text(angle = 90), 
        plot.title=element_text(hjust=0.5, size=15), 
        plot.background=element_rect(fill="white")) + 
  stat_summary(fun.y=mean, fun.ymin = min, fun.ymax = max) + 
  scale_color_manual(values=c("#0a85e5", "#f0902a")) + 
  labs(title="Daily Rates by Job Role")
daily_r


# What's the difference between in Daily rates by attrition and job role status.
attrition_daily <- df %>% select(JobRole, Attrition, DailyRate) %>% 
  group_by(JobRole) %>% filter(Attrition == "Yes") %>% 
  summarize(avg_attrition=mean(DailyRate))

noattrition_daily <- df %>% select(JobRole, Attrition, DailyRate) %>% 
  group_by(JobRole) %>% filter(Attrition == "No") %>% 
  summarize(avg_noattrition=mean(DailyRate))


combined_df <- merge(attrition_daily, noattrition_daily)
colourCount = length(unique(combined_df$JobRole))


percent_diff <- combined_df %>% mutate(pct_diff=round(((avg_noattrition - 
                                                          avg_attrition)/avg_noattrition),2) * 100) %>%
  
  ggplot(aes(x=reorder(JobRole,pct_diff), y=pct_diff, fill=JobRole)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_minimal_vgrid() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set3"))(colourCount)) + 
  theme(plot.title=element_text(hjust=0.5, size=15), 
        plot.background=element_rect(fill="white"), 
        legend.position="none") + 
  labs(x="JobRole", y="Percent Difference (%)", 
       title="Percent Difference Charged by Day") + 
  geom_label(aes(label=paste0(pct_diff, "%")), 
             colour = "black", fontface = "italic", hjust=0.2)
percent_diff


plot_grid(daily_r, percent_diff, nrow=2)



# Level of Attrition by Overtime Status
df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% 
  group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100)

overtime_number <- df %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% 
  group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% 
  
  ggplot(aes(x=OverTime, y=n, fill=OverTime)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) + 
  geom_text(aes(label=paste0(n)), colour = "black", size=5, vjust=-0.5) + 
  labs(title="Level of Attrition by Overtime Status", 
       x="Overtime Status", y="Number of Employees") + 
  theme_minimal_hgrid() + 
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(color="black"), 
        plot.background=element_rect(fill="white"),
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"),
        axis.title=element_text(colour="black"), 
        legend.background = element_blank()) 

overtime_number


# Working Environment
# Number of Employees by Job Role
role.amount <- df %>% select(JobRole) %>% group_by(JobRole) %>% summarize(amount=n()) %>%
  ggplot(aes(area=amount, fill=JobRole, label=JobRole)) +  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none") +
  labs(title = "Major Job Roles Inside the Organization",
       caption = "The area of each tile represents the number of employees by type of job role.", 
       fill = "JobRole")

role.amount


# Mean Salary by JobRole

# Highest percentage of attrition by JobRole

# Mean Salary
job.sal <- df %>% select(JobRole, MonthlyIncome) %>% group_by(JobRole) %>% 
  summarize(med=median(MonthlyIncome), avg=mean(MonthlyIncome))

mean.salary <- ggplot(job.sal, aes(x=reorder(JobRole,-avg), y=avg)) +  
  geom_bar(stat="identity", width=.5, fill="#0a85e5") + 
  theme_minimal_vgrid() + 
  coord_flip() + 
  labs(title="Salary by Job Role",
       x="Job Role",
       y="Mean Income") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

mean.salary


# Attrition by Job Role

# The Funnel with the Attrition Rates by Job Role

attr.job <- df %>% select(JobRole, Attrition) %>% group_by(JobRole, Attrition) %>% 
  summarize(amount=n()) %>% mutate(pct=round(prop.table(amount),2) * 100) %>% arrange(pct)

nofunc <- colorRampPalette(c("#fe902a", "#f5902a", "#f0902a"))
yesfunc <- colorRampPalette(c("#0a85e5", "#0a85e5", "#0a85e5"))

yes.attr <- attr.job %>% filter(Attrition == "Yes") %>% arrange(JobRole) 
no.attr <- attr.job %>% filter(Attrition == "No") %>% arrange(JobRole)

par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, 
                       labels = unique(attr.job$JobRole),
                       top.labels=c("No","","Yes"), 
                       main = "Attrition by Job Role", 
                       gap=40, show.values = T,
                       rxcol = yesfunc(9), 
                       lxcol = nofunc(9)))




# Current Managers and Average Satisfaction Score

df$CatYearsManager <- ifelse(df$YearsWithCurrManager <= 1, "Recently Hired", 
                             ifelse(df$YearsWithCurrManager > 1 & 
                                      df$YearsWithCurrManager <= 4, 
                                    "2-4 Years hired", 
                                    "Long Established Manager"))

# Determine what is the Average Relationship Satisfaction with the Recently Hired Managers
rel.sat <- df %>% select(CatYearsManager, RelationshipSatisfaction, Attrition) %>% 
  group_by(CatYearsManager, Attrition) %>%
  summarize(avg.sat=mean(RelationshipSatisfaction)) %>%
  
  ggplot(aes(x=fct_reorder(CatYearsManager,-avg.sat), y=avg.sat, fill=Attrition)) + 
  geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Attrition) + 
  geom_text(aes(x=CatYearsManager, y=0, label= paste0(round(avg.sat,2))), 
            hjust=-0.5, vjust=0.5, size=5, 
            colour="black", angle=360) + 
  coord_flip() + 
  theme_minimal_vgrid() + 
  theme(legend.position="none", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5),
        axis.text.y = element_text(angle = 0)) + 
  labs(x="Years with Current Manager", 
       y="Average Satisfaction Score", 
       title="Dealing with Current Managers") + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5"))
rel.sat



# Average Environment Satisfaction

env.attr <- df %>% select(EnvironmentSatisfaction, JobRole, Attrition) %>% 
  group_by(JobRole, Attrition) %>%
  summarize(avg.env=mean(EnvironmentSatisfaction))

ggplot(env.attr, aes(x=JobRole, y=avg.env)) + 
  geom_line(aes(group=Attrition), color="#58ACFA", linetype="dashed") + 
  geom_point(aes(color=Attrition), size=3) +  
  coord_flip() + 
  theme_minimal_grid() + 
  theme(plot.title=element_text(hjust=0.5), 
        axis.text.x=element_text(angle=90), 
        plot.background=element_rect(fill="white")) + 
  labs(title="Working Environment", 
       y="Average Environment Satisfaction", 
       x="Job Position") + 
  scale_color_manual(values=c("#0a85e5", "#f0902a"))



# In-Depth Look into Attrition

# Work Life Balance Environment
attritions <- df %>% filter(Attrition == "Yes")

attritions$WorkLifeBalance <- as.factor(attritions$WorkLifeBalance)

by.department <- attritions %>% select(Department, WorkLifeBalance) %>% 
  group_by(Department, WorkLifeBalance) %>%
  summarize(count=n()) %>% 
  
  ggplot(aes(x=fct_reorder(WorkLifeBalance, -count), y=count, fill=Department)) + 
  geom_bar(stat='identity') + facet_wrap(~Department) + 
  theme_minimal_hgrid() + 
  theme(legend.position="bottom", 
        plot.title=element_text(hjust=0.5), 
        plot.background=element_rect(fill="white")) + 
  scale_fill_manual(values=c("#f0902a", "#3768b2", "#0a85e5")) + 
  geom_text(aes(label=count, fill = Department), 
             colour = "black", vjust=-0.5) + 
  labs(title="Work Life Balance Environment", 
       x="Work and Life Balance", 
       y="Number of Employees")

by.department



# Digging into Research and Development
r.d <- df %>% select(Department, WorkLifeBalance, Attrition) %>% 
  filter(Department == "Research & Development" & WorkLifeBalance == 1 | WorkLifeBalance == 2) %>%
  group_by(Attrition) %>% summarize(count=n())



# Home Distance from Work
# Average distance of people who did not quit the organization

no.attritions <- df %>% filter(Attrition == "No")

# Average distance of employees that didn't quit.
med.distance <- no.attritions %>% select(DistanceFromHome) %>% 
  summarize(med.dist=round(median(DistanceFromHome), 2))

attritions$Median_Distance <- ifelse(attritions$DistanceFromHome < 7, "Below Average", "Above Average")

# Distribution of both Distance from Work Status
dist <- attritions %>% select(Median_Distance, DistanceFromHome) %>%
  ggplot(aes(x=DistanceFromHome, fill=Median_Distance)) + 
  geom_density() + 
  facet_wrap(~Median_Distance) + 
  theme_minimal() + 
  scale_color_manual(values=c("#2EFE64", "#FA5858")) + 
  scale_fill_manual(values=c("#F6CED8", "#ECF6CE")) +
  theme(legend.position="bottom", 
        plot.background=element_rect(fill="#FFF1E0")) + 
  geom_vline(aes(xintercept=7), color="black", linetype="dashed", size=1) + 
  annotate("text", label = "Median = 7", x = 15, y = 0.17, color = "black")

dist


work_dist_cnt <- attritions %>% select(Median_Distance) %>% group_by(Median_Distance) %>% 
  summarize(count=n()) %>%
  
  ggplot(aes(x=Median_Distance, y=count, color=Median_Distance, fill=Median_Distance)) + 
  geom_bar(stat="identity", position="dodge") +  
  theme_minimal_hgrid() +
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) + 
  geom_text(aes(label=count, fill = Median_Distance), colour = "black", vjust=1.5, size=5) +
  scale_color_manual(values=c("#f0902a", "#0a85e5")) + 
  labs(title = "# Distance from work", x="Distance from Work Status")

work_dist_pct <- attritions %>% select(Median_Distance) %>% group_by(Median_Distance) %>% 
  summarize(count=n()) %>%
  mutate(pct=round(prop.table(count),2) * 100) %>% 
  
  ggplot(aes(x=Median_Distance, y=pct, color=Median_Distance, fill=Median_Distance)) + 
  geom_bar(stat="identity") + 
  theme_minimal_hgrid() +  
  theme(legend.position="none") + 
  geom_text(aes(label=pct, fill = Median_Distance), colour = "black", vjust=1.5, size=5)  + 
  scale_fill_manual(values=c("#f0902a", "#0a85e5")) +
  scale_color_manual(values=c("#f0902a", "#0a85e5")) + 
  labs(title = "% Distance from work", x="Distance from Work Status", y="Percentage (%)")

plot_grid(work_dist_cnt, work_dist_pct, ncol=2)


# Average Monthly Income is for those who have stock option levels and for those who don't

stockoption <- df %>% select(StockOptionLevel, Attrition) %>% 
  group_by(StockOptionLevel, Attrition) %>% summarize(n=n())  %>%
  
  ggplot(aes(x=reorder(StockOptionLevel, -n), y=n, fill=factor(StockOptionLevel))) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  facet_wrap(~Attrition) + 
  theme_minimal_vgrid() + 
  scale_fill_manual(values=c("#c85855", "#de996c", "#96b9ca", "#5877a0")) + 
  guides(fill=guide_legend(title="Stock Option \n Level")) + 
  theme(legend.position="none", 
        plot.background=element_rect(fill="white"), 
        plot.title=element_text(hjust=0.5, color="black"), 
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        axis.title=element_text(colour="black"),
        strip.text.x = element_text(color="black"), 
        legend.text=element_text(color="black"))  + 
  geom_text(aes(label=n, fill = factor(StockOptionLevel)), 
             colour = "black", hjust=-0.5) + 
  labs(title="Number of Employees", x="StockOptionLevel", y="Amount")

# Average income by StockOption using the geom_line()
income_stockoption <- df %>% select(StockOptionLevel, MonthlyIncome, Attrition) %>% 
  group_by(StockOptionLevel, Attrition) %>%
  ggplot(aes(x=MonthlyIncome)) + 
  geom_area(aes(fill=factor(StockOptionLevel)), stat ="bin", bins=100, alpha=0.8) + 
  facet_wrap(~Attrition) + 
  theme_minimal_hgrid() +   
  scale_fill_manual(values=c("#c85855", "#de996c", "#96b9ca", "#5877a0")) + 
  guides(fill=guide_legend(title="Stock Option \n Level")) + 
  theme(legend.position="bottom", 
        plot.background=element_rect(fill="white"), 
        plot.title=element_text(hjust=0.5, color="black"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"),
        axis.title=element_text(colour="black"),
        strip.text.x = element_text(color="black"),
        legend.text=element_text(color="black"), 
        legend.background = element_blank())

plot_grid(stockoption, income_stockoption, nrow=2)



# Percent Attrition by Work Life Balance
# Percent Attrition due to Business Travels
work_bal_cnt <- df %>% select(Attrition, BusinessTravel, WorkLifeBalance) %>% 
  group_by(Attrition, BusinessTravel) %>% 
  summarize(count=n()) %>% mutate(pct=round(prop.table(count),2) * 100) %>%
  
  ggplot(aes(x=Attrition, y=count, 
             fill=BusinessTravel, color=Attrition)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~BusinessTravel) + 
  geom_text(aes(label=count, fill = BusinessTravel), 
             colour = "black", vjust=-0.5)  + 
  theme_minimal_hgrid() + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#f0902a", "#39bc3a", "#0a85e5")) +
  scale_color_manual(values=c("#808080", "#808080")) + 
  labs(title="Attrition by Business Travel of Employees", 
       x="Attrition", y="Number of Employees") + 
  theme(plot.title=element_text(hjust=0.5), 
        plot.background=element_rect(fill="white")) 

work_bal_pct <- df %>% select(Attrition, BusinessTravel, WorkLifeBalance) %>% 
  group_by(Attrition, BusinessTravel) %>% 
  summarize(count=n()) %>% mutate(pct=round(prop.table(count),2) * 100) %>%
  
  ggplot(aes(x=Attrition, y=pct, fill=BusinessTravel, color=Attrition)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~BusinessTravel) + 
  theme_minimal_hgrid() +  
  theme(legend.position="none") + 
  geom_text(aes(label=paste0(pct, "%"), fill = BusinessTravel), 
             colour = "black", vjust=-0.5)  + 
  scale_fill_manual(values=c("#f0902a", "#39bc3a", "#0a85e5")) +
  scale_color_manual(values=c("#808080", "#808080")) + 
  labs(x="Attrition", y="Percentage (%)") + 
  theme(plot.background=element_rect(fill="white"))

plot_grid(work_bal_cnt, work_bal_pct, nrow=2)




# Job Satisfaction as integer again.
df$JobSatisfaction <- as.integer(df$JobSatisfaction)


# Correlation Matrix
options(repr.plot.width=10, repr.plot.height=7) 

nums <- select_if(df, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr, type = "lower", lab = TRUE, lab_size = 3, 
           method="square",  colors = c("red", "white", "blue"), insig = "blank", 
           title="Correlogram Employee Attritions", 
           ggtheme=theme_minimal())



# Bi-variate Analysi.s

# Working Years and Monthly Income
sal_exp <- df %>% select(TotalWorkingYears, MonthlyIncome) %>%
  ggplot(aes(x=TotalWorkingYears, y=MonthlyIncome)) + 
  geom_point(colour = "#ce1d23", alpha=1/2) + 
  geom_smooth(method="loess",color="#e78f92") + 
  theme_minimal_hgrid() + 
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black")) + 
  labs(title="Salary vs Experience")

# Age and Monthly Income
age_salary <-  df %>% select(Age, MonthlyIncome) %>%
  ggplot(aes(x=Age, y=MonthlyIncome)) + 
  geom_point(colour = "#ce1d23", alpha=1/2) + 
  geom_smooth(method="loess", color="#e78f92") + 
  theme_minimal_hgrid() + 
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(hjust=0.5, color="black"), 
        plot.background=element_rect(fill="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black")) + 
  labs(title="Salary vs Age")

# Performance Rating and Salary Hike
rating_hike <-  df %>% select(PerformanceRating, PercentSalaryHike) %>%
  ggplot(aes(x=factor(PerformanceRating), y=PercentSalaryHike)) + 
  geom_boxplot(colour = "#fe632f", fill="#83b3d1") + 
  geom_jitter(color="#e78f92",alpha=1/3)  + 
  theme_minimal_hgrid() + 
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(hjust=0.5, color="black"), 
        plot.background=element_rect(fill="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black")) + 
  labs(title="% Salary Hike vs Performance", 
       x="Performance Rating")

# Years with Current Manager and Years since Last Promotion
curr_mang_prom <-  df %>% select(YearsWithCurrManager, YearsSinceLastPromotion) %>%
  ggplot(aes(x=factor(YearsWithCurrManager), y=YearsSinceLastPromotion)) + 
  geom_boxplot(colour = "#fe632f", fill="#83b3d1") + 
  geom_jitter(color="#e78f92",alpha=1/3) + 
  geom_smooth(method='loess',aes(group=1),color='#EE4037',lty=2,size=.5) + 
  theme_minimal_hgrid() + 
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(hjust=0.5, color="white"), 
        plot.background=element_rect(fill="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black")) + 
  labs(title="Promotions vs Current Manager", 
       x="Years with Current Manager")

plot_grid(sal_exp, age_salary, rating_hike, curr_mang_prom, ncol=2, nrow=2)




# Splitting the Data
set.seed(142)
# # I personally prefer to shuffle my data before splitting.
original_df <- original_df[sample(nrow(original_df)),]

# Let's encode the ordinal variables
original_df$BusinessTravel = factor(original_df$BusinessTravel, 
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'), 
                                    labels = c(1, 2, 3))

# Changing the datatype from integer to factors from the ordinal variables.
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel",
          "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", 
          "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")

original_df[cols] <- lapply(original_df[cols], factor)

# Delete unecessary columns
cols <- c("Over18", "EmployeeNumber", "EmployeeCount")

original_df[cols] <- NULL


# Splitting our data
trainIndex <- createDataPartition(original_df$Attrition, 
                                  p=0.8, list=FALSE, times=1)

train <- original_df[trainIndex,]
test <- original_df[-trainIndex,]


# Checking that both the training and testing sets have the same label proportions.
prop_train <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_train
prop_test



# Classification Tree
options(repr.plot.width=10, repr.plot.height=8) 

rpart.tree <- rpart(Attrition ~ ., data=train)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")


# Feature Importance
var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + 
  geom_bar(stat='identity') + 
  coord_flip() + 
  theme_minimal_vgrid() + 
  theme(legend.position="none", 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(color="black"), 
        plot.background=element_rect(fill="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black"), 
        legend.background = element_rect(fill="white", size=0.5, 
                                         linetype="solid", colour ="black")) + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(24, "Set3"))(colorCount)) + 
  geom_text(aes(label=paste0(importance, "%")), 
             colour = "black", hjust=0.5) + 
  labs(title="Feature Importance for Decision Tree Model", 
       x="Features", y="Importance")

feature_importance


# Confusion Matrix
options(repr.plot.width=8, repr.plot.height=6) 

predictions <- predict(rpart.tree, test, type="class")
conf_df <- data.frame(table(test$Attrition, predictions))

ggplot(data =  conf_df, mapping = aes(x = predictions, y = Var1)) +
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "#84b4d1", high = "#0a85e5") +
  theme_minimal_grid() + 
  theme(legend.position="none", strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="black"), 
        plot.subtitle=element_text(color="black"), 
        plot.background=element_rect(fill="white"), 
        axis.text.x=element_text(colour="black"), 
        axis.text.y=element_text(colour="black"), 
        axis.title=element_text(colour="black"),
        legend.background = element_blank()) + 
  labs(title="Confusion Matrix", y="Attrition Status", x="Predictions")




# Pruning reduces the size of decision trees by removing parts of the tree 
# that do not provide power to classify instances
prune.rpart.tree <- prune(rpart.tree, cp=0.02)
plot(prune.rpart.tree, uniform=TRUE, branch=0.6)
text(prune.rpart.tree, all=TRUE, use.n=TRUE)



rparty.tree <- as.party(rpart.tree)
rparty.tree

options(repr.plot.width=12, repr.plot.height=12) 
plot(rparty.tree)

options(repr.plot.width=12, repr.plot.height=12) 
fancyRpartPlot(rpart.tree)




colnames(df)

# Delete the following columns
delete <- c("Generation", "Educational_Levels", "CatYearsManager")
df[delete] <- NULL
head(df)



# Automated Machine Learning with H2o:

h2o.init()

# Putting the original dataframe into an h2o format
h2o_df <- as.h2o(df)

# Splitting into training, validation and testing sets
split_df <- h2o.splitFrame(h2o_df, c(0.7, 0.15), seed=12)

# Obtaining our three types of sets into three separate values
h2o_train <- h2o.assign(split_df[[1]], "train")
h2o_validation <- h2o.assign(split_df[[2]], "validation")
h2o_test <- h2o.assign(split_df[[2]], "test")

h2o.describe(h2o_train)

# Establish X and Y (Features and Labels)
y <- "Attrition"
x <- setdiff(names(h2o_train), y)

auto_ml <- h2o.automl(y = y, x = x, training_frame = h2o_train, 
                      leaderboard_frame = h2o_validation, project_name = "Attrition", 
                      max_models = 10, seed = 12)


# Check for the top models
top_models <- auto_ml@leaderboard
print(top_models)


# Get the best model

# Our aim is to determine the feature importance
model_id <- as.data.frame(top_models$model_id)[,1]
best_family <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_id, value=TRUE)[1])
obtain_model <- h2o.getModel(best_family@model$metalearner$name)



# How important is each model to the StackEnsemble
h2o.varimp(obtain_model)

options(repr.plot.width=8, repr.plot.height=4) 
h2o.varimp_plot(obtain_model)


xgb <- h2o.getModel(grep("XGBoost", model_id, value = TRUE)[1])

# Variable importance of the top XGBoost model
h2o.varimp(xgb)
h2o.varimp_plot(xgb)
