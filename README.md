# Employee Attritions

## **Introduction :**
### **What is Attrition and what determines it?** <br>
Attrition: It is basically the turnover rate of employees inside an organization.

This can happen for many reasons: <br>
1. Employees looking for better opportunities.
2. A negative working environment.
3. Bad management.
4. Excessive working hours.

### **Structure of the Project:** <br>
This project will be structured in the following way: <br>
- **Questions:** Questions will be asked previous to the visualization to make sure the visualizations shown in this project are insightful.
- **Summary:** After each section I will provide a summary to understand what we got from the visualizations.
- **Recommendations:** What recommendations could be given to the organization to reduce the attrition rate.

## **Table of Contents :** <br>

**I. Summary of our Data:** <br>

**II. Gender Analysis:** <br>
- Age Distribution by Gender <br>
- Job Satisfaction Distribution by Gender <br>
- Monthly Income by Gender <br>


**III. Analysis by Generation and Education:** <br>
- Understanding Attrition by Generation <br>
- Attrition by Educational Level <br>


**IV. The Impact of Income towards Attrition:** <br>
- Average income by Department <br>
- Satisfaction by Income <br>
- Income and the Levels of Attrition <br>
- Average Daily Rates and Percent Difference <br>
- Attrition by Overtime <br>


**V. Working Environment:** <br>
- Mean Salary by Job Role <br>
- Attrition by Job Role <br>
- Current Managers and Average Satisfaction Score <br>
- Average Environment Satisfaction <br>
- Distance from Work Status <br>
- Employees have Stockoption levels <br>
- Attrition due to Business Travels <br>


**VI. Analysis and Models** <br>
- Correlation Matrix <br>
- Bi-Variate Analysis <br>
- Decision Trees <br>
- Feature Importance <br>
- Confusion Matrix <br>

**VIII. Conclusion:** <br>
- Top Reasons why Employees Leave the Organization <br>

<br>
<br>

## **Summary of our Data:**
 
- **Dataset Structure:** 1470 observations (rows), 35 features (variables)
Missing Data: Luckily for us, there is no missing data! this will make it easier to work with the dataset. <br>
- **Data Type:** We only have two datatypes in this dataset: factors and integers
Label" Attrition is the label in our dataset and we would like to find out why employees are leaving the organization! <br>
- **Imbalanced dataset:** 1237 (84% of cases) employees did not leave the organization while 237 (16% of cases) did leave the organization making our dataset to be considered imbalanced since more people stay in the organization than they actually leave. <br>

 <br>

 
### **Distribution of our Labels :** <br>
This is an important aspect that we are dealing with an imbalanced dataset will help us determine what will be the best approach to implement our predictive model. 84% of employees did not quit the organization while 16% did leave the organization.
<br>

![plot_ly](./images/01-distr-attrition.png)

## **Gender Analysis :** <br>
In this section, we will try to see if there are any discrepancies between male and females in the organization. Also, we will look at other basic information such as the age, level of job satisfaction and average salary by gender.

### **Distribution of the Age of our employees :** <br>
The average age of females is 37.33 and for males is 36.65 and both distributions are similar.
<br>

![plot_ly](./images/02-age-distr.png)

<br>

### **Distribution of Job Satisfaction :** <br>
For individuals who didn't leave the organization, job satisfaction levels are practically the same. However, for people who left the organization , females had a lower satisfaction level as opposed to males.
<br>

![plot_ly](./images/0.3-distr-jobsatisf.png)

<br>

### **Monthly Income by Gender :**
The average salaries for both genders are the same with males having an average of 6380.51 and females 6686.57.

<br>

![plot_ly](./images/04-sal-gen.png)
<br>
![plot_ly](./images/05-dept-gen.png)


<br>

## **Generation and Education :**
Each type of generation have their particular peculiarities and that is why we should explore in this dataset.

### **Generational Behavior :** <br>
- Most millenials are still relatively young, so that explains why the number of companies for millenials is relatively low however. <br>
- It seems that millenials are the ones with the highest turnover rate, followed by the boomers. 
- The newer generation which are the millenials opt to look more easy for other jobs that satisfy the needs on the other side we have the boomers which are approximating retirement and could be one of the reasons why the turnover rate of boomers is the second highest.
<br>

![plot_ly](./images/06-gen.png)

<br>

### **Attrition by Educational Level :** <br>
The bachelors are the ones showing the highest level of attrition which makes sense since Millenials create the highest turnover rate inside the organization.
<br>

![plot_ly](./images/07-edu-attr.png)

<br>

## **Impact of Income towards Attrition :** <br>

### **Average Income by Department:**
There is a huge differences in each department by attrition.
<br>

![plot_ly](./images/08-inc-dept.png)

<br>

### **Satisfaction by Income :**
The lower the job satisfaction the wider the gap by attrition status in the levels of income.<br>

![plot_ly](./images/09-inc-satisf.png)

<br>

### **Income and the Level of Attrition:** <br>
This might indicate that at least for the these roles, the sample population that left the organization was mainly because of income.
<br>

![plot_ly](./images/10-inc-hike-rat.png)

<br>

### **Average and Percent Difference of Daily Rates :**
**HealthCare Representatives** , **Sales Representatives** , and **Research Scientists** have the highest daily rates differences in terms of employees who quit or didn't quit the organization.
<br>

![plot_ly](./images/11-daily-rate.png)

<br>

### **Attrition due to Overtime :**
Over 54% of workers who left the organization worked overtime!!!

<br>

![plot_ly](./images/12-overtime.png)

<br>

## **Working Environment :**
In this section, we will explore everything that is related to the working environment and the structure of the organization. <br>

### **Mean Salary by JobRole :**
**Managers** and **Research Directors** have the highest salary on average.
<br>

![plot_ly](./images/13-sal-jobrole.png)

<br>

### **Attrition by Job Role :**
Sales Representatives, HealthCare Representatives and Managers have the highest attrition rates. This could give us a hint that in these departments we are experiencing certain issues with employees.<br>

![plot_ly](./images/14-attr-jobrole.png)

<br>

### **Current Managers and Average Satisfaction Score:**
Employees that are dealing with recently hired managers have a lower satisfaction score than managers that have been there for a longer time. <br>

![plot_ly](./images/15-mang-satisf.png)

<br>

### **Average Environment Satisfaction:**
managers and healthcare representatives are dealing with a lower working environment however, we don't see the same with sales representatives that could be because most sales representatives work outside the organization. <br>

![plot_ly](./images/16-envr-satisf.png)

<br>

![plot_ly](./images/17-worklife-env.png)

<br>

### **Distance from Work Status :**



![plot_ly](./images/18-distance-work.png)

<br>

### **Employees have Stockoption levels :**



![plot_ly](./images/19-stockoption.png)

<br>

### **Attrition due to Business Travels :**



![plot_ly](./images/20-travel.png)

<br>

## **Correlation Matrix :**
In this section we will understand what features have a positive correlation with each other. This tells us whether there is an association between two variables.<br>

![plot_ly](./images/21-corrplot.png)

<br>

## **Bi-Variate Analysis :**

<br>

![plot_ly](./images/22-bivar.png)

<br>

## **Decision Trees :**


![plot_ly](./images/23-class-tree.png)

## **Feature Importance :**

![plot_ly](./images/24-feature-imp.png)

## **Confusion Matrix :**

![plot_ly](./images/25-conf-mat.png)

## **Conclusion :**
Top Reasons why Employees leave the Organization:
- **Monthly Income:** Income is a huge factor as why employees leave the organization in search for a better salary.
- **No Overtime:** This was a surpirse, employees who don't have overtime are most likely to leave the organization. This could be that employees would like to have a higher amount of income or employees could feel that they are underused.
- **Age:** This could also be expected, since people who are aiming to retire will leave the organization.<br>
Knowing the most likely reasons why employees leave the organization, can help the organization take action and reduce the level of Attrition inside the organization.