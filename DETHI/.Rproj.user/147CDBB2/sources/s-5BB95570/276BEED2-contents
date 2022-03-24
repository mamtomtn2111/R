library(readr)
library(tidyverse)
library(dslabs)
library(dplyr)
library(skimr)



employee <- readRDS("G:\\RCode\\employee_data.rds")
view(employee)
is.factor(employee$left_company)
names(employee)
unique(employee$job_level)
skim(employee, left_company, salary, department)

SalesLeftCompany <-  filter(employee,left_company == "Yes", department == "Sales" )
filter(employee, department %in% c('Sales', 'Marketing')) %>% 
  head()

# Tìm ra các nhân viên có lương trên 80000 và làm ở bộ phận Sales hoặc Marketing
filter(employee, salary > 80000, department %in% c('Sales', 'Marketing')) %>% 
  head()

select(employee, contains('job')) %>% 
  head()

# Khởi tạo một biến mới theo z-score đặt tên là hours_scaled
employee %>% 
  mutate(hours_scaled = (weekly_hours - mean(weekly_hours)) / sd(weekly_hours)) %>% 
  filter(left_company == 'Yes') %>% 
  summarise(avg_weekly_hours = mean(hours_scaled))


arrange(employee, left_company, salary) %>% 
  head()

employee_data_scaled <-  mutate(employee, 
                                salary_scaled = (salary - mean(salary))/sd(salary)) 
select(employee_data_scaled, salary, salary_scaled) %>% 
  head()

#Salary left employee
employee %>% 
  mutate(employee, 
         salary_scaled = (salary - mean(salary))/sd(salary)) %>% 
  filter(left_company == 'Yes') %>% 
  summarise(avg_weekly_salary = mean(salary_scaled))

#miles_from_home
employee %>% 
  mutate(employee, 
         distances_scaled = (miles_from_home - mean(miles_from_home))/sd(miles_from_home)) %>% 
  filter(left_company == 'No') %>% 
  summarise(avg_distances = mean(distances_scaled))

#Summarize salary
employee %>% 
  group_by(department) %>% # Sắp xếp theo nhóm phòng ban
  summarise(average_salary = mean(salary)) # Thống kê theo từng nhóm

employee %>% 
  group_by(department) %>% 
  summarise(number_employee = n())

employee %>% 
  group_by(marital_status) %>% 
  summarise(counts = n())

employee %>% 
  group_by(left_company, job_level) %>% 
  summarise(employee = n()) %>% 
  ungroup() %>% 
  mutate(percent_of_total_employee = 100*(employee/sum(employee)))





