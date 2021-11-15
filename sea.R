library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library("rio")

rm(list=ls())
#setwd("C:/Users/User/Documents/GitHub/wiot") 

################################### Socioeconomic accounts associated with the WIOT Database

### List of variables and description

# GO	Gross output by industry at current basic prices (in millions of national currency)
# II	Intermediate inputs at current purchasers' prices (in millions of national currency)
# VA	Gross value added at current basic prices (in millions of national currency)
# EMP	Number of persons engaged (thousands)
# EMPE	Number of employees (thousands)
# H_EMPE	Total hours worked by employees (millions)
# COMP	Compensation of employees (in millions of national currency)
# LAB	Labour compensation (in millions of national currency)
# CAP	Capital compensation (in millions of national currency)
# K	Nominal capital stock (in millions of national currency)

### Prices	
# GO_PI	Price levels gross output, 2010=100
# II_PI	Price levels of intermediate inputs, 2010=100
# VA_PI	Price levels of gross value added, 2010=100

### Volumes	
# GO_QI	Gross output, volume indices, 2010=100
# II_QI	Intermediate inputs, volume indices, 2010=100
# VA_QI	Gross value added, volume indices, 2010=100


### OUR VARIABLES:
### Variable construction

# 2 measures of surplus value: CAP and (VA-LAB)
### Rate of profit_1= CAP/K = (Output-Capital ratio)(Profit share)=(VA/K)*(CAP/VA)
### Rate of profit_2= (VA-LAB)/K

### Rate of surplus value= CAP/LAB
### Organic composition of capital= K/LAB

#Import Socioeconomic accounts
sea<-import("WIOD_SEA_Nov16.xlsx", sheet=2) 


# Get it in tidy format 
sea<-sea %>% pivot_longer("2000":"2014","year", values_to="value")
sea<-sea %>% pivot_wider(names_from = 'variable')

#Calculate relevant variables at the industry level:
sea.v<-sea %>% mutate(o.c.ratio=VA/K,
                      profit.share.1=CAP/VA,
                      profit.share.2=(VA-LAB)/VA,
                      profit.rate.1=o.c.ratio*profit.share.1,
                      profit.rate.2=o.c.ratio*profit.share.2,
                      labor.share=LAB/VA,
                      r.surplus.1=CAP/LAB,
                      r.surplus.2=(VA-LAB)/LAB,
                      org.comp=K/LAB)

#At the national level: just add the capital stock, profits, labor compensation, etc. into national aggregates
sea.n<-sea %>%group_by(year, country) %>% summarize(across(c(LAB, K, CAP, VA), sum))

#Calculate relevant variables
sea.n.v<-sea.n %>%  mutate(o.c.ratio=VA/K,
                           profit.share.1=CAP/VA,
                           profit.share.2=(VA-LAB)/VA,
                           profit.rate.1=o.c.ratio*profit.share.1,
                           profit.rate.2=o.c.ratio*profit.share.2,
                           labor.share=LAB/VA,
                           r.surplus.1=CAP/LAB,
                           r.surplus.2=(VA-LAB)/LAB,
                           org.comp=K/LAB)

