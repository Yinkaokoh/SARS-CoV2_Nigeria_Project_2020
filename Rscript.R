#RScript 
##SARS-CoV2 Research Project in Nigeria using data on GISAID, worldometer.com and NCDC
##In collaboration with Nidia (USA), Nicholas and Ann (Ghana) and Elijah et al (Nigeria)
## script written by Olayinka Sunday OKOH - Sept 2020 to January 2021

library(naijR) # This is to load a package specifically made for Nigeria Map
library(readxl)
library(ggplot2)


map_ng() #to draw Nigeria Map with States boundaries map_np(NULL) give map without State boundaries
#map_ng(states("sw"), show.text = TRUE, col = 4) gives map of South West

#load the data 
Nigeria_NCDC <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/Nigeria_State_NCDC.xlsx")

#convert to dataframe
Nigeria_NCDC.DF <- as.data.frame(Nigeria_NCDC)

#break the cases into categories
bb <- c(0,50, 100,500, 1000, 5000, 10000, 25000) 
pmb <- c(0, 50, 100, 500, 1000, 2000) ##breaks for per million infected 
pb <- c(0, 100, 500, 1000, 5000, 10000, 50000, 100000, 200000) ##breaks for positive cases
ppb <- c(0, 2, 5, 10, 15, 20) ##breaks for percentage positive cases
ppt <- c(0, 0.25, 0.5,0.75, 1.0, 3)
#Map showing the distribution of Covid 19 across States in Nigeria as reported by NCDC
pdf("PLOTS/Nigeria_Covid_Cases.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC$NCDC_Report, breaks = bb, col = "blue", title = "SARS-COV2 Distribution in Nigeria (NCDC, 2020)") 
dev.off()

#Map showing the number of test conducted
pdf("PLOTS/Nigeira_Tests.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC$Total_Tests, breaks = pb, col = "green", title = "Total number of SARS-COV2 tests conducted across Nigeria (NCDC, 2020)") 
dev.off()

#Map showing the percentage test positive
pdf("PLOTS/Nigeria_Percentage_Tests_positive.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC$Percentage_Positive, breaks = ppb, col = "orange", title = "Percentage SARS-COV2 tests positive across Nigeria (NCDC, 2020)") 
dev.off()

#Map showing the percentage of population tested
pdf("PLOTS/Nigeria_Perentage_population_tested.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC$Percentage_Popn_Tested, breaks = ppt, col = "blue", title = "Percentage of population tested for SARS-COV2 across Nigeria (NCDC, 2020)") 
dev.off()


#Map showing No of persons infected in each state per million population
pdf("PLOTS/Nigeria_Infected_per_million_population_tested.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC$Infected_Per_Million, breaks = pmb, col = "blue", title = "SARS-COV2 Per Million Population in Nigeria (NCDC, 2020)") 
dev.off()
##ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Covid_Cases_Per_Million.pdf")

#Map showing how SARS Cov2 Sequences from Nigeria were submitted in GISAID
pdf("PLOTS/Nigeria_Sequences_submitted_GISAID.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC.DF$GISAID_Submitted, breaks = c(0, 5, 10, 15, 20), col = "green", title = "SARS-COV2 Sequences  from Nigeria submitted in GISAID") 
dev.off()
##ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Covid_Submitted.pdf")

#Map showing percentage Reported cases submitted as sequence on GISAID
pdf("PLOTS/Nigeria_Percentage_Sequences_submitted_GISAID.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC.DF$Percentage_submitted_GISAID, breaks = c(0, 0.09, 0.1, 0.25, 0.5, 1.0, 1.5, 2.0), col = "green", title = "SARS-COV2 Sequences submitted on GISAID per 100 cases in Nigeria") 
dev.off()

#Map showing percentage population infected with SARS-CoV2 
pdf("PLOTS/Nigeria_Percentage_Population_infected.pdf")
map_ng(region = Nigeria_NCDC.DF$State, x = Nigeria_NCDC.DF$Percent_Infected, breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 0.18 ), col = "red", title = "Number of SARS-CoV2 cases per 100 persons") 
dev.off()





#Bar chart showing the distribution of SARSCoV2 in Nigeria
ggplot(Nigeria_NCDC.DF, aes(y = reorder(State, NCDC_Report), x = NCDC_Report)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "SARSCoV2 cases reported in various states in Nigeria", y = "State", x = "NCDC Reported Cases", caption = "Source: NCDC 2020")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/NigeriaCases_bar.pdf")


#Bar chart showing the number of infected persons per million population
ggplot(Nigeria_NCDC.DF, aes(y = reorder(State, Infected_Per_Million), x = Infected_Per_Million)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "SARSCoV2 reported per million in various states in Nigeria", y = "State", x = "Number of Infected Persons per million", caption = "Source: NCDC 2020")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/NigeriaInfectedPerMillion_bar.pdf")

#Bar chart showing the number of infected persons per ten thousand population
ggplot(Nigeria_NCDC.DF, aes(y = reorder(State, Infected_Per_10000), x = Infected_Per_10000)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "SARSCoV2 reported per ten thousand in various states in Nigeria", y = "State", x = "Number of Infected Persons per ten thousand" , caption = "Source: NCDC 2020")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/NigeriaInfectedPer10000_bar.pdf")

#Bar chart showing the total number of SARS-CoV2 Tests conducted across Nigeria
ggplot(Nigeria_NCDC.DF, aes(y = reorder(State, Total_Tests), x = Total_Tests)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Total number of SARS-CoV2 tests conducted across Nigeria", y = "State", x = "Number of tests conducted" , caption = "Source: NCDC 2020")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/NigeriaSARSCoV2Tests_bar.pdf")


#Bar chart showing the percentage SARS-CoV2 Tests positive across Nigeria
ggplot(Nigeria_NCDC.DF, aes(y = reorder(State, Percentage_Positive), x = Percentage_Positive)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Percentage SARS-CoV2 tests positive across Nigeria", subtitle = "Number of positive cases in every 100 tests conducted", y = "State", x = "Percentage" , caption = "Source: NCDC 2020")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/NigeriaPercentagePositiveTests_bar.pdf")


#Bar chart showing the percentage of population tested for SARS-CoV2 
ggplot(Nigeria_NCDC.DF, aes(y = reorder(State, Percentage_Popn_Tested), x = Percentage_Popn_Tested)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Percentage of population tested for SARS-CoV2 across Nigeria", subtitle = "Number of persons tested out of every 100 persons across Nigeria", y = "State", x = "Percentage" , caption = "Source: NCDC 2020")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Percentage_Popn_Tested_bar.pdf")


#Chart of Infected vs population in Nigeria
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = NCDC_Report)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) +
  labs(title = " SARS-CoV2 Infection in Nigeria", subtitle = "Plot of Population versus number of NCDC Reported COViD-19 cases", caption = "Data Source: NCDC, 2020", y = "Population (x1,000,000)", x ="NCDC Reported Cases") +
  theme_bw()
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_NCDC_Cases_Point.pdf")


#Chart showing no of persons infected per million in Nigeria
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = Infected_Per_Million)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = " SARS-CoV2 per one million population in Nigeria", subtitle = "Plot of population versus number of persons infected per one million persons in States across Nigeria", caption = "Data Source: NCDC, 2020", y = "Population", x ="Persons infected per Million")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Infected_Per_Million_point.pdf")


#Chart showing no of persons infected per ten thousand in Nigeria
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = Infected_Per_10000)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = " SARS-CoV2 Per Ten Thousand Population in Nigeria", subtitle = "Plot of Population versus number of Persons Infected Per Ten Thousand", caption = "Data Source: NCDC, 2020", y = "Population", x ="Persons infected per Ten Thousand")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Infected_Per_Ten_Thousand.pdf")


#graph of Infected vs Submission to GISAID
ggplot(Nigeria_NCDC.DF, aes(y = NCDC_Report, x = GISAID_Submitted)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = " SARS-CoV2 Submitted to GISAID from in Nigeria", subtitle = "Plot of NCDC Reported Cases versus number of Sequences submitted to GISAID", caption = "Data Source: NCDC, 2020 and GISAID, 2020", y = "Cases Reported by NCDC", x ="Sequences submitted to GISAID")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_GISAID_Submitted_Cases.pdf")


#Chart showing Total number of test conducted in relation to population
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = Total_Tests)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = "Total number of SARS-CoV2 tests conducted across Nigeria", subtitle = "Number of tests conducted in relation to population", caption = "Data Source: NCDC, 2020", y = "Population (X million)", x ="Number of tests conducted")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Total_Test_with_Population.pdf")


#Chart showing percentage positive test in relation to population
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = Percentage_Positive)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = "Percentage of SARS-CoV2 tests positive across Nigeria", subtitle = "Number of SARS-CoV2 cases in every 100 tests conducted across Nigeria", caption = "Data Source: NCDC, 2020", y = "Population (x million)", x ="Percentage of SARS-CoV2 tests positive")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Percentage_Positive_in_relation_to_Population.pdf")


#Chart showing percentage positive test in relation to the Number of Tests conducted
ggplot(Nigeria_NCDC.DF, aes(y = Total_Tests, x = Percentage_Positive)) + 
  geom_point(size = 2, color = 'green') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = "Percentage of SARS-CoV2 tests positive across Nigeria", subtitle = "Number of SARS-CoV2 cases in every 100 tests conducted across Nigeria", caption = "Data Source: NCDC, 2020", y = "Total number of tests", x ="Percentage of SARS-CoV2 tests positive")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Percentage_Positive_in_relation_to_Total_Tests.pdf")


#Chart showing total number of tests in relation to population
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = Total_Tests)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = "Total number of tests conducted in relation to population", subtitle = "Plot of the relationship between population and total number of tests conducted", caption = "Data Source: NCDC, 2020", y = "Population (x million)", x ="Total number of tests conducted")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_Total_Tests_in_relation_to_Total_Tests.pdf")


#Chart showing percentage of population tested for SARS-CoV2 in relations to population
ggplot(Nigeria_NCDC.DF, aes(y = Population, x = Percentage_Popn_Tested)) + 
  geom_point(size = 2, color = 'blue') + 
  geom_text(label = Nigeria_NCDC.DF$State) + theme_bw() +
  labs(title = "Percentage of population tested for SARS-CoV2 across Nigeria", subtitle = "Plot of the relationship between population and percentage population tested", caption = "Data Source: NCDC, 2020", y = "Population (x million)", x ="Percentage of population tested")
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/Nigeria_PercentagePopn_Tested_in_relation_to_Population.pdf")



##Including lockdown dates
NigGha_TimeSeries <- read_excel("C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/NigGha_TimeSeries.xlsx")
NigGha_TimeSeries <- as.data.frame(NigGha_TimeSeries)
NigGha_TimeSeries$NewDate <- as.Date(NigGha_TimeSeries$Date) ##This is to change the date from POXITct to string

ggplot(NigGha_TimeSeries, aes(x = NewDate, y = Cases, color = Country)) +
  geom_line() + geom_vline(xintercept = as.Date(c("2020-03-30","2020-07-01", "2020-10-01"))) +
  labs(title = "SARS-CoV-2 Trends within lockdown dates", y = "Number of Reported cases", x = "Date", caption = "Source: NCDC 2020", subtitle = "bar lines indicate lockdown dates"  )
ggsave(file = "C:/Users/YinkaOkoh/Desktop/Bioinformatics_Data/SARS-CoV2_Project_2021/SARS-CoV2_Nigeria_Project_2020/PLOTS/NigGha_lockdown_dates.pdf")



