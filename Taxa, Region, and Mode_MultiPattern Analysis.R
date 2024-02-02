---
#Analyses for CASA Tropics Diversification Review

#Installing required packages and loading data
##PACKAGES
  ###"read(xl)"
  ###"car"
  ###"emmeans"
  ###"effectsize"
  
# Load read(xl) library
library(read(xl))

# Load ANOVA data ("Kang_Literature Review_ANOVA.xlsx")
table_ANOVA <- read_excel("Kang_Literature Review_ANOVA.xlsx", 
           sheet = "ANOVA TABLE")

View(table_ANOVA)

# Prepare Data Set by taking out summary rows
table_ANOVA[-seq(8,288, by = 8),]
table_ANOVA_edit <- table_ANOVA[-seq(8,288, by = 8),]
summary(table_ANOVA_edit)

#checking the omitted values
summary(table_ANOVA)
complete.cases(table_ANOVA)
sum(complete.cases(table_ANOVA)) #complete cases tells a wrap of the number of true and false
sum(complete.cases(table_ANOVA_edit)) #tells you the row you have and want

#variable labels
#example of response variable: diversification event
#turning character variables into factor

table_ANOVA_edit$`BIOGEOGRAPHIC REGIONS`<- as.factor(table_ANOVA_edit$`BIOGEOGRAPHIC REGIONS`)
summary(table_ANOVA_edit)
#turning the character into factor so it gives more details 

#do it for the rest of the character variables/columns
table_ANOVA_edit$TAXA <- as.factor(table_ANOVA_edit$TAXA)
table_ANOVA_edit$`DIVERSIFICATION MODE`<- as.factor(table_ANOVA_edit$`DIVERSIFICATION MODE`)
summary(table_ANOVA_edit)

#ANOVA on all variables
#ANOVA code
#CODE FOR JUST MAIN EFFECTS
anova_result_main <-aov(`NUMBER OF DIVERSIFICATION EVENTS`~(`BIOGEOGRAPHIC REGIONS`+`TAXA`+`DIVERSIFICATION MODE`), data = table_ANOVA_edit)
anova_result_main
summary(anova_result_main)

#CODE FOR INTERACTIVE EFFECTS: NETWORK EFFECT AND COMBINATION EFFECT TEST
#NETWORK EFFECT
options(contrast = c("contr.poly"))
anova_result_network <- aov(`NUMBER OF DIVERSIFICATION EVENTS`~
                                `BIOGEOGRAPHIC REGIONS`*`TAXA`*`DIVERSIFICATION MODE`, 
                                data = table_ANOVA_edit)
anova_result_network
#results were inconclusive: no residuals tab and the value of the network ratio is the same as the residuals values
#moving forward without looking at interactive network effects, just combinations and such


#COMBINATION EFFECT
anova_result_interactive <- aov(`NUMBER OF DIVERSIFICATION EVENTS`~(`BIOGEOGRAPHIC REGIONS`*`TAXA`*`DIVERSIFICATION MODE`)-(
                      `BIOGEOGRAPHIC REGIONS`:`TAXA`:`DIVERSIFICATION MODE`), data = table_ANOVA_edit)
anova_result_interactive

#checking that the order of input doesn't affect the output
#Load car package
library(car)
car::Anova(anova_result_interactive, type = 3)
summary(anova_result_interactive)


#TESTING ANOVA ANALYSIS RESULTS FOR FINER GRAIN DETAILS ON INTERACTIONS AND RELATIONSHIPS
#emmeans test for post hoc pairwise Tukey Test
#load emmeans package
library(emmeans)

#TAXA AND DIVERSIFICATION MODE
emm_t_dm <- emmeans(anova_result_interactive, pairwise ~ `TAXA`|`DIVERSIFICATION MODE`, adjust= "Tukey")

#BIOGEOGRAPHIC REGIONS AND TAXA
emm_t_br <- emmeans(anova_result_interactive, pairwise ~ `BIOGEOGRAPHIC REGIONS`|`TAXA`, adjust= "Tukey")

#BIOGEOGRAPHIC REGIONS AND DIVERSIFICATION MODE
emm_br_dm <- emmeans(anova_result_interactive, pairwise ~ `BIOGEOGRAPHIC REGIONS`|`DIVERSIFICATION MODE`, adjust= "Tukey")

emm_t_dm
emm_t_br
emm_br_dm

plot(emm_t_dm)
plot(emm_t_br)
plot(emm_br_dm)


#what is the proportional variability and breakdown of these effects on the diversification event outcomes?
#What factor/effect is driving diversification events more/most?

#effectsize test for proportion of variability info: eta_squared and omega_squared
#load effectsize package
library(effectsize)
eta_squared(car::Anova(anova_result_interactive, type = 3), partial = FALSE)
omega_squared(car::Anova(anova_result_interactive, type = 3), partial = FALSE)

#omega squared vs eta squared
#eta squared can be biased
#ETA2 values reflect the percentage proportion of variability in diversification event outcome explained by each of these effects
#omega squared is the unbiased estimator of proportions variance explained *the correction*


#further isolating where the differences are (joint_tests package from emmeans)
joint_tests(anova_result_interactive, by= "BIOGEOGRAPHIC REGIONS")
joint_tests(anova_result_interactive, by= "TAXA")
joint_tests(anova_result_interactive, by= "DIVERSIFICATION MODE")
            
#citing version of R and packages used for analysis
version$version.string
citation("car")
citation("emmeans")
citation("effectsize")
citation("readxl")
packageVersion("car")
packageVersion("emmeans")
packageVersion("effectsize")
packageVersion("readxl")
