#ANOVA on all variables


# Load required library
library(readxl)
table_ANOVA <- read_excel("C://Users//Annette//Desktop/Kang_Literature Review_ANOVA.xlsx", 
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
#response variable: diversification event
#turning character variables into factor

table_ANOVA_edit$`BIOGEOGRAPHIC REGIONS`<- as.factor(table_ANOVA_edit$`BIOGEOGRAPHIC REGIONS`)
summary(table_ANOVA_edit)
#turning the character into factor so it gives more details 

#do it for the rest of the character variables/columns
table_ANOVA_edit$TAXA <- as.factor(table_ANOVA_edit$TAXA)
table_ANOVA_edit$`DIVERSIFICATION MODE`<- as.factor(table_ANOVA_edit$`DIVERSIFICATION MODE`)
summary(table_ANOVA_edit)


#ANOVA code
#CODE FOR JUST MAIN EFFECTS
anova_result_main <-aov(`NUMBER OF DIVERSIFICATION EVENTS`~(`BIOGEOGRAPHIC REGIONS`+`TAXA`+`DIVERSIFICATION MODE`), data = table_ANOVA_edit)
anova_result_main
summary(anova_result_main)

options(contrast = c("contr.poly"))

anova_result_interactive <- lm(`NUMBER OF DIVERSIFICATION EVENTS`~(`BIOGEOGRAPHIC REGIONS`*`TAXA`*`DIVERSIFICATION MODE`)-(
                      `BIOGEOGRAPHIC REGIONS`:`TAXA`:`DIVERSIFICATION MODE`), data = table_ANOVA_edit)
anova_result_interactive

#checking that the order of input doesn't affect the output
#install car package
library(car)
car::Anova(anova_result_interactive, type = 3)

summary(anova_result_interactive)
# anova_result
# Call:
#   aov(formula = `NUMBER OF DIVERSIFICATION EVENTS` ~ `BIOGEOGRAPHIC REGIONS` * 
#         TAXA * `DIVERSIFICATION MODE` - `BIOGEOGRAPHIC REGIONS`:TAXA:`DIVERSIFICATION MODE`, 
#       data = table_ANOVA_edit)
# 
# Terms:
#   `BIOGEOGRAPHIC REGIONS`     TAXA `DIVERSIFICATION MODE` `BIOGEOGRAPHIC REGIONS`:TAXA `BIOGEOGRAPHIC REGIONS`:`DIVERSIFICATION MODE`
# Sum of Squares                 1474.067 4892.877               5627.262                      804.242                                       2899.738
# Deg. of Freedom                       5        5                      6                           25                                             30
# TAXA:`DIVERSIFICATION MODE` Residuals
# Sum of Squares                     3843.262  1503.452
# Deg. of Freedom                          30       150
# 
# Residual standard error: 3.165915
# Estimated effects may be unbalanced



#given the residuals, testing for network effect
# anova_result_interactive <- aov(`NUMBER OF DIVERSIFICATION EVENTS`~`BIOGEOGRAPHIC REGIONS`*`TAXA`*`DIVERSIFICATION MODE`, 
#                                 data = table_ANOVA_edit)
# anova_result_interactive
# Call:
#   aov(formula = `NUMBER OF DIVERSIFICATION EVENTS` ~ `BIOGEOGRAPHIC REGIONS` * 
#         TAXA * `DIVERSIFICATION MODE`, data = table_ANOVA_edit)
# 
# Terms:
#   `BIOGEOGRAPHIC REGIONS`     TAXA `DIVERSIFICATION MODE` `BIOGEOGRAPHIC REGIONS`:TAXA `BIOGEOGRAPHIC REGIONS`:`DIVERSIFICATION MODE`
# Sum of Squares                 1138.544 4950.067               5535.937                      569.623                                       2566.873
# Deg. of Freedom                       5        5                      6                           25                                             30
# TAXA:`DIVERSIFICATION MODE` `BIOGEOGRAPHIC REGIONS`:TAXA:`DIVERSIFICATION MODE`
# Sum of Squares                     3783.349                                            1386.127
# Deg. of Freedom                          30                                                 150
#results were inconclusive: no residuals tab and the value of the network ratio is the same as the residuals values



#moving forward without looking at interactive network effects, just combinations and such



#CODE FOR INTERACTIONS EFFECTS
#response variable (y) ~ A+B+C + A:B +B:C +A:C + A:B:C
#r could also write as (y)~A*B*C
#y~A*B+C (main effects plus combo of A:B)
summary(anova_result)

# summary(anova_result)
# #                                                 Df Sum Sq Mean Sq F value   Pr(>F)    
# `BIOGEOGRAPHIC REGIONS`                          5   1474   294.8  29.414  < 2e-16 ***
#   TAXA                                             5   4893   978.6  97.633  < 2e-16 ***
#   `DIVERSIFICATION MODE`                           6   5627   937.9  93.572  < 2e-16 ***
#   `BIOGEOGRAPHIC REGIONS`:TAXA                    25    804    32.2   3.210 5.49e-06 ***
#   `BIOGEOGRAPHIC REGIONS`:`DIVERSIFICATION MODE`  30   2900    96.7   9.644  < 2e-16 ***
#   TAXA:`DIVERSIFICATION MODE`                     30   3843   128.1  12.781  < 2e-16 ***
#   Residuals                                      150   1503    10.0                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#USING ANOVA Results wtih emmeans
install.packages("emmeans")
library(emmeans)
emm_t_dm <- emmeans(anova_result_interactive, pairwise ~ `TAXA`|`DIVERSIFICATION MODE`, adjust= "Tukey")
emm_t_dm


emm_t_br <- emmeans(anova_result_interactive, pairwise ~ `BIOGEOGRAPHIC REGIONS`|`TAXA`, adjust= "Tukey")
emm_t_br

emm_br_dm <- emmeans(anova_result_interactive, pairwise ~ `BIOGEOGRAPHIC REGIONS`|`DIVERSIFICATION MODE`, adjust= "Tukey")
emm_t_dm


emm_t_dm
emm_t_br
emm_br_dm

plot(emm_t_dm)
plot(emm_t_br)
plot(emm_br_dm)


#What factor is driving diversification events among my data information?
install.packages(effectsize)
library(effectsize)
eta_squared(car::Anova(anova_result_interactive, type = 3), partial = FALSE)
omega_squared(car::Anova(anova_result_interactive, type = 3), partial = FALSE)
#ETA2 values reflect the percentage proportion of variability in diversification event outcome explained by each of these effects
#Partial R Squared
#main effects vs interactive effects and the importance 

#omega squared vs eta squared
#eta squared can be biased
#omega squared is the unbiased estimator of proportions variance explained *the correction*


# # Effect Size for ANOVA (Type III)
# 
# Parameter                                  | Omega2 |       95% CI
# ------------------------------------------------------------------
# BIOGEOGRAPHIC REGIONS                      |   0.05 | [0.00, 1.00]
# TAXA                                       |   0.25 | [0.14, 1.00]
# DIVERSIFICATION MODE                       |   0.03 | [0.00, 1.00]
# BIOGEOGRAPHIC REGIONS:TAXA                 |   0.04 | [0.00, 1.00]
# BIOGEOGRAPHIC REGIONS:DIVERSIFICATION MODE |   0.19 | [0.00, 1.00]
# TAXA:DIVERSIFICATION MODE                  |   0.25 | [0.02, 1.00]
# 
# - One-sided CIs: upper bound fixed at [1.00].

#isolating where the differences are (joint_tests package from emmeans)
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








#*SIDE NOTES*

#transform aov data to emmeans readable data
# model_anovamain_result <- lm(`NUMBER OF DIVERSIFICATION EVENTS`~(`BIOGEOGRAPHIC REGIONS`+`TAXA`+`DIVERSIFICATION MODE`), data = table_ANOVA_edit)
# plot(model_anovamain_result)
# summary(model_anovamain_result)
# 
# 
# 
# model_anova_result <- lm(`NUMBER OF DIVERSIFICATION EVENTS`~(`BIOGEOGRAPHIC REGIONS`*`TAXA`*`DIVERSIFICATION MODE`)-
#                            (`BIOGEOGRAPHIC REGIONS`:`TAXA`:`DIVERSIFICATION MODE`), data = table_ANOVA_edit)
# summary (model_anova_result)
# plot(model_anova_result)
# 
# 
# #install emmeans package to run interactive effects testing
# install.packages(emmeans)
# library(emmeans)
# 
# #TAXA and DIVERSIFICATION MODE EMMEANS TEST TRY 1
# anova_comparison_taxa_diversificationmode <- emmip(anova_result, 
#               `DIVERSIFICATION MODE`~`TAXA`)
# summary (anova_comparison_taxa_diversificationmode)
# taxa_diversificationmode_anovaplot <- plot(emmeans(anova_result, ~ `DIVERSIFICATION MODE`|`TAXA`))
# taxa_diversificationmode_anovaplot
# 
# #trying the emmeans test with the model transformed data set
# taxa_diversificationmode_anovaplot <- plot(emmeans(model_anova_result, ~ `DIVERSIFICATION MODE`|`TAXA`))
# taxa_diversificationmode_anovaplot
# 
# #TAXA and BIOGEOGRAPHIC REGIONS EMMEANS TEST TRY 1
# anova_comparison_taxa_biogeographicregion <- emmip(anova_result, `BIOGEOGRAPHIC REGIONS`~`TAXA`)
# anova_comparison_taxa_biogeographicregion
# taxa_biogeographicregion_anovaplot <- plot(emmeans(anova_result, ~ `BIOGEOGRAPHIC REGIONS`|`TAXA`))
# taxa_biogeographicregion_anovaplot
# 
# #DIVERSIFICATION MODE and BIOGEOGRAPHIC REGIONS EMMEANS TEST TRY 1
# anova_comparison_biogeographicregion_diversificationmode <- emmip(anova_result, `DIVERSIFICATION MODE`~`BIOGEOGRAPHIC REGIONS`)
# anova_comparison_biogeographicregion_diversificationmode
# biogeographicregion_diversificationmode_anovaplot <- plot(emmeans(anova_result, ~ `DIVERSIFICATION MODE`|`BIOGEOGRAPHIC REGIONS`))
# biogeographicregion_diversificationmode_anovaplot


#EMMEANS TEST TRY 2 IN A DIFFERENT WAy USING MODEL TRANSFORMED DATA
emm_t_dm <- emmeans(model_anova_result, pairwise ~ `TAXA`|`DIVERSIFICATION MODE`)
emm_t_br <- emmeans(model_anova_result, pairwise ~ `BIOGEOGRAPHIC REGIONS`|`TAXA`)
emm_br_dm <- emmeans(model_anova_result, pairwise ~ `BIOGEOGRAPHIC REGIONS`|`DIVERSIFICATION MODE`)




#IS THERE A DIFFERENCE BETWEEN USING THE MODEL TRANSFORMED DATA VS THE NON????





#IRRELEVANT....????
CON_TDM <- pairs(emm_t_dm, by= "DIVERSIFICATION MODE")
contrast(CON_TDM, "TAXA", by = NULL)
CONTRAST_TDM <- contrast(CON_TDM, "TAXA", by = NULL)
CONTRAST_TDM


t_dm_emmmeansummary <- as.data.frame(emm_t_dm$emmeans)
t_dm_emmmeansummary
t_dm_emmcontrastsummary <- as.data.frame(emm_t_dm$contrasts)
t_dm_emmcontrastsummary

t_br_emmmeansummary <- as.data.frame(emm_t_br$emmeans)
t_br_emmmeansummary
t_br_emmcontrastsummary <- as.data.frame(emm_t_br$contrasts)
t_br_emmcontrastsummary

br_dm_emmmeansummary <- as.data.frame(emm_br_dm$emmeans)
br_dm_emmmeansummary
br_dm_emmcontrastsummary <- as.data.frame(emm_br_dm$contrasts)
table_br_dm_emmcontrastsummary <- table(br_dm_emmcontrastsummary)
table_br_dm_emmcontrastsummary

ggplot(taxa_diversificationmode_anovaplot, aes(x=emmean, ))

#visualize the variations in means through ggplot
install.packages("interactions")
library(interactions)

model_anova_result <- lm(`NUMBER OF DIVERSIFICATION EVENTS`~`BIOGEOGRAPHIC REGIONS`*`TAXA`*`DIVERSIFICATION MODE`-
                           `BIOGEOGRAPHIC REGIONS`:`TAXA`:`DIVERSIFICATION MODE`, data = table_ANOVA_edit)
model_anova_result
summary(model_anova_result)

interact_plot(model_anova_result, pred = `NUMBER OF DIVERSIFICATION EVENTS`, modx = `TAXA`, interval = TRUE)

interact_plot(model_anova_result, pred = as.factor(`BIOGEOGRAPHIC REGIONS`), modx = `TAXA`, interval = TRUE)




emmeans package to functionally sclice up model results
visualizing finer granularity

emmeans (models ~ A|B)
emmeans (model, pairwise ~ A|B)




#visualizing comparison for each mode using bar plot from ggplot2

install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
anova_summaries <- data.frame(
  Diversification_Modes = c("Andes_Uplift", "IP_Emergence", "Miocene_Marine_Incursions", 
                            "Pleistocene_Climate", "Pleistocene_Landscape", 
                            "Riverine_Barrier", "Eastern_South_American_Dry_Diagonal"),
  Mean_Sq = c(437.5, 60.97, 15.2, 349.1, 89.07, 299.4, 5.467)
)

ggplot(anova_summaries, aes(x = Diversification_Modes, y = Mean_Sq, fill = Mean_Sq)) + 
  geom_bar(stat = "identity") + 
  labs(title = "ANOVA Summaries of Diversification Mode Across Taxa",
       x = "Diversification Modes",
       y = "Mean Square Values") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = FALSE)

                    