# Script paper:
# title: "In the name of God"
# subtitle: "Shaping public opinion about homosexuals in Brazil"
# author: "Renata Cavalcanti"

# Section "Argument"
# Figure 1- Theory (Diagram)
# First require package
library(DiagrammeR)
# Second set the theory
Theory <- DiagrammeR("graph TB;
                     A(Do deputies of Brazil think the same as voters do?)-->B(Democracy representation);
                     B-->C(Hypothesis 1: in Brazil, public opinion about LGBT people is the same as Congress.);
                     B-->D(Hypothesis 2: Brazil is a homophobic country.);
                     B-->E(Ideology and religion matters.);
                     E-->F(Hypothesis 3: ideology explains policy positions about LGBT.);
                     ")

Theory

# Section "Research design and data"

# TABLE 1 - LAPOP Database
# Load packages 
library(knitr) # to create table
library(kableExtra) # to adjust table

# Set the table
descriptive_lapop <- data.frame(Variable_code = c("q1", "d5", "d6", "l1", "q3c", "q5a", "q5b"),
                               Meaning = c("Sex", "Do you approve or disapprove that homosexuals run for office?",
                                           "Do you approve or disapprove that homosexuals have the right to get married?",
                                           "How do you place yourself on a left-right scale?", "What is your religion?",
                                           "How often do you go to religious cult?", "How important is religion in your life?"), 
                               Scale = c("1-Man; 2- Woman", "1- Strongly disapprove/10- Strongly approve", "1- Strongly disapprove/10- Strongly approve", "1- Left/10- Right","1- Catholic, 2 -Protestant, 3- Not Christian, 4- None, 
                                         5- Evangelical Pentecostal, 6- Mormon, 
                                         7- Traditional or Native Religions, 
                                         8- Kardecist Spiritist, 10- Jewish, 11- Atheist,
                                         12- Jehovah's Witness", "1- More than once a week, 2- Once a week, 3- Once or twice a month,
                                         4- Once or twice a year, 5- Never or almost never", "1- Very important, 2- Something important,
                                         3- Little important, 4- Not important"))
# Create table 
kable(descriptive_lapop, caption = "LAPOP Database", "latex", booktabs = T) %>% 
  kable_styling(full_width = T, latex_options = "striped") %>% column_spec(1, width = "5cm")

# TABLE 2 - PELA Database
# Set the table
descriptive_lapop <- data.frame(Variable_code = c("SOCD4", "VAL1", "ID1", "RE1a", "RE1b"),
                                Meaning = c("Sex", "Do you approve or disapprove that homosexuals have the right to get married?",
                                            "How do you place yourself on a left-right scale?", "What is your religion?",
                                            "How often do you go to religious cult?"),
                                Scale = c("1- Man/2- Woman", "1- Strongly disapprove/10- Strongly approve", "1- Left/10- Right", "1- Catholic, 2- Protestant, 3- Not Christian, 
                                          4- Evangelical Pentecostal, 
                                          6- Traditional or Native Religions, 9- Others", "5- More than once a week, 4- Once a week, 3- Once a month,
                                          2- Once or twice a year, 1- Never or almost never, 9- Others"))
# Create table
kable(descriptive_lapop, caption = "LAPOP Database", "latex", booktabs = T) %>% 
  kable_styling(full_width = T, latex_options = "striped") %>% column_spec(1, width = "5cm")

# Section "Findings/Results"
## _LAPOP Data_
### LGBT

# Load packages for this section
library(haven) # to open database
library(ggplot2) # to create graphs

# Open database
LAPOP_2010 <- read_stata("~/Downloads/Analise de dados/LAPOP_2010")

## GRAPH 1
# Call database
attach(LAPOP_2010)
# Set data frame
lapop_graph1 <- data.frame(q1 = factor(q1, labels = c("Men", "Women")), 
                         d5 = factor(d5, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                                    "6", "7", "8", "9", "Strongly approve")))
theme_set(theme_bw()) # for white background

# Build graph
ggplot(lapop_graph1, aes(d5)) + geom_bar(aes(fill=q1)) +
  facet_wrap(~ q1, ncol = 1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Graph 1 - Should homosexuals run for office?") +
  labs(caption = "Source: LAPOP") +
  theme(legend.position="none")

# TABLE 3 - Chi-Square
# Call databse
attach(LAPOP_2010)
# Chi test
chi1 <- chisq.test(LAPOP_2010$d5, LAPOP_2010$q1, correct = FALSE)
chi1
# Set table for markdown
chiTable <- data.frame(Coefficients = c("X-Squared", "P-Value","df", "N"),
                       Value = c(20.863, 0.013, 9, 2482))
kable(chiTable, caption = "Chi-Square")

# GRAPH 2
# Call database
attach(LAPOP_2010)
# Set data frame
lapop_graph2 <- data.frame(q1 = factor(q1, labels = c("Men", "Women")), 
                         d6 = factor(d6, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                                    "6", "7", "8", "9", "Strongly approve")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot(lapop_graph2, aes(d6)) + geom_bar(aes(fill=q1)) +
  facet_wrap(~ q1, ncol = 1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme(axis.title.x = element_blank()) +
  ggtitle("Graph 2 - Should homosexuals have the right to get married?") +
  labs(caption = "Source: LAPOP") +
  theme(legend.position="none")

# GRAPH 3
# Call database
attach(LAPOP_2010)
# Set data frame
lapop_graph3 <- data.frame(d6 = factor(d6, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                               "6", "7", "8", "9", "Strongly approve")), 
                    l1 = factor(l1, labels = c("Left", "2", "3", "4", "5", 
                                               "6", "7", "8", "9", "Right")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot() +
  geom_bar(data=lapop_graph3, aes(x=d6, fill = l1), alpha = 1) +
  facet_wrap(~l1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  labs(title="Graph 3 - Ideology vs. Public opinion about same sex marriage", x="", y="",
       fill = "",
       caption = "Source: LAPOP") +
  theme(legend.position="none")


### Religion
# Graph 4
# Call database
attach(LAPOP_2010)
# Set data frame
lapop_graph4 <- data.frame(q1 = factor(q1, labels = c("Men", "Women")), 
                        q3c = factor(q3c, labels = c("Catholic", "Protestant", "Not Christian", "None", 
                                                     "Evangelical Pentecostal", "Mormon", 
                                                     "Traditional or Native Religions", 
                                                     "Kardecist Spiritist", "Jewish", "Atheist",
                                                     "Jehovah's Witness")))

theme_set(theme_bw()) # for white background
#Build graph
ggplot(lapop_graph4, aes(q3c, fill=q3c)) + geom_bar() +
  facet_wrap(~ q1, ncol = 1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme(axis.title.x = element_blank()) +
  labs(title="Graph 4 - Religion by sex",
       caption="Source: LAPOP",
       x="Religion",
       y="N") +
  theme(legend.position="none")

# Graph 5
# Call database
attach(LAPOP)
# Set data frame
lapop_graph5 <- data.frame(q5a = factor(q5a, labels = c("More than once a week", "Once a week", "Once or twice a month",
                                                     "Once or twice a year", "Never or almost never")), 
                        q3c = factor(q3c, labels = c("Catholic", "Protestant", "Not Christian", "None", 
                                                     "Evangelical Pentecostal", "Mormon", 
                                                     "Traditional or Native Religions", 
                                                     "Kardecist Spiritist", "Jewish", "Atheist",
                                                     "Jehovah's Witness")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot(lapop_graph5, aes(x=q5a, y=q3c)) + 
  geom_jitter(size=1, aes(col=q3c)) + 
  geom_segment(aes(x=q5a, 
                   xend=q5a, 
                   y=0, 
                   yend=q3c)) + 
  labs(title="Graph 5 - Frequency of Religious Services", 
       caption="Source: LAPOP",
       x="Frequency",
       y="") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

# Graph 6
# Call database
attach(LAPOP_2010)
# Set data frame
lapop_graph6 <- data.frame(q3c = factor(q3c, labels = c("Catholic", "Protestant", "Not Christian", "None", 
                                                     "Evangelical Pentecostal", "Mormon", 
                                                     "Traditional or Native Religions", 
                                                     "Kardecist Spiritist", "Jewish", "Atheist",
                                                     "Jehovah's Witness")), 
                        q5b = factor(q5b, labels = c("Very important", "Something important",
                                                     "Little important", "Not important")))
# Build graph
ggplot(lapop_graph6, aes(x=q5b, y=q3c)) + 
  geom_jitter(size=1, aes(col=q3c)) + 
  geom_segment(aes(x=q5b, 
                   xend=q5b, 
                   y=0, 
                   yend=q3c)) + 
  labs(title="Graph 6 - Importance of Religion", 
       caption="Source: LAPOP",
       x="Level of importance",
       y="") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

# Graph 7
# Call database
attach(lapop2010)
# Set data frame
lapop_graph7 <- data.frame(q5a = factor(q5a, labels = c("More than once a week", "Once a week", "Once or twice a month",
                                                 "Once or twice a year", "Never or almost never")), 
                    d6 = factor(d6, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                               "6", "7", "8", "9", "Strongly approve")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot() +
  geom_bar(data=lapop_graph7, aes(x=q5a, fill = d6), alpha = 1) +
  facet_wrap(~d6) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  labs(title="Graph 7 - Frequncy vs. Public opinion about same sex marriage", x="", y="",
       fill = "",
       caption = "Source: LAPOP") +
  theme(legend.position="none")


## _PELA Data_
### LGBT

# Open database
pela2010 <- read_sav("~/Downloads/Analise de dados/CEL_UFMG_Banco_Elites_Nacionais_Deputados_Federais_2010/CEL_UFMG_Banco_Elites_Nacionais_Deputados_Federais_2010.sav")

# Graph 8
# Call database
attach(pela2010)

# Set data frame
lapop_graph8 <- data.frame(SOCD4 = factor(SOCD4, labels = c("Men", "Women")), 
                       VAL1 = factor(VAL1, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                                      "6", "7", "8", "9", "Strongly approve")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot(lapop_graph8, aes(VAL1)) + geom_bar(aes(fill=SOCD4)) +
  facet_wrap(~ SOCD4, ncol = 1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme(axis.title.x = element_blank()) +
  labs(title = "Graph 8 - Should homosexuals have the right to get married?",
       subtitle = "Deputies survey",
       caption = "Source: PELA") +
  theme(legend.position="none")


# TABLE 4
# Call database
attach(pela2010)
chi2 <- chisq.test(pela2010$SOCD4, pela2010$VAL1, correct = FALSE)
chi2
# Formula test
chiTable2 <- data.frame(Coefficients = c("X-Squared", "P-Value","df", "N"),
                       Value = c(15.493, 0.078, 9, 129))
kable(chiTable2, caption = "Chi-Square")

# Graph 9
# Call database
attach(pela2010)
# Set data frame
pela_graph9 <- data.frame(VAL1 = factor(VAL1, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                                  "6", "7", "8", "9", "Strongly approve")), 
                   ID1 = factor(ID1, labels = c("Left", "2", "3", "4", "5", 
                                                "6", "7", "8", "Right")))

theme_set(theme_bw()) # for white backgorud
#Build graph
ggplot() +
  geom_bar(data=pela_graph9, aes(x=VAL1, fill = ID1), alpha = 1) +
  facet_wrap(~ID1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  labs(title="Graph 9 - Ideology vs. Deputies opinion about same sex marriage", x="", y="",
       fill = "",
       caption = "Source: PELA") +
  theme(legend.position="none")

### Religion
# Graph 10
# Call database
attach(pela2010)
# Set the data frame
pela_graph10 <- data.frame(SOCD4 = factor(SOCD4, labels = c("Men", "Women")), 
                            RE1a = factor(RE1a, labels = c("Catholic", "Protestant", "Not Christian", 
                                                           "Evangelical Pentecostal", 
                                                           "Traditional or Native Religions", "Others")))

#Build graph
theme_set(theme_bw()) # for white backgorud
ggplot(pela_graph10, aes(RE1a, fill=RE1a)) + geom_bar() +
  facet_wrap(~ SOCD4, ncol = 1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme(axis.title.x = element_blank()) +
  labs(title="Graph 10 - Deputies religion by sex",
       caption="Source: PELA",
       x="Religion",
       y="N") +
  theme(legend.position="none")

# Graph 11
# Call database
attach(pela2010)
# Set data frame
pela_graph11 <- data.frame(RE1b = factor(RE1b, labels = c("More than once a week", "Once a week", "Once a month",
                                                            "Once or twice a year", "Never or almost never", "Others")), 
                             RE1a = factor(RE1a, labels = c("Catholic", "Protestant", "Not Christian", 
                                                            "Evangelical Pentecostal", 
                                                            "Traditional or Native Religions", "Others")))
# Build graph
theme_set(theme_bw()) # for white backgorud
ggplot(pela_graph11, aes(x=RE1b, y=RE1a)) + 
  geom_jitter(size=1, aes(col=RE1a)) + 
  geom_segment(aes(x=RE1b, 
                   xend=RE1b, 
                   y=0, 
                   yend=RE1a)) + 
  labs(title="Graph 11 - Deputies' Frequency of Religious Services", 
       caption="Source: PELA",
       x="Frequency",
       y="") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

### Comparing the data

# Graph 13
# Call LAPOP database
attach(lapop2010)
# Set LAPOP data frame
lapop_graph13 <- data.frame(l1 = factor(l1, labels = c("Left", "2", "3", "4", "5", 
                                                     "6", "7", "8", "9", "Right")))
# Call PELA databse
attach(pela2010)
# Set PELA data frame
pela_graph13 <- data.frame(ID1 = factor(ID1, labels = c("Left", "2", "3", "4", "5", 
                                                           "6", "7", "8", "Right")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot() + 
  geom_bar(data=lapop_graph13, aes(x=l1, fill = "Public"), alpha = 0.7) +
  geom_bar(data=pela_graph13, aes(x=ID1, fill = "Deputies"), alpha = 1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  labs(title="Graph 13 - Public and deputies idelogy", x="", y="",
       fill = "",
       caption = "Source: PELA and LAPOP")

# Graph 14
# Call LAPOP database
attach(lapop2010)
# Set LAPOP data frame
lapop_graph14 <- data.frame(q1 = factor(q1, labels = c("Men", "Women")), 
                               d6 = factor(d6, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                                          "6", "7", "8", "9", "Strongly approve")))

# Call PELA databse
attach(pela2010)
# Set PELA data frame
pela_graph14 <- data.frame(SOCD4 = factor(SOCD4, labels = c("Men", "Women")), 
                            VAL1 = factor(VAL1, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                                           "6", "7", "8", "9", "Strongly approve")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot() + 
  geom_density(data=lapop_graph14, aes(x=d6, fill = "Public"), alpha = 0.3) +
  geom_density(data=pela_graph14, aes(x=VAL1, fill = "Deputies"), alpha = 0.3) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  labs(title="Graph 14 - Public and deputies opinion about same sex civil union", x="", y="",
       fill = "",
       caption = "Source: PELA and LAPOP")


# Section "Regression Model"
## Checking regression assumptions

# Table 5 - Residuals assumption

# Load package for table
library(pander)
# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# The mean of residuals is (close to) zero
# Create table
resi_mean <- data.frame(Residual_mean = c("8.354e-16"))
pander(resi_mean, caption = "Residual Mean")

# Table 6 - Homoscedasticity

# Load package for test
library(car)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# Evaluate homoscedasticity
homo.test <- ncvTest(regres) 
homo.test # p-value is more than 0.05 -> homocedasticity not violated
# Create table
homoc.test <- data.frame(Coefficients = c("ChiSquare", "df","p-value"),
                         Value = c(2.162, 1, 0.1412))
pander(homoc.test, caption = "Homocedasticity Test")

# Graph 15 - Autocorrelation of residuals

# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# Evaluate Autocorrelation of residuals
acf(regres$residuals, main="Graph 15 - No autocorrelation")
# No autocorrelation of residuals

# Table 7 - Correlation between the independent variables
# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# Test
pander(cor.test(l1, q5a, data = lapop2010)) # not violated

# Table 8 - Variability
# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# Test
var(lapop$l1)
var(lapop$q5a)
# Create table for markdown
variability.test <- data.frame(Variable = c("Ideology(l1)", "Religion Frequency"),
                               Value = c(NA, NA))
pander(variability.test, caption = "Variability Test")

# Table 9 - Multicollinearity
# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop[["l1"]] <- factor(lapop[["l1"]]) # ideology
lapop[["q5a"]] <- factor(lapop[["q5a"]]) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# Test
multi_test <- vif(regres)
# Create table
multi <- data.frame(Coefficients = c("GVIF", "df", "GVIF^(1/(2*DF))"),
                    Ideology = c(1.02, 9, 1.001),
                    Religion_Frequency = c(1.02, 4, 1.002))
pander(multi, caption = "Multicollinearity Test")

# Graph 16 - Normality

# Call database
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# Plot
qqnorm(regres$residuals, main = "Graph 16 - Normal Q-Q Plot")



## Section Model - Ideology, Religion and Approval of Gay Marriage
# Table 10 - Regression results
#Call database
attach(LAPOP_2010)

# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
summary(regres)
# Create table
regress <- data.frame(Observations = c(1743),
                      Residual_Std._Error = c(3.405),
                      R2 = c(0.04232),
                      Adjusted_R2 = c(0.03512),
                      p_value = c("9.541e-11"))
pander(regress, caption = "Regression Results")

# Graph 17
# Load packages
library(sjPlot)
library(jtools)

# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# plot
theme_set(theme_sjplot()) # for white background
plot_model(regres,title = "Regression", type = "slope") +
  labs(y="Same sex marriage approval", title = "Graph 17 - Marginal effects")

# Graph 18
# Call database
attach(LAPOP_2010)
# Transform independent variables(categorical variables) into factor 
lapop <- LAPOP_2010 # to change the database name
lapop$l1 <- factor(lapop$l1) # ideology
lapop$q5a <- factor(lapop$q5a) # frequency of religious services
regres <- lm(d6 ~ l1 + q5a, data = lapop) # regression formula
# plot coefficients
coefplot::coefplot(regres, parm = -1, 
                   title = "Graph 18 - Coefficient Regression Plot") 

# Graph 19
# Call database
attach(LAPOP_2010)
# Load packages
library(ggridges)
# Set data frame
lapop_graph19 <- data.frame(d6 = factor(d6, labels = c("Strongly disapprove", "2", "3", "4", "5", 
                                               "6", "7", "8", "9", "Strongly approve")),
                    q3c = factor(q3c, labels = c("Catholic", "Protestant", "Not Christian", "None", 
                                                 "Evangelical Pentecostal", "Mormon", 
                                                 "Traditional or Native Religions", 
                                                 "Kardecist Spiritist", "Jewish", "Atheist",
                                                 "Jehovah's Witness")))
theme_set(theme_bw()) # for white background
# Build graph
ggplot(lapop_graph19, aes(x = d6, y = q3c, fill = q3c)) +
  geom_density_ridges(alpha=0.3, bandwidth=2) +
  theme_light() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme(legend.position = "none") +
  labs(title="Graph 19 - Religion rejection vs. Public opinion 
       about same sex marriage", 
       x="", y="",
       fill = "",
       caption = "Source: LAPOP")

#### END #### 