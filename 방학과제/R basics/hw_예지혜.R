# 1
students <- c("Sean", "Louisa", "Frank", "Farhad", "Li")
midterm <- c(80, 90, 93, 82, 95)
final <- c(78, 84, 95, 82, 91) # Final exam scores
names(midterm)<-students->names(final)
# ans
students[(final < midterm) & (midterm > 80)]

# 2
survey <- read.table("http://www.andrew.cmu.edu/user/achoulde/94842/data/survey_data.csv", header=TRUE, sep=",")
# ans
subset(survey, 
       select = c("OperatingSystem", "TVhours"),  
       subset = (Program == "PPM" | Program == "Other") & 
         (Rexperience == "Basic competence")
)

# 3
library(MASS)
data(Cars93)
head(Cars93, 3)
# ans
table(Cars93$AirBags, Cars93$Origin)

# 4 ans
tapply(Cars93[["Horsepower"]], INDEX = Cars93[c("Origin", "Type")], FUN=mean)

# 5
library(MASS)
library(ggplot2)
library(plyr)
data(birthwt)
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight", 
                       "race", "mother.smokes", "previous.prem.labor", "hypertension", "uterine.irr", 
                       "physician.visits", "birthwt.grams")
birthwt <- transform(birthwt, 
                     race = as.factor(mapvalues(race, c(1, 2, 3), 
                                                c("white","black", "other"))),
                     mother.smokes = as.factor(mapvalues(mother.smokes, 
                                                         c(0,1), c("no", "yes"))),
                     hypertension = as.factor(mapvalues(hypertension, 
                                                        c(0,1), c("no", "yes"))),
                     uterine.irr = as.factor(mapvalues(uterine.irr, 
                                                       c(0,1), c("no", "yes"))),
                     birthwt.below.2500 = as.factor(mapvalues(birthwt.below.2500,
                                                              c(0,1), c("no", "yes")))
)
head(birthwt,3)
# (1) ans
aggregate(birthwt.grams ~ race + mother.smokes, FUN=mean, data=birthwt)
# (2) ans
ggplot(birthwt, aes(x=mother.age, y=birthwt.grams, shape=mother.smokes, color=mother.smokes)) + 
  geom_point() +
  ylab("Birth Weight") +
  xlab("Mother's Age")

# 6 ans
library(maps)
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
head(arrests)
states <- map_data("state")
head(states)
choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
head(choro)

qplot(long, lat, data = choro, group = group, fill = murder, geom = "polygon") + 
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")

# 7 ans : difference in means is not equal to 0
birthwt.t.test <- t.test(birthwt.grams ~ mother.smokes, data = birthwt)
birthwt.t.test

# 8 ans
with(birthwt, qqnorm(birthwt.grams[mother.smokes=="no"]))
with(birthwt, qqline(birthwt.grams[mother.smokes=="no"], col = "blue"))

# 9
crime <- read.table("http://www.andrew.cmu.edu/user/achoulde/94842/data/crime_simple.txt", sep = "\t", header = TRUE)
colnames(crime) <- c("crime.per.million", "young.males", "is.south", "average.ed",
                     "exp.per.cap.1960", "exp.per.cap.1959", "labour.part",
                     "male.per.fem", "population", "nonwhite",
                     "unemp.youth", "unemp.adult", "median.assets", "num.low.salary")

head(crime,3)
# ans
crime.lm <- lm(crime.per.million ~ .- exp.per.cap.1959 - unemp.youth, data = crime)
summary(crime.lm)

# 10 ans
library(knitr)
crime.lm.summary <- summary(crime.lm)
kable(crime.lm.summary$coef, 
      digits = c(3, 3, 3, 4), format = 'markdown')

# 11 ans
gapminder <- read.delim("http://www.andrew.cmu.edu/user/achoulde/94842/data/gapminder_five_year.txt")
country.name <- "Ireland"  # Pick a country
gapminder.sub <- subset(gapminder, country == country.name)  # Pull data for this country
qplot(year, lifeExp, data = gapminder.sub, main = paste("Life expectancy in", country.name))

# 12 ans
year.min <- min(gapminder$year)
getCoef <- function(df) {
  coefs <- lm(lifeExp ~ I(year - year.min), data = df)$coef
  names(coefs) <- c("intercept", "slope")
  coefs
}
summary.coef <- ddply(gapminder, ~ country, getCoef)
summary.continent <- ddply(gapminder, ~ country, summarize, continent = unique(continent))
summary.merge <- merge(summary.coef, summary.continent, by = "country")
summary.intercept <- transform(summary.merge, country = reorder(country, intercept))
intercept.fig <- ggplot(data = summary.intercept, mapping = aes(x = country, y = intercept, fill = continent))
intercept.fig + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) 

# 13 
library(ggplot2)
library(dplyr)
library(nycflights13)
head(flights,3)

# ans
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    mean_arr_delay = mean(arr_delay, na.rm = TRUE),
    mean_dep_delay = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(mean_arr_delay > 30 | mean_dep_delay > 30)
