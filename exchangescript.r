####################
#Paul M. Cohen
#11/16/2014

library(plyr)

####################
#Data source: https://www.healthcare.gov/health-plan-information-2015/

fullPlanData.2014 <- read.csv("2014PlanData.csv",as.is=TRUE)
fullPlanData.2015 <- read.csv("2015PlanData.csv",as.is=TRUE)
fullPlanData.2015 <- subset(fullPlanData.2015, select=-Accredidation)

fullPlanData.2014$Year <- 2014
fullPlanData.2015$Year <- 2015

fullPlanData <- rbind(fullPlanData.2014, fullPlanData.2015)
fullPlanData <- subset(fullPlanData, !is.na(fullPlanData$Premium.Adult.Individual.Age.21))

####################

fullPlanData$Premium.Adult.Individual.Age.21 <- as.numeric(fullPlanData$Premium.Adult.Individual.Age.21)

fullPlanData.21premium.byState.MetalLevel <- aggregate(fullPlanData$Premium.Adult.Individual.Age.21, list(State = fullPlanData$State, Metal.Level = as.factor(fullPlanData$Metal.Level), Year = as.factor(fullPlanData$Year)),  min)

fullPlanData.count.byState.MetalLevel <- aggregate(fullPlanData$Issuer.Name, list(State = fullPlanData$State, Metal.Level = as.factor(fullPlanData$Metal.Level), Year = as.factor(fullPlanData$Year)), countFunction)

fullPlanData.byState.MetalLevel <- merge(fullPlanData.21premium.byState.MetalLevel, fullPlanData.count.byState.MetalLevel, by = c("State", "Metal.Level", "Year"))
colnames(fullPlanData.byState.MetalLevel)[4] <- "Premium21"
colnames(fullPlanData.byState.MetalLevel)[5] <- "Competition"

fullPlanData.byState.MetalLevel$Metal.Level <- factor(fullPlanData.byState.MetalLevel$Metal.Level, levels = c("Catastrophic", "Bronze", "Silver", "Gold", "Platinum"))


price_competition <- ggplot(fullPlanData.byState.MetalLevel, aes (x = Competition, y = Premium21)) + geom_point(aes(group = Year, color=Metal.Level), position="Jitter") + scale_y_continuous(name = "Monthly Premium for 21yo", limit=c(0,600), breaks=seq(0,600, by=100), labels=dollar) + scale_x_continuous(limit=c(0,16.25))
price_competition + stat_smooth(method = lm, aes(fill = factor(Metal.Level))) + facet_grid(Year ~Metal.Level) + guides(fill=guide_legend(title="Metal Level")) + labs(title="Minimum Monthly Premiums by Competition by State, 2014 vs. 2015", x = "Number of Unique Competitors in Market") + theme(legend.position="none", plot.title = element_text(vjust=1, face="bold"))


summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Bronze"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Silver"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Gold"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Platinum"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Catastrophic"))

summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset= Year == "2014"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset= Year == "2015"))



####################
###Mean Analysis

fullPlanData.mean$Premium.Adult.Individual.Age.21 <- as.numeric(fullPlanData.mean$Premium.Adult.Individual.Age.21)

fullPlanData.mean.21premium.byState.MetalLevel <- aggregate(fullPlanData.mean$Premium.Adult.Individual.Age.21, list(State = fullPlanData.mean$State, Metal.Level = as.factor(fullPlanData.mean$Metal.Level), Year = as.factor(fullPlanData.mean$Year)),  mean)

fullPlanData.mean.count.byState.MetalLevel <- aggregate(fullPlanData.mean$Issuer.Name, list(State = fullPlanData.mean$State, Metal.Level = as.factor(fullPlanData.mean$Metal.Level), Year = as.factor(fullPlanData.mean$Year)), countFunction)

fullPlanData.mean.byState.MetalLevel <- merge(fullPlanData.mean.21premium.byState.MetalLevel, fullPlanData.mean.count.byState.MetalLevel, by = c("State", "Metal.Level", "Year"))
colnames(fullPlanData.mean.byState.MetalLevel)[4] <- "Premium21"
colnames(fullPlanData.mean.byState.MetalLevel)[5] <- "Competition"

fullPlanData.mean.byState.MetalLevel$Metal.Level <- factor(fullPlanData.mean.byState.MetalLevel$Metal.Level, levels = c("Catastrophic", "Bronze", "Silver", "Gold", "Platinum"))


price_competition <- ggplot(fullPlanData.mean.byState.MetalLevel, aes (x = Competition, y = Premium21)) + geom_point(aes(group = Year, color=Metal.Level), position="Jitter") + scale_y_continuous(name = "Monthly Premium for 21yo", limit=c(0,600), breaks=seq(0,600, by=100), labels=dollar) + scale_x_continuous(limit=c(0,16.25))
price_competition + stat_smooth(method = lm, aes(fill = factor(Metal.Level))) + facet_grid(Year ~Metal.Level) + guides(fill=guide_legend(title="Metal Level")) + labs(title="Mean Monthly Premiums by Competition by State, 2014 vs. 2015", x = "Number of Unique Competitors in Market") + theme(legend.position="none", plot.title = element_text(vjust=1, face="bold"))


summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset = Metal.Level == "Bronze"))
summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset = Metal.Level == "Silver"))
summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset = Metal.Level == "Gold"))
summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset = Metal.Level == "Platinum"))
summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset = Metal.Level == "Catastrophic"))

summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset= Year == "2014"))
summary(lm(Premium21~Competition, fullPlanData.mean.byState.MetalLevel, subset= Year == "2015"))

####################
###Minimum Analysis

fullPlanData$Premium.Adult.Individual.Age.21 <- as.numeric(fullPlanData$Premium.Adult.Individual.Age.21)

fullPlanData.21premium.byState.MetalLevel <- aggregate(fullPlanData$Premium.Adult.Individual.Age.21, list(State = fullPlanData$State, Metal.Level = as.factor(fullPlanData$Metal.Level), Year = as.factor(fullPlanData$Year)),  min)

fullPlanData.count.byState.MetalLevel <- aggregate(fullPlanData$Issuer.Name, list(State = fullPlanData$State, Metal.Level = as.factor(fullPlanData$Metal.Level), Year = as.factor(fullPlanData$Year)), countFunction)

fullPlanData.byState.MetalLevel <- merge(fullPlanData.21premium.byState.MetalLevel, fullPlanData.count.byState.MetalLevel, by = c("State", "Metal.Level", "Year"))
colnames(fullPlanData.byState.MetalLevel)[4] <- "Premium21"
colnames(fullPlanData.byState.MetalLevel)[5] <- "Competition"

fullPlanData.byState.MetalLevel$Metal.Level <- factor(fullPlanData.byState.MetalLevel$Metal.Level, levels = c("Catastrophic", "Bronze", "Silver", "Gold", "Platinum"))


price_competition <- ggplot(fullPlanData.byState.MetalLevel, aes (x = Competition, y = Premium21)) + geom_point(aes(group = Year, color=Metal.Level), position="Jitter") + scale_y_continuous(name = "Monthly Premium for 21yo", limit=c(0,600), breaks=seq(0,600, by=100), labels=dollar) + scale_x_continuous(limit=c(0,16.25))
price_competition + stat_smooth(method = lm, aes(fill = factor(Metal.Level))) + facet_grid(Year ~Metal.Level) + guides(fill=guide_legend(title="Metal Level")) + labs(title="Minimum Monthly Premiums by Competition by State, 2014 vs. 2015", x = "Number of Unique Competitors in Market") + theme(legend.position="none", plot.title = element_text(vjust=1, face="bold"))


summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Bronze"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Silver"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Gold"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Platinum"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset = Metal.Level == "Catastrophic"))

summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset= Year == "2014"))
summary(lm(Premium21~Competition, fullPlanData.byState.MetalLevel, subset= Year == "2015"))



####################