#We are beginning by finding the optimal starting 5 based on the Yahoo fantasy points per game

setwd("~/Desktop/Side Projects/Fantasy")
library(tidyverse)
library(lpSolveAPI)
dfsdata <- read.csv("Yahoo_DF_player_export.csv")
originalyahoodf <- read.csv("Yahoo_DF_player_export.csv")
head(originalyahoodf)
#Importing projections from linups.com
optimprojections <- read.csv("nba-fantasy-basketball-projections.csv")
optimprojections <- optimprojections %>% select(c(Name, Projection, USG.))
#Going to remove injured or not playing players
test <- within(originalyahoodf, Name <- paste(First.Name, Last.Name, sep = " "))
test <- test %>% select(-c(First.Name, Last.Name, Starting))

dfsdata <- left_join(test, optimprojections, by = "Name")
dfsdata <- dfsdata %>% filter(Injury.Status != "INJ") %>%
                       filter(Injury.Status != "O")

#Need to also remove some by hand for undocumented injuries
#as well as personal vendetta's I have
dfsdata <- dfsdata %>% filter(Name != "Kristaps Porzingis") %>%
  filter(Name != "Jalen Brunson") %>%
  filter(Name != "LeBron James") %>%
  filter(Name != "Tobias Harris") %>%
  filter(Name != "Mikal Bridges") %>%
  filter(Name != "Lonzo Ball")

#Dont need to set up a classifier for utility because we can define utility in the 
#problem constraints

#Creating some positional identifiers in the pool of players to simplify linear constraints
dfsdata$C_Check <- ifelse(dfsdata$Position == "C",1,0)
dfsdata$PG_Check <- ifelse(dfsdata$Position == "PG",1,0)
dfsdata$SG_Check <- ifelse(dfsdata$Position == "SG",1,0)
dfsdata$SF_Check <- ifelse(dfsdata$Position == "SF",1,0)
dfsdata$PF_Check <- ifelse(dfsdata$Position == "PF",1,0)
dfsdata$One <- 1


dfsdata <- dfsdata[order(dfsdata$C_Check),]
dfsdata <- dfsdata[order(dfsdata$PG_Check),]
dfsdata <- dfsdata[order(dfsdata$SG_Check),]
dfsdata <- dfsdata[order(dfsdata$SF_Check),]
dfsdata <- dfsdata[order(dfsdata$PF_Check),]

#Solver implementation
Number_of_Players <- length(dfsdata$One)

basketball_model= make.lp(0, Number_of_Players)
set.objfn(basketball_model, dfsdata$Projection)

lp.control(basketball_model, sense= "max")
set.type(basketball_model, 1:Number_of_Players, "binary")

add.constraint(basketball_model, dfsdata$Salary, "<=",200)

add.constraint(basketball_model, dfsdata$C_Check, "<=",2)
add.constraint(basketball_model, dfsdata$C_Check, ">=",1)

add.constraint(basketball_model, dfsdata$PG_Check, "<=",3)
add.constraint(basketball_model, dfsdata$PG_Check, ">=",1)

add.constraint(basketball_model, dfsdata$SG_Check, "<=",3)
add.constraint(basketball_model, dfsdata$SG_Check, ">=",1)

add.constraint(basketball_model, dfsdata$PF_Check, "<=",3)
add.constraint(basketball_model, dfsdata$PF_Check, ">=",1)

add.constraint(basketball_model, dfsdata$SF_Check, "<=",3)
add.constraint(basketball_model, dfsdata$SF_Check, ">=",1)

add.constraint(basketball_model, dfsdata$One, "=",8)

solve(basketball_model)
First_Score <- crossprod(dfsdata$Projection,get.variables(basketball_model))

get.variables(basketball_model)

optimal_basketball_lineup <- subset(data.frame(dfsdata$Name, dfsdata$Position, 
                                               dfsdata$Salary, dfsdata$FPPG, dfsdata$Projection), 
                                    get.variables(basketball_model) == 1)
optimal_basketball_lineup
sum(optimal_basketball_lineup$dfsdata.Projection)

