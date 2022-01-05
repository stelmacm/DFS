#This is my first DFS script where I try to predict how many points I can get
library(tidyverse)
library(lpSolveAPI)
dfsdata <- read.csv("Jan4NBA.csv")
dfsdata <- DFSdf %>% filter(Injury.Status != "INJ") %>%
  filter(Injury.Status != "O")

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
set.objfn(basketball_model, dfsdata$FPPG)

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
First_Score <- crossprod(dfsdata$FPPG,get.variables(basketball_model))

get.variables(basketball_model)

optimal_basketball_lineup <- subset(data.frame(dfsdata$Last.Name, dfsdata$Position, 
                                               dfsdata$Salary, dfsdata$FPPG), 
                                    get.variables(basketball_model) == 1)
optimal_basketball_lineup
sum(optimal_basketball_lineup$dfsdata.FPPG)
