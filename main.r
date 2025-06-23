#imports
library(tidyverse)
library(panelr)
library(foreign)
library(ggplot2)
library(dplyr)
library(lme4)
library(lattice)
library(plm)
library(margins)
#dataset
match <- read.csv("datasets/df_mens_indv_21_23.csv")
players <- read.csv("datasets/df_mens_rosters_21_23.csv")
res <- read.csv("datasets/results.csv")
res <- res %>% mutate(winner = ifelse(startsWith(Score, '3'), TeamA, TeamB))
match <- merge(match, res, by=c('TeamA', 'TeamB', 'Match_Date'))
match[, 6:ncol(match)-2] <- lapply(6:ncol(match)-2, function(x) as.numeric(match[[x]]))
players <- players[,2:9]
data <- merge(match, players, by=c('Player_ID', 'Year'))
data <- data %>% mutate(result = ifelse(winner == Country_Name, 1, 0))
data <- data[data$Position!='L' & data$Year==2021,]
data <- subset(data, select=c(Match_Date, Player_ID, Age, Height, Serve_Points, Serve_Errors, Block_Successful, Block_Errors, Block_Rebounds, Player.Name, Position, Country_Name, result))
data <- na.omit(data)
data$Match_Date <- as.Date(data$Match_Date , format = "%d/%m/%Y")
names <- c(id='Player_ID', t='Match_Date', age = 'Age', height = 'Height',
           serve_p='Serve_Points', serve_e='Serve_Errors',
           block_s = "Block_Successful", block_e = "Block_Errors", block_r = "Block_Rebounds",
           name = "Player.Name", pos = "Position", country = "Country_Name")
data <- rename(data, all_of(names))
data_scaled <- data
#data_scaled[,3:9] <- sapply(data_scaled[,3:9], function(x) scale(x,scale=FALSE))  

#descriptive analysis
summary(data)

length(unique(data$id))
length(unique(data$country))
ggplot(data,aes(x=serve_p))+geom_histogram()+facet_wrap(~ country, nrow = 1)
ggplot(data,aes(x=serve_e))+geom_histogram()+facet_wrap(~ country, nrow = 1)
ggplot(data,aes(x=block_s))+geom_histogram()+facet_wrap(~ country, nrow = 1)
ggplot(data,aes(x=block_e))+geom_histogram()+facet_wrap(~ country, nrow = 1)
ggplot(data,aes(x=block_r))+geom_histogram()+facet_wrap(~ country, nrow = 1)

#Fixed effect panel model
panel_pd <- pdata.frame(data, index= c('id', 't', 'country'), drop.index=TRUE)
fe <- plm(result ~ serve_p + serve_e +  
            block_s + block_e + block_r,
          data=panel_pd, model="within")
summary(fe)
plmtest(fe, type='bp')

#Random
re <- plm(result ~ serve_p + serve_e + block_s + block_e + block_r, data=panel_pd, model="random")
summary(re)
phtest(fe, re)

re2 <- plm(result ~ serve_p + serve_e + block_s + block_e + block_r, data=panel_pd, model="random", effect = 'nested', random.method = "walhus")
summary(re2)
phtest(re2, fe)
re2 <- plm(result ~ serve_p + serve_e + block_s + block_e + block_r, data=panel_pd, model="random", effect = 'nested', random.method = "swar")
summary(re2)

#log
logmodel <- glmer(result ~ serve_p + serve_e +
                    block_s + block_e + block_r +
                    (1|country) +
                    (1|country:id),
                  data = data,
                  family=binomial(link="logit"))
summary(logmodel)
marg <- margins(logmodel, data = data)
marg
