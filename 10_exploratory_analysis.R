
aleague_team_stats <- aleague_team_data %>% ungroup() %>%  select_at(stats_vars)
#linear regression test
lm <- lm(stats.corners_taken ~ . -stats.corners_won, data = aleague_team_stats)
summary(lm)

#correlation test
library(Hmisc)
res2<-rcorr(as.matrix(mtcars[,1:7]))
flattenCorrMatrix(res2$r, res2$P)


stats.attacking_third_passes
stats.attacking_third_passes_complete

#Volume in final third
stats.crosses 
stats.ball_in_box

#Quality in Final Third
stats.attcking_third_passes_percentage

library(caret)

#library(xgboost)
#
aleague_team_opp_stats <- aleague_team_opp_data %>%
  select(contains("stats.")) %>% 
  mutate_all(as.numeric)

set.seed(123)
fit.xgb <- train(team.stats.corners_taken ~ . -team.stats.corners_won, 
                 data = aleague_team_opp_stats,
                 method="xgbTree", preProc=c("center", "scale"))
print(fit.xgb$finalModel)
varImp(fit.xgb)

vars <- rownames(varImp(fit.xgb)$importance) %>% head(20)

deciles <- aleague_team_opp_stats %>% 
  mutate(corners_decile = ntile(team.stats.corners_taken, 10)) %>% 
  group_by(corners_decile) %>% 
  summarise_at(vars(vars), mean) %>% 
  arrange(desc(corners_decile))
 
v <- data.frame(names(aleague_team_opp_stats))


fit.xgb.possessions <- train(team.stats.corners_taken ~ . -team.stats.possessions, 
                 data = aleague_team_opp_stats,
                 method="xgbTree", preProc=c("center", "scale"))
print(fit.xgb.possessions$finalModel)
varImp(fit.xgb.possessions)



#### Plots ####
ggplot(data = aleague_team_opp_data) +
  geom_boxplot(mapping = aes(x = as.character(team.est_mins_trailing), y = total.corners_taken))

ggplot(data = aleague_team_opp_data) +
  geom_boxplot(mapping = aes(x = as.character(team.goals_margin), y = total.corners_taken))


ggplot(data = aleague_team_opp_data) +
  geom_boxplot(mapping = aes(x = as.factor(team.goals_margin), y = team.stats.corners_taken))



#Check impact of rain

ggplot(data = aleague_match_data) +
  geom_boxplot(mapping = aes(x = rainfall.cat, y = total.corners))

ggplot(data = aleague_match_data) +
  geom_boxplot(mapping = aes(x = max_temperature.cat, y = total.corners))

geom_boxplot(mapping = aes(x = as.factor(team.goals_margin), y = team.stats.corners_taken))

ggplot(aleague_team_opp_data, aes(team.goals_margin, team.stats.corners_taken, color=as.factor(team.goals_margin))) + geom_jitter()
ggplot(aleague_team_opp_data, aes(team.goals_margin, total.corners_taken, color=as.factor(team.goals_margin))) + geom_jitter()

ggplot(aleague_match_data, aes(x=rainfall, color=total.corners_band)) +
  geom_density()
# Add mean lines
p<-ggplot(df, aes(x=weight, color=sex)) +
  geom_density()+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")
p