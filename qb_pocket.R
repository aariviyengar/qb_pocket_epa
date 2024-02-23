library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(nflreadr)
library(dplyr)
library(ggrepel)
out_of_pocket <- load_ftn_charting(seasons=most_recent_season())|>
  filter(is_qb_out_of_pocket==1)|>
  group_by(nflverse_game_id,nflverse_play_id)
in_pocket <- load_ftn_charting(seasons=most_recent_season())|>
  filter(is_qb_out_of_pocket!=1)|>
  group_by(nflverse_game_id,nflverse_play_id)
pbp <- load_pbp(seasons=most_recent_season())|>
  filter(pass==1)|>
  group_by(passer_player_id)|>
  filter(!is.na(epa))
out_of_pocket <- left_join(out_of_pocket,pbp,by=c("nflverse_game_id"="game_id","nflverse_play_id"="play_id"))|>
  group_by(passer_player_id)|>
  summarize(name=first(passer_player_name),
            team=first(posteam),
            passes = n(),
            epa_play_out_of_pocket = mean(epa))|>
  filter(!is.na(name))|>
  filter(passes>=15)
in_pocket <- left_join(in_pocket,pbp,by=c("nflverse_game_id"="game_id","nflverse_play_id"="play_id"))|>
  group_by(passer_player_id)|>
  summarize(name=first(passer_player_name),
            team=first(posteam),
            passes=n(),
            epa_play_in_pocket = mean(epa))
pocket <- left_join(out_of_pocket,in_pocket,by=c("passer_player_id","name","team"))
pocket <- pocket|>
  mutate(epa_difference=epa_play_out_of_pocket-epa_play_in_pocket)

pocket <- left_join(pocket,teams_colors_logos,by=c("team"="team_abbr"))
pocket|>
  ggplot(aes(x=epa_play_in_pocket,y=epa_play_out_of_pocket))+
  geom_point(aes(fill=team_color,color=team_color2),shape=21,alpha=0.9)+
  scale_color_identity(aesthetics = c("fill","color"))+
  geom_text_repel(aes(label=paste(name)))+
  geom_hline(yintercept=mean(pocket$epa_play_out_of_pocket),linetype="dashed")+
  geom_vline(xintercept=mean(pocket$epa_play_in_pocket),linetype="dashed")+
  theme_bw()+
  labs(x="EPA/Play When Throwing From Inside the Pocket",
       y="EPA/Play When Not Throwing From Outside the Pocket",
       title = "EPA/Play When Throwing From Inside and Outside the Pocket",
       subtitle = "2023 Season",
       caption = "By Aariv Iyengar | @AarivAnalytics")+
  theme(panel.grid.major.y = element_blank(),
        plot.title=element_text(size=22, hjust=0.5,face="bold"),
        plot.subtitle=element_text(size=16,hjust=0.5))
ggsave("epa_pocket.png",width=14,height=10,dpi="retina")
