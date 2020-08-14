###########
#STEP1 Import Data
##########

library(tidytext)
#authenticiation
library(spotifyr)
get_spotify_access_token()
Sys.setenv(SPOTIFY_CLIENT_ID = '57a57fe2e5b44b2bab703613bb86725e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'de4e3ed30440488887e56a9c56800acb')

access_token <- get_spotify_access_token()
rm(list = ls())
library(spotifyr)
smiths <- get_artist_audio_features('the smiths')

#########Lyrics

Meat <- get_album_data("The Smiths","Meat Is Murder",
                       authorization = get_spotify_access_token())
Queen <- get_album_data("The Smiths","The Queen is Dead",
                        authorization = get_spotify_access_token())
Strangeways <- get_album_data("The Smiths","Strangeways, Here We Come",
                              authorization = get_spotify_access_token())
TheSmiths <- get_album_data("The Smiths","The Smiths",
                            authorization = get_spotify_access_token())

Smithslyrics <- rbind(rank_lyrics_1, rank_lyrics_2, rank_lyrics_3,
                      rank_lyrics_4, rank_lyrics_5, rank_lyrics_6,
                      rank_lyrics_7, rank_lyrics_8, rank_lyrics_9,
                      rank_lyrics_10, rank_lyrics_11)

Smithslyrics$album <- "The Smiths"
TheSmiths1 <- TheSmiths
totallyrics <- rbind(Strangewayslyrics, Queenlyrics,
                     Meatlyrics, Smithslyrics)

###########
#STEP2 Tidy Text
##########

library(tidytext)
tidy_lyrics <- totallyrics %>%
  unnest_tokens(word, lyric) 
tidy_lyrics <- tidy_lyrics %>%
  anti_join(stop_words)  %>%
  select(-c("line"))
tidy_lyrics_count <- tidy_lyrics %>%
  group_by(track_number, album) %>%
  dplyr::count(word, sort = TRUE)
top_words <- tidy_lyrics_count %>%
  filter(n>10)
library(tidytext)
# install.packages("textdata")
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

###inner join
tidy_lyrics_sent <- tidy_lyrics %>%
  inner_join(get_sentiments("bing"))  %>%
  mutate(pos_score = ifelse(sentiment == "positive", 1, 0))
tidy_lyrics_sent$track_number <- as.factor(tidy_lyrics_sent$track_number)
class(tidy_lyrics_sent$pos_score)
tidy_lyrics_sent_group <- tidy_lyrics_sent %>%
  dplyr::group_by(album, track_number) %>%
  dplyr::summarise(new = mean(pos_score))
names(tidy_lyrics_sent_group) <- c("album_name", "track_n", "positive mean")

###can potentially do an analysis of the keys in each track
working_data <- rbind(Meat, Queen, Strangeways, TheSmiths) %>%
  select(album_release_date, tempo, time_signature, track_name, key_name, mode_name, key_mode, track_n, album_name)

###merge these two together
table(working_data$key_mode)
majors <- working_data %>%
  filter(mode_name == 'minor')

###put them together ##make positive mean
tidy_lyrics_sent_group$track_n <- as.integer(tidy_lyrics_sent_group$track_n)
tidy_lyrics_sent_group$album_name[tidy_lyrics_sent_group$album_name=="Meat is Murder"] <- "Meat Is Murder"
tidy_lyrics_sent_group$album_name[tidy_lyrics_sent_group$album_name=="Strangeways"] <- "Strangeways, Here We Come"
merged <- left_join(working_data, tidy_lyrics_sent_group, by = c("track_n", "album_name"), keep = TRUE)
merged <- merged %>%
  filter(album_name != "Rank") %>%
  drop_na()
merged$key_name <- as.factor(merged$key_name)
merged$mode_name <- as.factor(merged$mode_name)
merged$posmean <- merged$`positive mean`
summary(merged)
new <- merged %>% arrange(desc(`positive mean`))

sample <- merged %>%
  select(key_name, album_name, mode_name) %>%
  group_by(album_name, mode_name) %>%
  dplyr::summarise(n = n())

sample2 <- merged %>%
  group_by(album_name) %>%
  dplyr::summarise(mean = mean(posmean))
sample2

afinn <- tidy_lyrics %>%
  inner_join(get_sentiments("afinn"))%>% 
  group_by(album,track_number) %>%
  dplyr::summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") %>%
  ungroup()
afinn <- add_row(afinn, album = "Meat is Murder", track_number = 8,
                 sentiment = -10, method = "AFINN")

#alter working data
working_data$track_number <- working_data$track_n
working_data$album <- working_data$album_name
working_data <- working_data %>%
  mutate(album = recode(album,
                        `Meat Is Murder` = "Meat is Murder",
                        `Strangeways, Here We Come` = "Strangeways"))
##merge
merged <- left_join(working_data, afinn, by = c("track_number", "album"), keep = TRUE)
rows <-  c("album", "track_number", "posmean")
colnames(tidy_lyrics_sent_group) <- rows
class(tidy_lyrics_sent_group$track_number)
tidy_lyrics_sent_group$track_number <- as.numeric(tidy_lyrics_sent_group$track_number)
##add in the other lexicon
merged <- left_join(merged, tidy_lyrics_sent_group, by = c("track_number", "album"), keep = TRUE)


###########
#STEP3 Visualization
##########

plot <- afinn %>%
  ggplot(aes(track_number, sentiment, fill= sentiment)) +
  geom_col() +
  facet_wrap(~album, ncol = 1, scales = "free_y") +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title.x.bottom = element_blank())
plot

##do the sentiments match up with the keys
class(merged$sentiment)
par(mar = c(1,1,1,1))
plot <- ggplot(merged, aes (x = mode_name, y = sentiment))+
  geom_boxplot(width = .3) +
  theme(axis.title = element_text(size = 15)) +
  facet_wrap(~album) +
  geom_point(aes(colour = key_name, size = 3, alpha = .8)) +
  theme(axis.text.x.bottom = element_text(angle = 70, vjust = .5, size = 11)) +
  guides(color = guide_legend("Key"), fill = FALSE) +
  scale_size_continuous(guide= FALSE)  +
  scale_alpha_continuous(guide = FALSE) +
  labs( y = "Sentiment") +
  labs(x = "Mode") 
plot
savePlot(file = "mondayplot.png", width = 500, height = 500)

####outables
table <- merged %>%
  dplyr::group_by(album) %>%
  dplyr::summarise(new = mean(posmean))
table
merged$afinn <- as.numeric(merged$afinn)
table <- merged %>%
  dplyr::group_by(album) %>%
  dplyr::summarise(new = mean(sentiment))
table