#upload data
song <- read.csv("songs.csv")

#understand data
table(song$year)
table(song$artistname)
summary(song)
Michael <- subset(song, artistname == "Michael Jackson" & Top10)
summary(Michael)
table(song$timesignature)
which.max(song$tempo)
song[6206,]
SongsTrain <- subset(song, song$year < "2010")
SongsTest <-  subset(song, song$year == "2010")

# remove year songtitle artistname songid and artistid from training set

nonvars <-  c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <-  SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest <-  SongsTest[ , !(names(SongsTest) %in% nonvars) ]

#Create first model
SongsLog1 <-  glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

#check correlation between loudness and energy
cor(song$loudness, song$energy)

#create second model without loudness
SongsLog2 <-  glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#create third model without energy
SongsLog3 <-  glm(Top10 ~ .- energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#predict
Testpredict <- predict(SongsLog3, newdata = SongsTest, type = "response")

#table with 0.45 threshold
table(SongsTest$Top10, Testpredict >= 0.45)

