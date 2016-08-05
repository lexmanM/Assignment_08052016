# Assignment_08052016

library('lubridate')
require(reshape2)
library(stringi)
install.packages('reshape')
library(reshape)
install.packages('taRifx')
library(taRifx) 

# Set working directory
setwd("C:\\Users\\Lexman.Kumar\\Desktop\\GreedyGame")

# Importing the log file
Data = read.csv("ggevent.log", header = FALSE)

# Adding column names based on the description given 
colnames(Data) = c('ai5','debug','random','sdkv','event','V6','V7','ts','game_id')

# Cleaning the column entries
Data$Device_id = substr(Data$ai5,16, 50)
Data$sdk_version <- substr(Data$sdkv,7,12)
Data$sdk_version <- gsub("}"," ",Data$sdk_version)
Data$event_info <- substr(Data$event,16,25)
Data$gameid <- substr(Data$game_id,10,18) 
Data$timestamp <- substr(Data$ts,21,45)
Data$ts_id <- substr(Data$V6,5,stri_locate(pattern = '}', Data$V6, fixed = TRUE)-1)

# Preparing a dataset to work on
Working_data <- Data[,c('Device_id','sdk_version','event_info','gameid','ts_id','timestamp')]

# Calculating number of unique devices, games and sdk versions
No_of_devices <- unique(Working_data$Device_id) #17419
No_of_games <- unique(Working_data$gameid) #20
No_of_sdkv <- unique(Working_data$sdk_version) #7

# Extracting timestamp info from epoch value
Working_data$ts <- as.POSIXct(as.numeric(Working_data$ts_id)/1000, origin="1970-01-01")
NAs_in_ts <- Working_data[which(is.na(Working_data$ts)),] #0

# Making an unique ID at a Game-device and Game-devive-sdkv level 
Working_data$game_device <- paste(Working_data$gameid, Working_data$Device_id, sep = "-")
Working_data$game_device_sdkv <- paste(Working_data$game_device, Working_data$sdk_version,sep = "-")

# To check the no of unique mappings between game-device and game-device-sdkv
No_of_gamedevicesdkcombo <- unique(Working_data$game_device_sdkv) #17492
No_of_gamedevice <- unique(Working_data$game_device) #17492

# Using game-device as the level to analyse and subsetting the data
Working_data_event <- Working_data[,c('event_info','gameid','Device_id','game_device','ts')]

# Sorting the data by ascending order of timestamp at a game-device level
Working_data_event_sorted <- Working_data_event[order(Working_data_event$game_device,Working_data_event$ts),]

# No NAs in the event_info column with either ggstart or ggstop
ggstart <- Working_data_event_sorted[Working_data_event_sorted$event_info=="ggstart",]#88786 rows
ggstop <- Working_data_event_sorted[Working_data_event_sorted$event_info=="ggstop",]#82935 rows #Total rows 171721

# Adding a flag for Start and Stop based on ggstart and ggstop
Working_data_event_sorted$flag <- ifelse(Working_data_event_sorted$event_info=="ggstart",'Start','Stop')
NAs_in_flag <- Working_data_event_sorted[which(is.na(Working_data_event_sorted$flag)),]

# Using only the required columns
Final_data <- Working_data_event_sorted[,c('game_device','flag','ts')]

# Quality Check with the Start-Stop info
Check_no_of_start_stop_gamedevice <- cast(Final_data, game_device ~ flag, fun.aggregate = length)
Check_no_of_start_stop_gamedevice$difference <- Check_no_of_start_stop_gamedevice$Start - Check_no_of_start_stop_gamedevice$Stop
summary(Check_no_of_start_stop_gamedevice$difference)
Check_no_of_start_stop_gamedevice$flag_diff <- ifelse(Check_no_of_start_stop_gamedevice$difference > 1, 1, ifelse(Check_no_of_start_stop_gamedevice$difference < (-1), 1, 0))
sum(Check_no_of_start_stop_gamedevice$flag_diff) #1958 game_device has irregular starts and stops

# Finding the time interval between starts and stops and stops and starts given the game_device id is the same
Final_data$time <- ifelse(Final_data$game_device==shift(Final_data$game_device),difftime(time1 = shift(Final_data$ts),time2 =  Final_data$ts),0)

# Extract the session time information and the session info
Final_data$session_time <- ifelse(Final_data$flag=='Start',Final_data$time,ifelse(Final_data$time<30,0,0))
Final_data$session_add <- ifelse(Final_data$flag=='Stop',ifelse(Final_data$time>30,1,0),ifelse(Final_data$game_device==shift(Final_data$game_device),0,1))

# Checking for NAs in the session time and session add columns
Final_data[is.na(Final_data$session_time),] #No NAs
Final_data[is.na(Final_data$session_add),] #No NAs

# To start with the first session 
Final_data[1,6] <- 1

# Calculating Number of sessions and total session time
sum(Final_data$session_add) #16141 sessions - Total sessions
sum(Final_data$session_time) #25929766 secs

# Finding Valid sessions
Final_data$Valid_sessions <- ifelse(Final_data$session_time>60, Final_data$session_time, 0)
Final_data$Session_cum <- cumsum(Final_data$session_add)
max(Final_data$Session_cum)

# Aggregating the session time based on the session number
Session <- aggregate(formula = Valid_sessions ~ Session_cum, data = Final_data, FUN = sum)
sum(Session$Valid_sessions) #25382284

# Removing invalid sessions from the Valid Session table
Valid_session <- Session[Session$Valid_sessions!=0,]
sum(Valid_session$Valid_sessions) #25382284
Valid_session$Session_cum <- NULL

# Average session time (Valid)
No_of_valid_sessions <- nrow(Valid_session) # 10979 - Valid sessions
Average_session_time <- sum(Valid_session$Valid_sessions)/No_of_valid_sessions #2311 secs - Average session time (Valid)

# Removing the highest 5% percentile from the data - looking at the distribution 
Valid_session_95per <- Valid_session[Valid_session$Valid_sessions < quantile(Valid_session$Valid_sessions, 0.95), ]

# Calculating average session time and number of valid sessions after subsetting for 95 percentile
No_of_valid_sessions_95per <- length(Valid_session_95per) # 10430 - Valid sessions
Average_session_time_95per <- sum(Valid_session_95per)/No_of_valid_sessions_95per #685 secs - Average session time (Valid) 

