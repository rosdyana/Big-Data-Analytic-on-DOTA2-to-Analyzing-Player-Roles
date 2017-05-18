library(ROpenDota)

# get heroes data and save it to csv
# heroes = data.frame(get_heroes())
# write.table(heroes[,1:5], file = "heroes.csv",row.names=FALSE, na="", sep=",")
# loadHeroes <- data.frame(read.csv("heroes.csv", header = T, sep = ","))

# hero roles
# carry
carry = c(1,4,6,8,9,10,11,12,13,15,17,18,19,21,23,28,32,34,35,36,39,41,42,43,44,46,47,48,49,52,53,54,56,59,60,61,63,67,69,70,71,72,73,74,75,76,77,78,80,81,82,89,93,94,95,99,104,106,109,113,114)
support = c(3,5,7,20,25,26,27,30,31,37,40,50,57,58,64,66,68,79,83,84,85,86,87,90,91,92,101,102,108,110,111,112)
initiator = c(2,7,13,16,29,38,51,55,65,97,100,103,96)
nuker = c(22,45,98,105,107)
escape = c(62)
disabler = c(88,14,33)
# total = length(carry)+length(support)+length(initiator)+length(nuker)+length(escape)+length(disabler)

# get all items and save it to csv
# items = data.frame(get_items())
# write.table(items[,1:6], file = "items.csv",row.names=FALSE, na="", sep=",")
# loadItems <- data.frame(read.csv("heroes.csv", header = T, sep = ","))

# get all match id from TI 2014 - 2016
# TI2014 = read.csv("TI2014.csv", header = T, sep = ",")
# TI2015 = read.csv("TI2015.csv", header = T, sep = ",")
# TI2016 = read.csv("TI2016.csv", header = T, sep = ",")
# KIEV = read.csv("KIEVMAJOR.csv", header = T, sep = ",")
# MANILA = read.csv("MANILAMAJOR.csv", header = T, sep = ",")
# SHANGHAI = read.csv("SHANGHAIMAJOR.csv", header = T, sep = ",")
# BOSTON = read.csv("BOSTONMAJOR.csv", header = T, sep = ",")
# merge the data
# matchesData <- rbind(MANILA, SHANGHAI, BOSTON)
 matchesData <- read.csv("TI2016.csv", header = T, sep = ",")

# the data consist 10 match id in one match because 10 player have own macth id
matchesId <- unique(matchesData$match_id)
# x = get_match_details(3188810567)
# x$players$
# dataset format 
# accountid ,level, kda, kill, death, assist , neutral kill, obs, sen, GPM, XPM, last hit, denied, Hero damaged, hero healing, tower damage,tower kill, gold, XP, hero as class
for ( i in matchesId )
{
  print(i)
  x = get_match_details(i)
  account_id = x$player$account_id
  level = x$player$level
  kda = x$players$kda
  kill = x$players$kills
  if(is.null(kill)) kill=0
  death = x$players$deaths
  if(is.null(death)) death=0
  assist = x$players$assists
  if(is.null(assist)) assist=0
  NK = x$players$neutral_kills
  if(is.null(NK)) NK=0
  obs = x$players$obs_placed
  if(is.null(obs))obs=0
  sen = x$players$sen_placed
  if(is.null(sen)) sen=0
  GPM = x$players$gold_per_min
  if(is.null(GPM)) GPM=0
  XPM = x$players$xp_per_min
  if(is.null(XPM)) XPM=0
  LH = x$players$last_hits
  if(is.null(LH)) LH=0
  DN = x$players$denies
  if(is.null(DN)) DN=0
  HD = x$players$hero_damage
  if(is.null(HD)) HD=0
  HH = x$players$hero_healing
  if(is.null(HH)) HH=0
  TD = x$players$tower_damage
  if(is.null(TD)) TD=0
  TK = x$players$tower_kills
  if(is.null(TK)) TK=0
  gold = x$players$total_gold
  if(is.null(gold)) gold=0
  XP = x$players$total_xp
  if(is.null(kill)) kill=0
  hero = x$players$hero_id
  
  y = data.frame(account_id, level, kda, kill, death, assist,obs, sen, NK, GPM, XPM, LH, DN, HD, HH, TD,TK, gold, XP, hero )
  y[is.na(y)] <- 0
  write.table(y, file = "datasets.csv",row.names=FALSE, col.names=FALSE, sep=",",append = T,quote = )
}

# added class for training data - need to clasify more :(
# full set class
datasets <- data.frame(read.csv("datasets.csv", sep = ","))
colnames(datasets)<- c("account_id","level","kda","kill","death", "assist", "observer","sentry","NK", "GPM", "XPM", "LH", "DN","HD", "HH", "TD", "TK", "gold", "XP", "hero_id")
datasets$hero_id[datasets$hero_id %in% carry] <- "carry"
datasets$hero_id[datasets$hero_id %in% support] <- "support"
datasets$hero_id[datasets$hero_id %in% disabler] <- "support"
datasets$hero_id[datasets$hero_id %in% escape] <- "support"
datasets$hero_id[datasets$hero_id %in% initiator] <- "ganker"
datasets$hero_id[datasets$hero_id %in% nuker] <- "ganker"
datasets$hero_id[datasets$TK > 5 ] <- "pusher"
datasets$hero_id[datasets$assist > 10 ] <- "roaming-support"
datasets$hero_id[datasets$HH > 2000 ] <- "support"
datasets$hero_id[datasets$death > 20 ] <- "feeder"
datasets$account_id <- NULL
write.table(datasets, file = "trainingdata.csv",row.names=FALSE, col.names=FALSE, sep=",",append = T,quote = )

# reduced class
datasets <- data.frame(read.csv("datasets.csv", sep = ","))
colnames(datasets)<- c("account_id","level","kda","kill","death", "assist", "observer","sentry","NK", "GPM", "XPM", "LH", "DN","HD", "HH", "TD", "TK", "gold", "XP", "hero_id")
datasets$hero_id[datasets$hero_id %in% carry] <- "carry"
datasets$hero_id[datasets$hero_id %in% support] <- "support"
datasets$hero_id[datasets$hero_id %in% disabler] <- "support"
datasets$hero_id[datasets$hero_id %in% escape] <- "support"
datasets$hero_id[datasets$hero_id %in% initiator] <- "carry"
datasets$hero_id[datasets$hero_id %in% nuker] <- "carry"
datasets$hero_id[datasets$TK > 5 ] <- "carry"
datasets$hero_id[datasets$assist > 10 ] <- "support"
datasets$hero_id[datasets$HH > 2000 ] <- "support"
datasets$account_id <- NULL
write.table(datasets, file = "reduced_trainingdata.csv",row.names=FALSE, col.names=FALSE, sep=",",append = T,quote = )
