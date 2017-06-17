library(ROpenDota)

# get heroes data and save it to csv
# heroes = data.frame(get_heroes())
# write.table(heroes[,1:5], file = "heroes.csv",row.names=FALSE, na="", sep=",")
# loadHeroes <- data.frame(read.csv("heroes.csv", header = T, sep = ","))

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
matchesData <- read.csv("EPICENTER2.csv", header = T, sep = ",")
# matchesData <- rbind(TI2016)
# the data consist 10 match id in one match because 10 player have own macth id
matchesId <- unique(matchesData$match_id)
# x = get_match_details(3240966096)
# aa = data.frame(x$players$purchase)
# if(!is.na(aa)) print("AA") else print("BB")

# dataset format
# accountid ,level, kda, kill, death, assist , neutral kill, GPM, XPM, last hit, denied, Hero damaged, hero healing, tower damage,tower kill, gold, XP, hero as class
for (z in matchesId)
{
  print(z)
  x = get_match_details(z)
  
  level = x$player$level
  kda = x$players$kda
  kill = x$players$kills
  if (is.null(kill))
    kill = 0
  death = x$players$deaths
  if (is.null(death))
    death = 0
  assist = x$players$assists
  if (is.null(assist))
    assist = 0
  NK = x$players$neutral_kills
  if (is.null(NK))
    NK = 0
  GPM = x$players$gold_per_min
  if (is.null(GPM))
    GPM = 0
  XPM = x$players$xp_per_min
  if (is.null(XPM))
    XPM = 0
  LH = x$players$last_hits
  if (is.null(LH))
    LH = 0
  DN = x$players$denies
  if (is.null(DN))
    DN = 0
  HD = x$players$hero_damage
  if (is.null(HD))
    HD = 0
  HH = x$players$hero_healing
  if (is.null(HH))
    HH = 0
  TD = x$players$tower_damage
  if (is.null(TD))
    TD = 0
  TK = x$players$tower_kills
  if (is.null(TK))
    TK = 0
  gold = x$players$total_gold
  if (is.null(gold))
    gold = 0
  XP = x$players$total_xp
  if (is.null(kill))
    kill = 0
  hero = x$players$hero_id
  eff = x$players$lane_efficiency
  if (is.null(eff))
    eff = 0
  lane = x$players$lane_role
  if (is.null(lane))
    lane = 0
  checkMe = data.frame(x$players$purchase)
  Observer = 0
  Sentry = 0
  if (!is.na(checkMe)) {
    Observer = x$players$purchase$ward_observer
    Observer[is.na(Observer)] = 0
    Sentry = x$players$purchase$ward_sentry
    Sentry[is.na(Sentry)] = 0
  }
  
  # class
  players = length(x$players$account_id)
  isRadiant = x$players$isRadiant
  # radiant
  radEFF = x$player$lane_efficiency_pct[1:5]
  if (is.null(radEFF))
    radEFF = 0
  radEFF[is.na(radEFF)] <- 0
  
  radroam = x$players$is_roaming[1:5]
  if (is.null(radroam))
    radEFF = FALSE
  radroam[is.na(radroam)] <- FALSE
  
  radlane = x$players$lane[1:5]
  
  radgold = x$players$gold[1:5]
  if (is.null(radgold))
    radgold = 0
  radgold[is.na(radgold)] <- 0
  
  radObserver = 0
  if (!is.na(checkMe)) {
    radObserver = x$players$purchase$ward_observer[1:5]
    radObserver[is.na(radObserver)] = 0
  }
  
  radSentry = 0
  if (!is.na(checkMe)) {
    radSentry = x$players$purchase$ward_sentry[1:5]
    radSentry[is.na(radSentry)] = 0
  }
  
  # # dire
  dirEFF = x$player$lane_efficiency_pct[6:10]
  if (is.null(dirEFF))
    dirEFF = 0
  dirEFF[is.na(dirEFF)] <- 0
  
  dirroam = x$players$is_roaming[6:10]
  if (is.null(dirroam))
    radEFF = FALSE
  dirroam[is.na(dirroam)] <- FALSE
  
  dirObserver = 0
  if (!is.na(checkMe)) {
    dirObserver = x$players$purchase$ward_observer[6:10]
    dirObserver[is.na(dirObserver)] = 0
  }
  
  dirSentry = 0
  if (!is.na(checkMe)) {
    dirSentry = x$players$purchase$ward_sentry[6:10]
    dirSentry[is.na(dirSentry)] = 0
  }
  
  dirlane = x$players$lane[6:10]
  
  dirgold = x$players$gold[6:10]
  if (is.null(dirgold))
    dirgold = 0
  dirgold[is.na(dirgold)] <- 0
  m_class = c()
  for (i in 1:players) {
    if (isRadiant[i])
    {
      if (radgold[i] == max(radgold)) {
        rolesx = "carry"
      } else if (radEFF[i] == radEFF[order(radEFF)[4]]) {
        rolesx = "carry"
      } else if (radEFF[i] == radEFF[order(radEFF)[5]]) {
        rolesx = "midlaner"
      } else if (radlane[i] == 2) {
        rolesx = "midlaner"
      } else if (radEFF[i] == radEFF[order(radEFF)[3]]) {
        rolesx = "offlaner"
      } else if (radgold[i] == radgold[order(radgold)[3]]) {
        rolesx = "offlaner"
      } else if (radroam[i]) {
        rolesx = "roaming_support"
      } else if (radSentry[i] == max(radSentry)) {
        rolesx = "hard_support"
      } else if (radObserver[i] == max(radObserver)) {
        rolesx = "hard_support"
      } else if (radlane[i] == 3) {
        rolesx = "offlaner"
      } else if (radObserver[i] != 0 && radObserver[i] < max(radObserver)) {
        rolesx = "roaming_support"
      } else if (radSentry[i] != 0 && radSentry[i] < max(radSentry)) {
        rolesx = "roaming_support"
      } else
        rolesx = "unknown"
      m_class <- c(m_class, rolesx)
    } else {
      if (dirgold[i - 5] == max(dirgold)) {
        rolesx = "carry"
      } else if (dirEFF[i - 5] == dirEFF[order(dirEFF)[4]]) {
        rolesx = "carry"
      } else if (dirEFF[i - 5] == dirEFF[order(dirEFF)[5]]) {
        rolesx = "midlaner"
      } else if (dirlane[i - 5] == 2) {
        rolesx = "midlaner"
      } else if (dirEFF[i - 5] == dirEFF[order(dirEFF)[3]]) {
        rolesx = "offlaner"
      } else if (dirgold[i - 5] == dirgold[order(dirgold)[3]]) {
        rolesx = "offlaner"
      } else if (dirroam[i - 5]) {
        rolesx = "roaming_support"
      } else if (dirSentry[i - 5] == max(dirSentry)) {
        rolesx = "hard_support"
      } else if (dirObserver[i - 5] == max(dirObserver)) {
        rolesx = "hard_support"
      } else if (dirlane[i - 5] == 1) {
        rolesx = "offlaner"
      } else if (dirObserver[i - 5] != 0 && dirObserver[i - 5] < max(dirObserver)) {
        rolesx = "roaming_support"
      } else if (dirSentry[i - 5] != 0 && dirSentry[i - 5] < max(dirSentry)) {
        rolesx = "roaming_support"
      } else
        rolesx = "unknown"
      m_class <- c(m_class, rolesx)
    }
    
  }
  y = data.frame(lane, eff, HD, GPM, XPM, DN, HH, NK, Observer, Sentry, m_class)
  y[is.na(y)] <- 0
  write.table(
    y,
    file = "datasets.csv",
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    append = T,
    quote =
  )
}

# split dataset into trainig and testing with 70:30
data = read.csv("datasets_baruloh.csv", sep = "," ,header = F)

# Sample Indexes
indexes = sample(1:nrow(data), size=0.3*nrow(data))

# Split data
test = data[indexes,]
dim(test)  
train = data[-indexes,]
dim(train)
write.table(
  test,
  file = "testing_baruloh.csv",
  row.names = FALSE,
  col.names = FALSE,
  sep = ",",
  append = T,
  quote =
)
write.table(
  train,
  file = "training_baruloh.csv",
  row.names = FALSE,
  col.names = FALSE,
  sep = ",",
  append = T,
  quote =
)
