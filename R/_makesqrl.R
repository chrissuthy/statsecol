redsq <- read.table("C:\\Users\\csutherland\\Dropbox\\W\\Workshops\\Taught\\NPS unmarked\\Ch10_Occupancy\\SwissSquirrels.txt", h=T)

redsq <- redsq[,c(7,8,9,4,6,10,11,12,13,14,15)]

colnames(redsq) <- c("y.1","y.2","y.3", "elev", "forest","day.1","day.2","day.3","dur.1","dur.2","dur.3")

redsq$elev <- redsq$elev/1000
redsq$forest <- redsq$forest/100

redsq[,6:8] <- redsq[,6:8]-min(redsq[,6:8],na.rm=TRUE)+1
redsq[,9:11] <- round(redsq[,9:11]/60,2)

save(redsq,file="data/redsq.RData")
promptData(redsq)
