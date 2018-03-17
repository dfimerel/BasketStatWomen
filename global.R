# Data and Functions for the analysis

# load data
scores=read.csv("Data/a1Score.csv",sep="\t",stringsAsFactor=F)
playerScores=read.csv("Data/a1Players.csv",sep="\t",stringsAsFactor=F)
standings=read.csv("Data/standings.csv",sep="\t",stringsAsFactor=F)
teamsNames=unique(scores$HOME)
playerNames=unique(playerScores$Player)



# function 1: get the scores for each team and boxplot all of the teams
allTeamScores <- function (scores=scores,teamsNames=teamsNames) {
	scoresDF=data.frame(matrix(ncol=12,nrow=13))
	colnames(scoresDF)=teamsNames
	rownames(scoresDF)=paste(rep("Game"),1:13,sep="")
	for (i in 1:12) {
		scoresDF[,i]=c(scores[scores$HOME==teamsNames[i],"SCORE1"],scores[scores$AWAY==teamsNames[i],"SCORE2"])
	}
	newScores=melt(scoresDF)
	colnames(newScores)=c("Team","Score") 
	ggplot(newScores,aes(x=Team,y=Score))+geom_boxplot(aes(fill=Team))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +labs(x="Team",y="Points",fill="Team\n") + scale_fill_manual(values=rainbow(13))
}



# function 2: get the score for whole season for each team and make scatterplot
getTeam <- function (scores=scores,teamName) {
	allScore=subfunc(scores,teamName)
	ggplot(allScore,aes(GAME,SCORE))+geom_point(shape=23,aes(fill=PLACE),size=3)+geom_line(colour="black")+theme_bw()+labs(x="Game",y="Points",fill="")+ expand_limits(x=c(0,14))+scale_x_continuous(breaks=c(1:13),labels=c(1:13))
}



# function 3: make boxplot team score home vs away and stats
teamHomeAwayScore <- function (scores=scores,teamName) {
	allScore=subfunc(scores,teamName)
	ggplot(allScore,aes(PLACE,SCORE))+geom_boxplot(fill="darkblue",width=0.8)+labs(x="",y="SCORE",fill="")
}




# function 4: make players scoring points plot
getPlayer <- function (playerScores=playerScores,player) {
	teamName=playerScores[playerScores$Player==player,"Team"]
	output=subfunc(scores,teamName)
	points=as.data.frame((as.numeric(playerScores[playerScores$Player==player,3:13])))
	points[,2]=1:length(points[,1])
	points[,3]=output$PLACE[1:11] #**** FIX THIS CORRECT NUMBER OF GAMES
	colnames(points)=c("POINTS","GAME","PLACE")
	#return(points)
	ggplot(points,aes(GAME,POINTS))+geom_bar(stat="identity",fill="darkmagenta",colour="red")+theme_bw()+theme(panel.grid.minor = element_blank())+labs(x="Game",y="Points",fill="")+ expand_limits(x=c(0,13),y=c(0,30))+scale_x_continuous(breaks=c(1:12),labels=c(1:12))
}




# subfunction
subfunc <- function (scores,teamName) {
	homeScore=scores[scores$HOME==teamName,c("SCORE1","GAME")]
	awayScore=scores[scores$AWAY==teamName,c("SCORE2","GAME")]
	colnames(homeScore)=c("SCORE","GAME")
	colnames(awayScore)=c("SCORE","GAME")
	homeScore[,"PLACE"]="Home"
	awayScore[,"PLACE"]="Away"
	allScore=rbind(homeScore,awayScore)
	allScore=allScore[order(allScore[,2]),]
	return(allScore)
}




