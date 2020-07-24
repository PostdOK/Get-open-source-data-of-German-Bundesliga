# Script to transform the BuLi data of the BTLLasso-Package into match-by-match data frame. 
# Data includes Total distance covered (km), ball position (%), Tackling rate (%), Shots on goal (n), Completion rate for passes (%), fouls suffered (n), offsides (n) for all seasons
# Data also includes corners for the 2016/17 and 2017/18 season
# Note that the final result is displayed as ordinal variable:
# 1 = if the home team has won by at least 2 goals
# 2 = if the home team has won by one goal
# 3 = tie
# 4 = if the away team has won by one goal
# 5 = if the away team has won by at least 2 goals
# For this example, I use the 2016/2017 season
# To better access it, Z1 and Y5 are extracted from list and renamed properly
install.packages('BTLLasso')
library(BTLLasso)
data("Buli1617")
GeneralInformation<-Buli1617$Y5
DataMatchdays<-Buli1617$Z1

# Transformation process
#
# Create match-by-match data frame. All variables that already are listed as match-by-match can be included right away
Match_by_Match<-data.frame("Home"=GeneralInformation$first.object, "Away"=GeneralInformation$second.object, "ResultOrdinal"=GeneralInformation$response)
# Assign team names
for (i in 1:18){
  Match_by_Match$Home[which(!is.na(match(Match_by_Match$Home,i)))]<-GeneralInformation$object.names[i]
  Match_by_Match$Away[which(!is.na(match(Match_by_Match$Away,i)))]<-GeneralInformation$object.names[i]
}
# Add Matchday, then reorder (cosmetically)
Match_by_Match["Matchday"]=ceiling(as.numeric(rownames(Match_by_Match))/9)
Match_by_Match<-Match_by_Match[,c("Matchday","Home","Away","ResultOrdinal")]
# Add match-by-match data for both teams and the difference for each variable
FirstColumn=5
ColumnsPerTeam=ncol(DataMatchdays)/18
for (j in 1:nrow(Match_by_Match)){
  Match_by_Match[j,FirstColumn:(FirstColumn+ColumnsPerTeam-1)]<-DataMatchdays[Match_by_Match$Matchday[j],which(regexpr(Match_by_Match$Home[j],colnames(DataMatchdays))>1)]
  Match_by_Match[j,(FirstColumn+ColumnsPerTeam):(FirstColumn+ColumnsPerTeam*2-1)]<-DataMatchdays[Match_by_Match$Matchday[j],which(regexpr(Match_by_Match$Away[j],colnames(DataMatchdays))>1)]
  Match_by_Match[j,(FirstColumn+ColumnsPerTeam*2):(FirstColumn+ColumnsPerTeam*3-1)]<-Match_by_Match[j,FirstColumn:(FirstColumn+ColumnsPerTeam-1)]-Match_by_Match[j,(FirstColumn+ColumnsPerTeam):(FirstColumn+ColumnsPerTeam*2-1)]
}
# Add colnames
colnames(Match_by_Match)[FirstColumn:(FirstColumn+ColumnsPerTeam-1)]<-paste(unique(gsub("\\.\\S+","",colnames(DataMatchdays))),"Home",sep = ".")
colnames(Match_by_Match)[(FirstColumn+ColumnsPerTeam):(FirstColumn+ColumnsPerTeam*2-1)]<-paste(unique(gsub("\\.\\S+","",colnames(DataMatchdays))),"Away",sep = ".")
colnames(Match_by_Match)[(FirstColumn+ColumnsPerTeam*2):(FirstColumn+ColumnsPerTeam*3-1)]<-paste(unique(gsub("\\.\\S+","",colnames(DataMatchdays))),"Diff",sep = ".")
