df<-read.csv("C:\\Users\\prach\\Downloads\\Novo Nordisk clean.csv")
library(dplyr)
library(tidyr)
library(ggplot2)
Top_interventions<- df %>%
  group_by(Interventions,Study.Type) %>%
  summarise(Total_count = n()) %>%
  arrange(desc(Total_count))
top_5<-head(Top_interventions,5)
top_5<-top_5 %>% arrange(Total_count)
pl1 <- ggplot(top_5,aes(x=Interventions,y=Total_count,fill= reorder(Study.Type, -Total_count)))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Total_count), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Interventions", y = "Total Count", title = "Top 5 Interventions by Total Count")
pl1
#Observational studies involve observing and recording the effects of a treatment or intervention without actively intervening. 
#Researchers monitor participants and collect data based on their normal behavior and outcomes.
#Interventional studies, also known as clinical trials, involve actively intervening and then observing the effects. Researchers administer treatments or interventions and monitor outcomes.

tablee<-table(df$Interventions)
df2<-as.data.frame(tablee)
df2<-df2%>%arrange(desc(Freq))
table_status<-table(df$Study.Status)
df_status<-as.data.frame(table_status)
df_status<-df_status %>% arrange(Freq)
df_status <- df_status %>% rename(Study_status = Var1, Count = Freq )
#With 191 out of 239 trials completed (~79.9%), Novo Nordisk shows a high success rate in executing their planned clinical trials. 
#This suggests efficient project management and a strong commitment to seeing trials through to their conclusion.

#pl2<-ggplot(df,aes(x=Phases))+geom_bar(fill="blue")+
  geom_text(stat = "count",aes(label=..count..),position = position_stack(vjust = 0.5))
#pl2
table_inter<-table(df$Study.Type)
df_inter<-as.data.frame(table_inter)
df_inter<-df_inter %>% rename(Study_Type = Var1, Count = Freq)
df$Phases[df$Phases == ""] <- "Not Reported"
Top_interventions<- df %>%
  group_by(Interventions,Study.Type,Phases) %>%
  summarise(Total_count = n()) %>%
  arrange(desc(Total_count))
top_5<-head(Top_interventions,5)
top_5<-top_5 %>% arrange(Total_count)
pl4 <- ggplot(top_5,aes(x=Interventions,y=Total_count,fill= reorder(Phases, -Total_count)))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Total_count), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Interventions", y = "Total Count", title = "Top 5 Interventions by Total Count",fill="Phases")
pl4

str(df)
df$Start.Date <- as.Date(df$Start.Date, format="%d-%m-%Y")
df$Completion.Date<-as.Date(df$Completion.Date,format = "%d-%m-%Y")
df$period_of_completion<-df$Completion.Date-df$Start.Date
phases <- subset(df, Phases != "Not Reported")
phases_completion<-phases %>% group_by(Phases) %>% summarise(avg_days=mean(period_of_completion,na.rm = TRUE))
phases_completion$avg_days <- as.numeric(phases_completion$avg_days)
pl5 <- ggplot(phases_completion, aes(x = Phases, y = avg_days)) +
  geom_line(aes(group=1)) +
  geom_point(color= "Blue") +  
  geom_text(aes(label = round(avg_days,2)), vjust = -0.5, hjust = 1)+
  labs(x = "Phases", y = "Average Days") +  
  ggtitle("Average Completion Days by Phase")  
pl5
#Phases 1 and 2 have relatively shorter completion times (360 and 346 days, respectively). 
#These phases primarily focus on initial safety and efficacy, involving smaller participant groups, which often allows for quicker completion.
#Phases 3 and 4 take significantly longer to complete (716 and 751 days, respectively). 
#These phases involve larger participant groups and more extensive testing to confirm effectiveness, monitor side effects, and compare the new treatment to existing standards.

phases_completion_study_status<-phases %>% group_by(Phases,Study.Status) %>% summarise(avg_days=mean(period_of_completion,na.rm = TRUE))
pl6 <- ggplot(phases_completion_study_status, aes(x = Phases, y = avg_days,fill= reorder(Study.Status,-avg_days))) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(avg_days,2)), position=position_stack(vjust = 0.5))+
  labs(x = "Phases", y = "Average Days",fill="Study Status") +  
  ggtitle("Average Completion Days by Phase")  
pl6
#The longer durations for terminated and withdrawn trials suggest that substantial resources and time are invested before the decision to halt these trials is made. 
#The extensive duration for active, not recruiting trials, particularly in Phases 3 and 4, emphasizes the importance of long-term data collection and monitoring to ensure comprehensive safety and efficacy evaluations.

