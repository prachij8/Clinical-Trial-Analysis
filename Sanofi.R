library(dplyr)
library(ggplot2)
#Clinical_trial_data
df<-read.csv("C:\\Users\\prach\\Downloads\\sanofi.csv")
Top_interventions<- df %>%
  group_by(Interventions) %>%
  summarise(Total_count = n()) %>%
  arrange(desc(Total_count))
top5_interventions<-head(Top_interventions,5)
df1<-df%>%subset(df$Interventions %in% top5_interventions$Interventions)
pl1 <- ggplot(df1, aes(x=Interventions, fill=Study.Type)) +
  geom_bar() +
  geom_text(
    stat='count', 
    aes(label=..count..), 
    position = position_stack(vjust=0.5), 
    size=3
  )
pl1
table_study_type<-table(df$Study.Type)
df2<-as.data.frame(table_study_type)
df2<-df2%>%rename(Study_Type=Var1 ,Count= Freq)


table_study_status<-table(df$Study.Status)
df3<-as.data.frame(table_study_status)
df3<-df3%>%rename(Study_Status=Var1 ,Count= Freq)


df$Start.Date<-as.Date(df$Start.Date,format= "%d-%m-%Y")
df$Completion.Date<-as.Date(df$Completion.Date,format = "%d-%m-%Y")
df$period_of_completion<-df$Completion.Date-df$Start.Date
df$period_of_completion<-as.numeric(df$period_of_completion)
#df$approx_years<-round(df$period_of_completion/365,2)
df$Phases[df$Phases == ""] <- "Not Reported"
phases <- subset(df, Phases != "Not Reported" & !is.na(Phases))

phases_completion <- phases %>%
  group_by(Phases) %>%
  summarise(avg_days = mean(period_of_completion, na.rm = TRUE))

pl2<-ggplot(phases_completion,aes(x=Phases,y=avg_days))+geom_line(group=1)+
  geom_point(color= "Blue") +  
  geom_text(aes(label = round(avg_days,2)), vjust = 1.4)+
  labs(x = "Phases", y = "Average Days") +  
  ggtitle("Average Completion Days by Phase")  
pl2
#Phase 1 Efficiency: The shortest phase, taking just over half a year, reflects focused, early-stage trials primarily concerned with safety and initial dose range.
#Extended Phase 2: Taking nearly one and a half years, Phase 2 is crucial for detailed efficacy and side effect analysis.
#Prolonged Phase 3: The longest phase at almost two years, highlighting comprehensive testing on larger populations to confirm effectiveness and monitor adverse reactions.
#Comprehensive Phase 4: Slightly shorter than Phase 3 but still extensive, focusing on post-market surveillance to gather long-term data on safety and effectiveness.
phases_completionn <- phases %>%
  group_by(Phases,Study.Status) %>%
  summarise(avg_days = mean(period_of_completion, na.rm = TRUE))
pl3<-ggplot(phases_completionn,aes(x=Phases,y=avg_days,fill=Study.Status))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=round(avg_days,2)),position =position_stack(vjust=0.5),size=3)
pl3
#The high number of withdrawn Phase 2 studies and terminated Phase 3 studies highlights the risks and challenges in clinical research. 
#These figures reflect significant investment and potential setbacks, emphasizing the need for robust early-phase research and effective risk management to mitigate late-stage failures.
phases_table=table(phases$Phases)
phases_df<-as.data.frame(phases_table)
