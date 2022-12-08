#loading required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Data loading
data=read.csv('Matches_Data.csv')

#creating the header ribbon
header=dashboardHeader(title="Menu")

#creating the sidebar with menu options and sub-options, with their respective tags
sidebar=
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",
               tabName="home"),
      menuItem("Envisage Data",
               tabName="di"),
      menuItem(
        "Conclusion",
         menuItem("Seasons",
                  tabName="s"),
         menuItem("Teams",
                  tabName="t"),
         menuItem("Individuals",
                  tabName="i")
      )
    )
  )

body=
  dashboardBody(
    tabItems(
      #corresponding to 'home' tab, we have a panel of three tabs: Introduction, Data Description and Data glance with their respective outputs
      tabItem(
        tabName = "home",
        fluidRow( 
          tabsetPanel(
            tabPanel("Introduction",
                     uiOutput("Intro")),
            tabPanel(
              "Data Description",
              fluidRow(
                uiOutput("desc")
              )
            ),
            tabPanel(
              "Data glance",
              fluidPage(
                tableOutput("look1"),
                p("........."),
                tableOutput("look2"),
                #font size adjusted to display the entire table on screen
                useShinyjs(),
                inlineCSS(
                  list("table"="font-size: 9px")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName="di",
        fluidRow(
          tabsetPanel(
            #corresponding to 'Envisage Data' tab, we have a panel of four tabs for statistics of seasons, teams, individuals and toss respectively
            tabPanel(
              "Season statistics",
              br(),
              fluidRow(
                tabsetPanel(
                  #under season statistics, we have two tabs corresponding to victories for target setting and chasing respectively
                  #both of where we ask the user to choose two seasons to compare, and their respective plots shown
                  tabPanel(
                    "Victories batting 1st",
                    fluidRow(
                      column(
                        10,
                        offset=1,
                        br(),
                        fluidRow(
                          column(
                            6,
                            selectInput(
                              'season1',
                              label='Select 1st season',
                              choices=as.factor(
                                        unique(data$season)
                                      ),
                              selected=2009
                            )
                          ),
                          column(
                            6,
                            selectInput(
                              'season2',
                              label='Select 2nd season',
                              choices=as.factor(
                                        unique(data$season)
                                      ),
                              selected=2010
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            6,
                            plotOutput('p1')
                          ),
                          column(
                            6,
                            plotOutput('p2')
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    "Victories batting 2nd",
                     fluidRow(
                       column(
                        10,
                        offset=1,
                        br(),
                        fluidRow(
                          column(
                            6,
                            selectInput(
                              'season3',
                              label='Select 1st season',
                              choices=as.factor(
                                        unique(data$season)
                                      ),
                              selected=2009
                            )
                          ),
                          column(
                            6,
                            selectInput(
                              'season4',
                              label='Select 2nd season',
                              choices=as.factor(
                                        unique(data$season)
                                      ),
                              selected=2010
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            6,
                            plotOutput('p3')
                          ),
                          column(
                            6,
                            plotOutput('p4')
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            #under team statistics, we have two tabs corresponding to Overview for each team, and year-wise details for any dynammically selected team
            tabPanel(
              "Team statistics",
              br(),
              fluidRow(
                tabsetPanel(
                  tabPanel(
                    "Overview",
                    fluidRow(
                      column(
                        10,
                        offset=1,
                        br(),
                        p(
                          "A look at the performance of various franchises over the years through the proportion and percentage of results",
                          align="centre"
                        ),
                        br(),
                        fluidRow(
                          column(
                            6,
                            plotOutput('pl1')
                          ),
                          column(
                            6,
                            plotOutput('pl2')
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    "Yearwise win%",
                    br(),
                    fluidRow(
                      column(
                        10,
                        offset=1,
                        selectInput(
                          'team1',
                          label='Select a team',
                          choices=as.factor(
                                    unique(data$team1)
                                  ),
                          selected='Chennai Super Kings'
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        10,
                        offset=1,
                        plotOutput('p5')
                      )
                    ),
                    fluidRow(
                      column(10,
                        offset=1,
                        p("NOTE: Win percentage 0 implies the franchise didn't participate in that edition")
                      )
                    )
                  )
                )
              )
            ),
            #under individual statistics, we have two tabs corresponding to Umpires and POTM respectively
            #in the former, we visualize the number of matches each umpire has stood at
            #in the latter, a season-wise break up of the most significant match performances is shown
            tabPanel(
              "Individual statistics",
              br(),
              fluidRow(
                tabsetPanel(
                  tabPanel(
                    "Umpires",
                    fluidRow(
                      column(
                        10,
                        offset=1,
                        fluidRow(
                          selectInput(
                            'season5',
                            label='Select season',
                            choices=as.factor(
                                      unique(data$season)
                                    ),
                            selected=2009
                          ),
                          plotOutput('p6')
                        )
                      )
                    )
                  ),
                  tabPanel(
                    "POTM",
                    fluidRow(
                      column(
                        10,
                        offset=1,
                        fluidRow(
                          selectInput(
                           'season6',
                           label='Select season',
                           choices=as.factor(
                                     unique(data$season)
                                   ),
                           selected=2009
                          ),
                          plotOutput('p7'),
                          fluidRow(
                            column(
                              10,
                              offset=1, 
                              p("NOTE: POTM is short for Player of the Match")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            #under toss statistics, we have three tabs
            #Overview tells the impact of toss on the result across seasons and teams
            #Season-wise tells the impact of toss on the result for a season across teams
            #Team-wise tells the impact of toss on the result for a team, across seasons where it has participated
            tabPanel(
              "Toss statistics",
              br(),
              tabsetPanel(
                tabPanel(
                  "Overall",
                  br(),
                  fluidRow(
                    column(
                      10,
                      offset=1,
                      plotOutput('p8')
                    )
                  )
                ),
                tabPanel(
                  "Seasonwise",
                  fluidRow(
                    column(
                      10,
                      offset=1,
                      fluidRow(
                        selectInput(
                        'season7',
                        label='Select season',
                        choices=as.factor(
                                  unique(data$season)
                                ),
                        selected=2009
                        ),
                        plotOutput('p9')
                      )
                    )
                  )
                ),
                tabPanel(
                  "Teamwise",
                  br(),
                  br(),
                  fluidRow(
                    column(
                      10,
                      offset=1,
                      selectInput(
                      'team2',
                      label='Select a team',
                      choices=as.factor(
                                unique(data$team1)
                              ),
                      selected='Chennai Super Kings'
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      10,
                      offset=1,
                      plotOutput('p10')
                    )
                  )
                )
              )
            )
          )
        )
      ),
      #in the following lines, we print the conclusion to the User Interface under each sub-head: Season, Team and Individuals
      tabItem(
        tabName = "s",
        fluidRow(
          h1("IPL as a league has had the competitiveness intact across seasons and venues, as is witnessed by the victory margin, among other factors"),
          br(),
          p("Atleast 50% of the wins batting first have been by under 30 runs or even under 20 runs, whereas wins by more than 50 runs are extremely rare.This is a clear indication that the chasing team was in contention throughout."),
          br(),
          p("Similarly most of the wins chasing have been by 4-6 wickets, wins by under 3 wickets or over 8 wickets are extremely rare. This shows the bowling team wasn't out of contention either, for most parts of the chase."),
          br(),
          p("For the first 8 years, results were almost entirely independent of the toss except for the 2009 season (venue based exception: South Africa). Even the UAE season of 2014 wasn't affected much by toss result."),
          br(),
          p("However, recently a trend has been developing with toss as a major factor in match result. The trend is being witnessed in increasing proportions especially since 2016, the UAE season of 2021 being the epitome of the trend.")
        )
      ),
      tabItem(
        tabName = "t",
        fluidRow(
          h1("All the teams have been competitive"),
          br(),
          p("This is backed by the fact that their win percentages over the years are between 40% and 60%."),
          br(),
          p("Moreover the teams which have had a stable and strong management are the ones in the upper bracket of win percentage and show consistency across years, also due to their ability to handle pressure well."),
          br(),
          p("The franchises which have played across years are seen to have their match results very much independent of the toss result in the long run, and follow the season trends in the short run"),
          br(),
          p("The visuals back the fact that CSK has been the most consistent side, save the two year blip due to a few dishonest individuals creeping up to the top."),
          br(),
          p("Additionally the pictures reveal the fickelity of T20 format, where performance on the day is not a result of consistency, much more than it happens in any other format. More often than not, the most consistent side doesn't end up winning the IPL title")
        )
      ),
      tabItem(
        tabName = "i",
        fluidRow(
          h1("IPL is becoming more Indian"),
          br(),
          p("Visualisation shows us that Indian participation is on the rise"),
          br(),
          p("The proportion of quality Indian umpires has increased which reflects in the umpires selected for IPL matches"),
          br(),
          p("More Indians are turning up for their franchises in crunch situations which is the reason for more Indians securing Player of the Match awards"),
          br(),
          p("Even otherwise, following cricket will show how IPL has developed the Indian talent pool and made India a global force to reckon with, across formats and across conditions"),
          br(),
        )
      )
    )
  )
ui= dashboardPage(skin="blue",header,sidebar, body)

server=function(input,output){
  output$Intro = renderUI(
    #about the data is printed to the left and image to the right
    p(
      br(),
      br(),
      img(src="IPL.jpg",height=300, width=400,align="right"),
      br(),
      br(),
      br(),
      h1("About the data", style="color:red"),
      br(),
      p("The data set pertains to matches played in the",
        tags$span(
          "IPL from 2008 to 2019",
          style="color:blue"
        ),
        "except those abandoned without a ball being bowled.",
      ),
      p(
        "Source :",
        a(
          "Kaggle: ipl-datasets",
          href="https://www.kaggle.com/datasets/anuranroy/ipldatasets"
        )
      )
    )
  )
  output$desc=renderUI(
    #data description has been printed to the interface, formated as statements followed by a bulleted list 
    tags$div(
      tags$br(),
      tags$br(),
      tags$ul(
        tags$li("The dataset has 16 columns and 756 rows."),
        tags$br(),
        tags$li(
          "It contains the following information:",
          tags$br(),
          tags$br(),
          tags$ul(
            tags$li("Season and date"),
            tags$br(),
            tags$li("City and Venue"),
            tags$br(),
            tags$li("Teams involved"),
            tags$br(),
            tags$li("Toss winner and decision"),
            tags$br(),
            tags$li("Whether DLS was applied"),
            tags$br(),
            tags$li("Match winner and win margin"),
            tags$br(),
            tags$li("Player of the match"),
            tags$br(),
            tags$li("Umpires involved"),
          )
        )
      )
    )
  )
  #data glance prints the first 4 and the last 4 rows of the dataset
  output$look1=renderTable(
    {
      print(
        tibble(
          head(data,n=4)
        )
      )
    }
  )
  output$look2=renderTable(
    {
      print(
        tibble(
          tail(data,n=4)
        )
      )
    }
  )
  output$pl1=renderPlot(
    {
      #data frame of matches played by each team
      team_matches=
        data.frame(
          table(
            unlist(
              append(
                data['team1'],
                data['team2']
              )
            )
          )
        )
      colnames(team_matches)=c('team','played')
      #we append the number of victories for each team and then calculate their win%
      team_matches['won']=
        data.frame(
          table(
            unlist(data['winner'])
          )
        )['Freq'][2:15,]
      team_matches['win%']=team_matches['won']/team_matches['played']
      #data frame of number of victories and defeats for each team grouped by the tags 'won' and 'lost'
      team_matches_reshaped=
        data.frame(
          x=team_matches$team,
          y=c(
            team_matches$won,
            team_matches$played-team_matches$won
          ),
          group=c(
            rep('won',nrow(team_matches)),
            rep('lost',nrow(team_matches))
          ),
          `win%`=team_matches$`win%`
        )
      #we plot a bar graph showing the proportion of wins and losses for each team
      ggplot(team_matches_reshaped,aes(x=x,y=y,fill=group))+
        geom_col()+
        labs(x='Teams',y='Results')+
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) # to pretty print the cuts on x-axis diagonally
    }
  )
  output$pl2=renderPlot(
    {
      #similar construction of data frame as the first one above
      team_matches=
        data.frame(
          table(
            unlist(
              append(
                data['team1'],
                data['team2']
              )
            )
          )
        )
      colnames(team_matches)=c('team','played')
      team_matches['won']=
        data.frame(
          table(
            unlist(data['winner'])
          )
        )['Freq'][2:15,]
      team_matches['win%']=team_matches['won']/team_matches['played']
      #we plot a line graph of overall win% for each team across seasons
      ggplot(team_matches,aes(x=team,y=`win%`,group=1))+
        geom_point()+
        geom_line()+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    }
  )
  output$p1=renderPlot(
    {
      #extract the victory margin of teams batting first for the selected season
      x=list(
        data[data$season==input$season1,]['win_by_runs']
      )
      #store the frequency distribution of victory margin as a data frame
      run_victory=
        data.frame(
          table(
            factor(
              cut(
                unlist(x),
                breaks=seq(0,150,10)
              )
            )
          )
        )
      #plot a histogram of the victory margin by runs, shaded distinctly by number of occurences in each class
      ggplot(run_victory,aes(x=Var1,y=Freq,fill=Freq))+
        geom_col()+
        labs(x='Run margin',y='No. of occurences')+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        geom_text(aes(label=Freq),vjust=-0.3,size=3)
    }
  )
  output$p2=renderPlot(
    {
      #a replica of output$p1 for the second season selected for comparison
      x=list(
        data[data$season==input$season2,]['win_by_runs']
      )
      run_victory=
        data.frame(
          table(
            factor(
              cut(
                unlist(x),
                breaks=seq(0,150,10)
              )
            )
          )
        )
      ggplot(run_victory,aes(x=Var1,y=Freq,fill=Freq))+
        geom_col()+
        labs(x='Run margin',y='No. of occurences')+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        geom_text(aes(label=Freq),vjust=-0.3,size=3)
    }
  )
  output$p3=renderPlot(
    {
      #store as a data frame, the frequency distribution of margin of victory by wickets for the selected season
      wicket_victory=
        data.frame(
          table(
            unlist(
              data[data$season==input$season3,]['win_by_wickets']
            )
          )
        )
      #plot a histogram of the victory margin by wickets, shaded distinctly by number of occurences in each class
      ggplot(wicket_victory[2:nrow(wicket_victory),],aes(x=Var1,y=Freq,fill=Freq))+
        geom_col()+
        labs(x='Wicket margin',y='No. of occurences')+
        geom_text(aes(label=Freq),vjust=-0.3,size=3)
    }
  )
  output$p4=renderPlot(
    {
      #a replica of output$p3 for the second season selected for comparison
      wicket_victory=
        data.frame(
          table(
            unlist(
              data[data$season==input$season4,]['win_by_wickets']
            )
          )
        )
      ggplot(wicket_victory[2:nrow(wicket_victory),],aes(x=Var1,y=Freq,fill=Freq))+
        geom_col()+
        labs(x='Wicket margin',y='No. of occurences')+
        geom_text(aes(label=Freq),vjust=-0.3,size=3)
    }
  )
  output$p5=renderPlot(
    {
      #data frame of all franchises ever participated in the IPL
      teams=sort(
        unique(data$team1)
      )
      #an empty data frame to store the win% for each team across seasons
      wins=
        data.frame(
          matrix(,nrow=12,ncol=14)
        )
      colnames(wins)=teams
      rownames(wins)=c(seq(2008,2019))
      #similar to output$pl1, we calculate the win%, this time seasonwise and teamwise
      for(x in 2008:2019){
        team_matches=
          data.frame(
            table(
              unlist(
                append(
                  data[data$season==x,]['team1'],
                  data[data$season==x,]['team2']
                )
              )
            )
          )
        colnames(team_matches)=c('team','played')
        df=
          data.frame(
            table(
              unlist(
                data[data$season==x,]['winner']
              )
            )
          )
        if(x %in% c(2011,2015,2019))
          team_matches['won']=df[df$Freq>2,]['Freq']
        else
          team_matches['won']=df['Freq']
        team_matches['win%']=team_matches['won']/team_matches['played']
        w=c()
        #if franchise didn't participate in a season, assign a win% of 0, else that which is computed for the season
        for(y in teams){
          if(is.na(team_matches[team_matches$team==y,][1,'win%']))
            w=append(w,0)
          else
            w=append(w,
                     team_matches[team_matches$team==y,][1,'win%']
              )
        }
        wins[toString(x),]=w
      }
      #extracting the win% data for the chosen team
      v=wins[input$team1]
      colnames(v)=c('win%')
      #plot an area graph of win% for the team across seasons
      ggplot(v,aes(x=rownames(v),y=`win%`,group=1))+
        geom_area(fill='#AF7C7B')+
        labs(x='Season',y='win%')+
        geom_text(aes(label=round(`win%`,4))) #explicitly mentioning the win% for each season
    }
  )
  output$p6=renderPlot(
    {
      #freqeuncy distribution of no. of matches as a field umpire for each individual
      df=
        data.frame(
          table(
            unlist(
              append(
                data[data$season==input$season5,]['umpire1'],
                data[data$season==input$season5,]['umpire2']
              )
            )
          )
        )
      umpires=data.frame('name'=df$Var1,'matches'=df$Freq)
      #plot a 'colorful' bar graph of the number of matches umpired by various officials
      ggplot(umpires,aes(x=name,y=matches,fill=name))+
        geom_col()+
        theme(axis.text.x = element_text(angle = 60, hjust = 1),legend.position = 'none')+ #remove the legend as it is of no use, the same as x-axis labels
        labs(y='Matches umpired')
    }
  )
  output$p7=renderPlot(
    {
      #plot a data frame of frequency table of the player of match for the selected season and for all seasons combined
      #country manually listed for each player in the latter data frame
      players=data.frame(table(unlist(data[data$season==input$season6,]['player_of_match'])))
      df=data.frame(table(unlist(data['player_of_match'])))
      df=df[-1,]
      df['country']=c('India','West Indies','India','India','India','India','Australia','Australia','India','South Africa','India','Australia','Australia','England','Sri Lanka','West Indies','Australia','Australia','India','India','India','India','India','India','Pakistan','India','Australia','Australia','India','England','New Zealand','Australia','Australia','Australia','Australia','West Indies','South Africa','New Zealand','Australia','West Indies','Sri Lanka','South Africa','Australia','Australia','West Indies','India','Australia','West Indies','India','New Zealand','Australia','Sri Lanka','West Indies','South Africa','England','South Africa','India','South Africa','Australia','India','Australia','Australia','England','India','India','South Africa','India','South Africa','India','India','India','South Africa','India','India','England','England','South Africa','South Africa','South Africa','England','New Zealand','India','New Zealand','New Zealand','South Africa','India','England','South Africa','Australia','India','West Indies','South Africa','West Indies','Sri Lanka','India','India','West Indies','India','India','India','Sri Lanka','England','New Zealand','India','India','India','South Africa','New Zealand','England','West Indies','India','New Zealand','India','South Africa','Sri Lanka','South Africa','Afghanistan','India','India','India','Australia','India','Australia','India','Australia','Sri Lanka','Australia','England','New Zealand','India','India','Australia','India','India','West Indies','India','India','Australia','Australia','India','India','India','Bangladesh','South Africa','India','Australia','India','India','India','India','India','India','India','England','India','India','India','South Africa','India','India','India','South Africa','India','India','India','India','Afghanistan','South Africa','India','Australia','India','India','England','India','India','India','India','England','India','India','India','West Indies','India','India','India','India','India','India','India','India','India','Australia','Bangladesh','Pakistan','India','India','Australia','Sri Lanka','Australia','South Africa','Pakistan','India','West Indies','Australia','India','Australia','India','Sri Lanka','India','England','New Zealand', 'New Zealand','India','Sri Lanka','Pakistan','India','India','India','India','India','India','Sri Lanka','India','India','India','India')
      #using the master data frame of players with their countries, we extract the data for countries of the POTM awardees for a particular season
      x=c()
      for (player in players$Var1){
        if (player!='')
          x=append(x,
                   df[which(df$Var1==player),'country']
            )
        else
          x=append(x,'')
      }
      players['country']=x
      colnames(players)=c('Players','Freq','country')
      #a bar plot of countries vs number of POTM awardees where each bar is subdivided according to the distinct POTM awardees from that country
      ggplot(players[players$Players!='',],aes(x=country,y=Freq,fill=Players))+
        geom_col()+
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        labs(x='Players by country',y='No. of POTM awards')
    }
  )
  output$p8=renderPlot(
    {
      #data frame of toss winner and match winner for each match across seasons
      TossAndRes=data%>%select('season','toss_winner','winner')
      #adding a column which shows the (dis)similarity between toss and match results
      x=c()
      for(i in 1:nrow(TossAndRes)){
        x=append(x,
                 TossAndRes[i,'toss_winner']==TossAndRes[i,'winner']
          )
      }
      TossAndRes['toss=result']=x
      #a bar plot of (dis)similarity between toss and match results where each bar is subdivided by the winning team, after ignoring the matches with NR
      ggplot(TossAndRes[TossAndRes$winner != '',],aes(x=`toss=result`,fill=winner))+
        geom_bar()
    }
  )
  output$p9=renderPlot(
    {
      TossAndRes=data%>%select('season','toss_winner','winner')
      x=c()
      for(i in 1:nrow(TossAndRes)){
        x=append(x,
                 TossAndRes[i,'toss_winner']==TossAndRes[i,'winner']
          )
      }
      TossAndRes['toss=result']=x
      #the data frame is prepared as done previously and then data for the selected season extracted
      SeasonToss=TossAndRes[TossAndRes$season==input$season7,]
      #a bar plot of (dis)similarity between toss and match results where each bar is subdivided by the winning team, after ignoring the matches with NR, for the chosen season
      ggplot(SeasonToss[SeasonToss$winner != '',],aes(x=`toss=result`,fill=winner))+
        geom_bar()
    }
  )
  output$p10=renderPlot(
    {
      #the data frame is prepared as on the previous two occasions
      TossAndRes=data%>%select('season','team1','team2','toss_winner','winner')
      x=c()
      for(i in 1:nrow(TossAndRes)){
        x=append(x,
                 TossAndRes[i,'toss_winner']==TossAndRes[i,'winner']
          )
      }
      TossAndRes['toss=result']=x
      #extracting all matches where the selected team played
      sub1=subset(TossAndRes,team1==input$team2)
      sub2=subset(TossAndRes,team2==input$team2)
      TeamToss=rbind(sub1,sub2)
      #a bar plot of (dis)similarity between toss and match results where each bar is subdivided by the winning team, after ignoring the matches with NR, for the chosen team
      ggplot(TeamToss[TeamToss$winner != '',],aes(x=`toss=result`,fill=winner))+
        geom_bar()
    }
  )
}

#runs the app with the interface and output coded above
shinyApp(ui = ui, server = server)