
library(shiny)
library(plotly)
library(readxl)

# ?????͸? ?о??ɴϴ?.
library(readr)

# ?׷??? ???̺귯???? ?ҷ??ɴϴ?.

# ?ð?�� ??��?մϴ?.
library(tidyverse)

# 2??°?ð?�� ??��?մϴ?.
library(lubridate)

library(reprex)
library(shinydashboard)

# ???? ??��
col1<-runif(255)
col2<-runif(255)
col3<-runif(255)
#########################




#fileEncoding="UTF-8", quote=""

# Read in data , csv???? ????
df <- read_excel("Simulation results_0629_ver3.xlsx")

df<-as.data.frame(df)

df1<-function(x,y) {
  

  # Dataset START_TIME -> Define TIME RANGE therefore Decide df range 
  
  START_TIME_SET<-sort(unique(df$START_TIME),decreasing = FALSE)
  
  END_TIME_SET<-sort(unique(df$END_TIME),decreasing = FALSE)
  
  # Define order time range. Ex) 202006250000 ~ 202006270000
  
  # 1. Compare time SET END TIME Because we want to find start LOT and time.
  
  # 2. Make Unique set time so find the nearnest minimum start time.
  
  # 3. Find row the nearnest minimum start time about df tibble.
  
  # 4. Filter tibble ~ we find to adjustable tibble range
  
  # Choose data date range
  
  # Start time : 202006250000
  
  # End time : 202006290000
  
  END_TIME_SET1 <- which((START_TIME_SET <= x)==TRUE)
  
  END_TIME_SET2 <- START_TIME_SET[max(END_TIME_SET1)]
  
  END_TIME_SET3 <- which(df$START_TIME ==  START_TIME_SET[max(END_TIME_SET1)])
  
  # We find START TIME Search to END TIME ZONE
  
  START_TIME_ROW <- min(END_TIME_SET3)
  
  # START_TIME_ROW FIND!
  
  
  # Define order time range. Ex) 202006250000 ~ 202006290000
  
  START_TIME_SET1 <- which((START_TIME_SET <= y)==TRUE)
  
  START_TIME_SET2 <- START_TIME_SET[max(START_TIME_SET1)]
  
  START_TIME_SET3 <- which(df$START_TIME ==  START_TIME_SET[max(START_TIME_SET1)])
  
  # We find END TIME Search to START TIME ZONE
  
  END_TIME_ROW <- max(START_TIME_SET3)
  
  
  
  # Define Range of df
  
  
  df<-df[START_TIME_ROW:END_TIME_ROW,]
  
  

  
}

df1(202006250000,202007061300)

df2<-df1(202006250000,202007061300)


# Convert to dates
# df$ArrivalTime <- as.Date(df$ArrivalTime, format = "%m/%d/%Y")
# df$Total_CycleTime <- as.Date(df$Total_CycleTime, format = "%m/%d/%Y")
# var<-c(1,2)
# Sample client name
client = "Sample Line"


# Choose colors based on number of resources

#cols<-RColorBrewer::brewer.pal(length(unique(df$MAT_ID)), name = "Set3")


# Time ??ȯ utc?? ktc?? ????(for)
# RES ?????? ?ּҸ? ã?��ϴ?
# ü???ð?�� ???մϴ?.


# ?ߺ??? ????
# subset(df$END_TIME,df$LOT_ID==c[1])
# a<-subset(df$END_TIME,df$LOT_ID==c[1])
# a[2]
# END_TIME2<-ymd_hm(a[2],tz="Asia/Seoul");


#df$RES_NUM<-RES_NUM
#df$MAT_ID_NUM<-MAT_NUM
#EX_TIME<-df$START_TIME
#EX_TIME<-(EX_TIME+150)
#df$EX_TIME<-EX_TIME



ui<-dashboardPage(
  
  dashboardHeader(title="CWNU Scheduler"),
  dashboardSidebar(),
  dashboardBody( fluidRow(
  box(plotlyOutput("plot")),
  box(plotlyOutput("plot2")),
  box(numericInput("x", "START_TIME", 202006250000)),
  box(numericInput("y", "END_TIME", 202007061300)),
  box(title="START_TIME TABLE", solidHeader=TRUE, tableOutput("Start_table")),
  box(title="END_TIME TABLE", solidHeader=TRUE, tableOutput("End_table"))
  
  )
  
  )
  
)




server <- function(input, output){
  
  output$plot<- renderPlotly({
    
    
    LOT_ID<-unique(df$LOT_ID)
    
    MachineName<-df$RES_ID
    # Convert to dates
    # df$ArrivalTime <- as.Date(df$ArrivalTime, format = "%m/%d/%Y")
    # df$Total_CycleTime <- as.Date(df$Total_CycleTime, format = "%m/%d/%Y")
    # var<-c(1,2)
    # Sample client name
    client = "CWNU_Scheduler"
    
    
    # Choose colors based on number of resources
    
    #cols<-RColorBrewer::brewer.pal(length(unique(df$MAT_ID)), name = "Set3")
    
    LOT_ID<-unique(df$LOT_ID)
    MAT_ID<-unique(df$MAT_ID)
    OPER<-unique(df$OPER)
    # selectcols<-sample(cols,size=color_num)
    #df$color <- factor(df$MAT_ID, labels = cols)
    MachineName <- unique(df$RES_ID)
    
    MachineName <- sort(MachineName,decreasing=TRUE)
    
    
    # Time ??ȯ utc?? ktc?? ????(for)
    # RES ?????? ?ּҸ? ã?��ϴ?
    # ü???ð?�� ???մϴ?.
    
    
    RES_NUM<-c()
    Diff<-period()
    OPER_NUM<-c()
    MAT_NUM<-c()
    
    for(i in 1:nrow(df)){
      
      RES_NUM[i]<-which(MachineName==df$RES_ID[i])
      OPER_NUM[i]<-which(OPER==df$OPER[i])
      MAT_NUM[i]<-which(MAT_ID==df$MAT_ID[i])
      
    }
    
    
    
    # ?ߺ??? ????
    # subset(df$END_TIME,df$LOT_ID==c[1])
    # a<-subset(df$END_TIME,df$LOT_ID==c[1])
    # a[2]
    # END_TIME2<-ymd_hm(a[2],tz="Asia/Seoul");
    
    
    df$RES_NUM<-RES_NUM
    df$MAT_ID_NUM<-MAT_NUM
    EX_TIME<-df$START_TIME
    EX_TIME<-(EX_TIME+150)
    df$EX_TIME<-EX_TIME
    
    
    fig <- plot_ly()
    
    
    for(i in 1:length(LOT_ID)) {
      
      mat1<-subset(df$MAT_ID,df$LOT_ID==LOT_ID[i])
      
      mat2<-unique(mat1)
      
      for(j in 1: length(mat2)) {
        
        a1<-subset(df$START_TIME,df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        a2<-subset(df$END_TIME,df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        r1<- subset(df$RES_NUM,df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        m1<- subset(df$MAT_ID,df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        Oper1<- subset(df$OPER,df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        Flow1<- subset(df$FLOW, df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        MAT_NUM1<-subset(df$MAT_ID_NUM, df$LOT_ID==LOT_ID[i]& df$MAT_ID==mat2[j])
        
        fig <- add_trace(fig,
                         x = c(ymd_hm(a1[1],tz="Asia/Seoul"),ymd_hm(a2[2],tz="Asia/Seoul")),  # x0, x1
                         y = c(r1[1],r1[1]),  # y0, y1
                         mode="lines",
                         line = list(color = rgb(col1[MAT_NUM1[1]],col2[MAT_NUM1[1]],col3[MAT_NUM1[1]]), width = 20),

                         # name = MAT_ID[MAT_NUM1[1]],
                         
                         showlegend = F,
                         
                         
                         evaluate = F  # needed to avoid lazy loading
                         
                         
                         
        )
        
      }
    }
    

    
    UNI_MAT_NUM <-c()
    for (i in 1: max(MAT_NUM))
    {
      
      UNI_MAT_NUM[i] = min(which(MAT_NUM==i))
      
    }
    
    
    
    
    
    
    for(i in 1:length(unique(MAT_NUM)) ) {
      
      
      a1<-df$START_TIME[UNI_MAT_NUM[i]]
      r1<- df$RES_NUM[UNI_MAT_NUM[i]]
      
      MAT_NUM1<-df$MAT_ID_NUM[UNI_MAT_NUM[i]]
      
      
      fig <- add_trace(fig,
                       x = c(ymd_hm(a1[1],tz="Asia/Seoul")),  # x0, x1
                       y = c(r1[1],r1[1]),  # y0, y1
                       mode="lines",
                       line = list(color = rgb(col1[MAT_NUM1[1]],col2[MAT_NUM1[1]],col3[MAT_NUM1[1]]), width = 20),
                       
                       name = MAT_ID[MAT_NUM1[1]],
                       
                       showlegend = T,

                       evaluate = F  # needed to avoid lazy loading
                       
                       
      )
      
    }
    
    
    
    
    
    
    fig <- layout(fig,
                  
                  # Axis options:
                  # 1. Remove gridlines
                  # 2. Customize y-axis tick labels and show task names instead of numbers
                  
                  xaxis = list(title='Time',showgrid = T, tickfont = list(color = "black")),
                  
                  yaxis = list(title='Machine',showgrid = T, tickfont = list(color = "black"),
                               tickmode = "array", tickvals = 1:length(unique(MachineName)), ticktext = (unique(MachineName)),
                               domain = c(0, 0.9)),
                  
                  plot_bgcolor = "white",  # Chart area color
                  paper_bgcolor = "white") # Axis area color
    
    
    fig
    
    
    b <- list(xref = "paper",
              yref = "paper",
              x = 0.1,
              y = 1,
              xanchor = "left",
              text = paste0(client),
              font = list(color = '#264E86', size = 20, family = "Times New Roman"),
              ax = 0,
              ay = 0,
              align = "left",
              showarrow = FALSE)
    
    
    # fig <- fig %>% layout(annotations = a)
    
    fig <- fig %>% layout(annotations = b)
    
    fig
    
    
    
    
    
  })
  
  output$plot2<- renderPlotly({
    
    df2<-df1(input$x,input$y)
    
    LOT_ID<-unique(df2$LOT_ID)
    
    MachineName<-df2$RES_ID
    
    # Convert to dates
    # df2$ArrivalTime <- as.Date(df2$ArrivalTime, format = "%m/%d/%Y")
    # df2$Total_CycleTime <- as.Date(df2$Total_CycleTime, format = "%m/%d/%Y")
    # var<-c(1,2)
    # Sample client name
    
    client = "START_TIME ~ END_TIME"
    
    # Choose colors based on number of resources
    

    #cols<-RColorBrewer::brewer.pal(length(unique(df2$MAT_ID)), name = "Set3")
    

    LOT_ID<-unique(df2$LOT_ID)
    MAT_ID<-unique(df2$MAT_ID)
    OPER<-unique(df2$OPER)
    # selectcols<-sample(cols,size=color_num)
    #df2$color <- factor(df2$MAT_ID, labels = cols)
    MachineName <- unique(df2$RES_ID)
    
    MachineName <- sort(MachineName,decreasing=TRUE)
    
    
    # Time ??ȯ utc?? ktc?? ????(for)
    # RES ?????? ?ּҸ? ã?��ϴ?
    # ü???ð?�� ???մϴ?.
    
    
    
    RES_NUM<-c()
    Diff<-period()
    OPER_NUM<-c()
    MAT_NUM<-c()
    
    for(i in 1:nrow(df2)){
      
      RES_NUM[i]<-which(MachineName==df2$RES_ID[i])
      OPER_NUM[i]<-which(OPER==df2$OPER[i])
      MAT_NUM[i]<-which(MAT_ID==df2$MAT_ID[i])
      
    }
    
    
    
    # ?ߺ??? ????
    # subset(df2$END_TIME,df2$LOT_ID==c[1])
    # a<-subset(df2$END_TIME,df2$LOT_ID==c[1])
    # a[2]
    # END_TIME2<-ymd_hm(a[2],tz="Asia/Seoul");
    
    
    df2$RES_NUM<-RES_NUM
    df2$MAT_ID_NUM<-MAT_NUM
    EX_TIME<-df2$START_TIME
    EX_TIME<-(EX_TIME+150)
    df2$EX_TIME<-EX_TIME
    
    
    fig <- plot_ly()
    
    
    for(i in 1:length(LOT_ID)) {
      
      mat1<-subset(df2$MAT_ID,df2$LOT_ID==LOT_ID[i])
      
      mat2<-unique(mat1)
      
      for(j in 1: length(mat2)) {
        
        a1<-subset(df2$START_TIME,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        a2<-subset(df2$END_TIME,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        r1<- subset(df2$RES_NUM,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        m1<- subset(df2$MAT_ID,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        Oper1<- subset(df2$OPER,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        Flow1<- subset(df2$FLOW, df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        MAT_NUM1<-subset(df2$MAT_ID_NUM, df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        
        fig <- add_trace(fig,
                         x = c(ymd_hm(a1[1],tz="Asia/Seoul"),ymd_hm(a2[2],tz="Asia/Seoul")),  # x0, x1
                         y = c(r1[1],r1[1]),  # y0, y1
                         mode="lines",
                         line = list(color = rgb(col1[MAT_NUM1[1]],col2[MAT_NUM1[1]],col3[MAT_NUM1[1]]), width = 20),
                         
                         # name = MAT_ID[MAT_NUM1[1]],
                         
                         showlegend = F,
                         

                         
                         
                         evaluate = F  # needed to avoid lazy loading
                         
                         
                         
        )
        
      }
    }
    
    
    for(i in 1:length(LOT_ID) ) {
      
      mat1<-subset(df2$MAT_ID,df2$LOT_ID==LOT_ID[i])
      
      mat2<-unique(mat1)
      
      for(j in 1: length(mat2)) {
        
        a1<-subset(df2$START_TIME,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        a2<-subset(df2$END_TIME,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        r1<- subset(df2$RES_NUM,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        m1<- subset(df2$MAT_ID,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        Oper1<- subset(df2$OPER,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        Flow1<- subset(df2$FLOW, df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        MAT_NUM1<-subset(df2$MAT_ID_NUM, df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        a3<-subset(df2$EX_TIME,df2$LOT_ID==LOT_ID[i]& df2$MAT_ID==mat2[j])
        
        fig <- add_trace(fig,
                         x = c(ymd_hm(a1[1],tz="Asia/Seoul"),ymd_hm(a3[1],tz="Asia/Seoul")),  # x0, x1
                         y = c(r1[1],r1[1]),  # y0, y1
                         mode="lines",
                         line = list(color = "black", width = 20),
                         
                         
                         showlegend = F,
                         
                         hoverinfo = "text",
                         
                         # Create custom hover text
                         text = paste("LOT_ID: ", LOT_ID[i], "<br>",
                                      "OPER: ", Oper1[1], "<br>",
                                      "MAT_ID",  m1[1], "<br>",
                                      "StartTime: ", ymd_hm(a1[1],tz="Asia/Seoul"), "KTS<br>",
                                      "EntTime", ymd_hm(a2[2],tz="Asia/Seoul"), "KTS<br>",
                                      "FLOW: ", Flow1[1]),                   
                         
                         evaluate = F  # needed to avoid lazy loading
                         
                         
        )
        
      }
    }
    
    
    UNI_MAT_NUM <-c()
    for (i in 1: max(MAT_NUM))
    {
      
      UNI_MAT_NUM[i] = min(which(MAT_NUM==i))
      
    }
    
    
    

    for(i in 1:length(unique(MAT_NUM)) ) {
      
      
      a1<-df2$START_TIME[UNI_MAT_NUM[i]]
      r1<- df2$RES_NUM[UNI_MAT_NUM[i]]
      
      MAT_NUM1<-df2$MAT_ID_NUM[UNI_MAT_NUM[i]]
      
      
      fig <- add_trace(fig,
                       x = c(ymd_hm(a1[1],tz="Asia/Seoul")),  # x0, x1
                       y = c(r1[1],r1[1]),  # y0, y1
                       mode="lines",
                       line = list(color = rgb(col1[MAT_NUM1[1]],col2[MAT_NUM1[1]],col3[MAT_NUM1[1]]), width = 20),
                       
                       name = MAT_ID[MAT_NUM1[1]],
                       
                       showlegend = T,
                       
                       
                       evaluate = T  # needed to avoid lazy loading
                       
                       
      )
      
    }
    
    
    
    
    
    
    fig <- layout(fig,
                  
                  # Axis options:
                  # 1. Remove gridlines
                  # 2. Customize y-axis tick labels and show task names instead of numbers
                  
                  xaxis = list(title='Time',showgrid = T, tickfont = list(color = "black")),
                  
                  yaxis = list(title='Machine',showgrid = T, tickfont = list(color = "black"),
                               tickmode = "array", tickvals = 1:length(unique(MachineName)), ticktext = (unique(MachineName)),
                               domain = c(0, 0.9)),
                  
                  plot_bgcolor = "white",  # Chart area color
                  paper_bgcolor = "white") # Axis area color
    
    
    fig
    
    
    
    b <- list(xref = "paper",
              yref = "paper",
              x = 0.1,
              y = 1,
              xanchor = "left",
              text = paste0( client),
              font = list(color = '#264E86', size = 20, family = "Times New Roman"),
              ax = 0,
              ay = 0,
              align = "left",
              showarrow = FALSE)
    
    
    # fig <- fig %>% layout(annotations = a)
    
    fig <- fig %>% layout(annotations = b)
    
    fig
    
    
    
  })
  
  output$Start_table <- renderTable({
    
    df2<-df1(input$x,input$y) 
    
    f1<-which(df2$START_TIME ==  df2$START_TIME[1])
    
    df3<-df2[f1,]
    
    MATERIAL<-unique(df3$MAT_ID)
    
    RES_NUM<-0
    RES_FIND<-0
    RES_ROW3<-0
    
    # Match Unique MAT_ID, FIND RES_NUM
    for(i in 1:length(MATERIAL)){
      RES_NUM[i] <- which(df3$MAT_ID == MATERIAL[i])
    }
    
    
    RES_ROW<-unique(RES_NUM)
    
    for (i in 1:length(RES_ROW)) {
      
      RES_ROW2<-which(RES_NUM==RES_ROW[i])
      
      RES_ROW3<-seq(1:length(RES_ROW2))
      
      for(i in 1:length(RES_ROW3)){
        RES_NUM[RES_ROW2[i]] <- RES_ROW3[i]
      }
    }
    
    
    df3$RES_NUM<-RES_NUM
    
    RES_COUNT<-0
    
    RES<-subset(df3$RES_ID, df3$MAT_ID==MATERIAL[1])
    RES<-data.frame(RES,stringsAsFactors = F)
    
    
    for (i in 1:length(MATERIAL)){
      RES_COUNT[i]<-length(which(df3$MAT_ID == MATERIAL[i]))
    }
    
    MAX_COUNT <- max(RES_COUNT)
    
    
    #RES_SEQ <- subset(df3$RES_ID, df3$MAT_ID==MATERIAL[1])
    #if (MAX_COUNT>length(RES_COUNT[1])){
    #for (j in length(RES_COUNT[1])+1:MAX_COUNT){
    #RES_SEQ[j] <- NA
    #}
    #} else {RES_SEQ}
    
    #RES <- rbind(RES,RES_SEQ)
    
    # Don't use length function... WHY???
    
    RES_SEQ <- subset(df3$RES_ID, df3$MAT_ID==MATERIAL[1])
    if (MAX_COUNT>length(RES_COUNT[1])){
      for (j in (RES_COUNT[1]+1):MAX_COUNT){
        RES[j] <- NA
      }
    } else {RES}
    
    for (i in 2:length(RES_COUNT)){
      RES_SEQ <- subset(df3$RES_ID, df3$MAT_ID==MATERIAL[i])
      if (MAX_COUNT>length(RES_COUNT[i])){
        for (j in (RES_COUNT[i]+1):MAX_COUNT){
          RES_SEQ[j] <- NA
        }
      } else {RES_SEQ}
      RES <- rbind(RES,RES_SEQ)
    }
    
    RES<-cbind(MATERIAL,RES_COUNT,RES)
    

    
  })
  
  output$End_table <- renderTable({
    
    df2<-df1(input$x,input$y) 
    
    f1<-which(df2$START_TIME ==  df2$START_TIME[length(df2$START_TIME)])
    
    df3<-df2[f1,]
    
    MATERIAL<-unique(df3$MAT_ID)
    
    RES_NUM<-0
    RES_FIND<-0
    RES_ROW3<-0
    
    # Match Unique MAT_ID, FIND RES_NUM
    for(i in 1:length(MATERIAL)){
      RES_NUM[i] <- which(df3$MAT_ID == MATERIAL[i])
    }
    
    
    RES_ROW<-unique(RES_NUM)
    
    for (i in 1:length(RES_ROW)) {
      
      RES_ROW2<-which(RES_NUM==RES_ROW[i])
      
      RES_ROW3<-seq(1:length(RES_ROW2))
      
      for(i in 1:length(RES_ROW3)){
        RES_NUM[RES_ROW2[i]] <- RES_ROW3[i]
      }
    }
    
    
    df3$RES_NUM<-RES_NUM
    
    RES_COUNT<-0
    
    RES<-subset(df3$RES_ID, df3$MAT_ID==MATERIAL[1])
    RES<-data.frame(RES,stringsAsFactors = F)
    
    
    for (i in 1:length(MATERIAL)){
      RES_COUNT[i]<-length(which(df3$MAT_ID == MATERIAL[i]))
    }
    
    MAX_COUNT <- max(RES_COUNT)
    
    
    #RES_SEQ <- subset(df3$RES_ID, df3$MAT_ID==MATERIAL[1])
    #if (MAX_COUNT>length(RES_COUNT[1])){
    #for (j in length(RES_COUNT[1])+1:MAX_COUNT){
    #RES_SEQ[j] <- NA
    #}
    #} else {RES_SEQ}
    
    #RES <- rbind(RES,RES_SEQ)
    
    # Don't use length function... WHY???
    
    RES_SEQ <- subset(df3$RES_ID, df3$MAT_ID==MATERIAL[1])
    if (MAX_COUNT>length(RES_COUNT[1])){
      for (j in (RES_COUNT[1]+1):MAX_COUNT){
        RES[j] <- NA
      }
    } else {RES}
    
    for (i in 2:length(RES_COUNT)){
      RES_SEQ <- subset(df3$RES_ID, df3$MAT_ID==MATERIAL[i])
      if (MAX_COUNT>length(RES_COUNT[i])){
        for (j in (RES_COUNT[i]+1):MAX_COUNT){
          RES_SEQ[j] <- NA
        }
      } else {RES_SEQ}
      RES <- rbind(RES,RES_SEQ)
    }
    
    RES<-cbind(MATERIAL,RES_COUNT,RES)
    
    
    
  })
  
}



shinyApp(ui,server)
