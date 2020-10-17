# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("DT")
# install.packages("shinyjs")
# install.packages("V8")

# install.packages("ggplot2")
# install.packages("RColorBrewer")
# install.packages("ggcorrplot")
# install.packages("gridExtra")

# install.packages("randomForest")
# install.packages("rpart")
# install.packages("rpart.plot")

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(V8)

library(ggplot2)
library(RColorBrewer)
library(ggcorrplot)
library(gridExtra)

library(randomForest)
library(rpart)
library(rpart.plot)

###############################################################################

setwd("C:\\Users\\admin\\Desktop\\korea_gg_real_estate_Anomaly_Detection\\")

train <-readRDS(file="train.rds")
test <-readRDS(file="test.RDS")
all <-readRDS(file="all.RDS")

md_all <- all[,c(1,2,5,6,9,12)]
md_train <- train[,c(1,2,5,6,9,12)]
md_test <- test[,c(1,2,5,6,9,12)]

md_train$aptType <- as.factor(md_train$aptType)
md_train$si <- as.factor(md_train$si)
md_test$aptType <- as.factor(md_test$aptType)
md_test$si <- as.factor(md_test$si)

true <- md_test$price
true2 <- md_test$deposit

###############################################################################

jscode <- "shinyjs.refresh = function() { history.go(0); }"

###############################################################################

# 타이틀
header <- dashboardHeader(title = "Final Test")
  
# 사이드 바
sidebar <- dashboardSidebar(
  
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  
  sidebarMenu(
    
    selectInput(inputId = "dataset",
                label = "Choose a dataset:",
                choices = c("all", "train", "test")),
    
    useShinyjs(),
    extendShinyjs(text = jscode),
    actionButton("refresh", "화면 새로고침"),
    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
    menuItem("Dataset", tabName = "dataset", icon = icon("database")),
    menuItem("EDA1", tabName = "EDA1", icon = icon("chart-pie")),
    menuItem("EDA2", tabName = "EDA2", icon = icon("chart-pie")),
    menuItem("Decision Tree", tabName="tree", icon = icon("tree")),
    menuItem("ML : RandomForest", tabName = "randomforest", icon = icon("laptop"))
    # https://fontawesome.com/icons?d=gallery
  )
)
  
# 바디
body <- dashboardBody(
  
  # 메뉴 1 : 완료
  tabItems(
    tabItem(
      tabName = "dataset",
      fluidRow(
          DT::dataTableOutput("table")
      )
    )
  ),
  
  
  # 메뉴 2 
  tabItems(
    tabItem(
      tabName = "EDA1",
      fluidRow(
          h4("전체분포"),
          plotOutput("plot", width = "600px", height="500px"), hr(),
          h4("월세분포"),
          plotOutput("plot2", width = "600px", height="500px"), hr(),
          h4("전세분포"),
          plotOutput("plot3", width = "600px"), hr(),
          h4("시계열"),
          plotOutput("plot4", width = "1000px", height = "250px"), hr()
      )
    )
  ),
  
  # 메뉴 3
  tabItems(
    tabItem(
      tabName = "EDA2",
      fluidRow(
        h4("월세 관련 분포 및 상관관계"),
        plotOutput("plot5", width = "1000px"),
        plotOutput("plot6"),
        
        h4("전세 관련 분포 및 상관관계"),
        plotOutput("plot7", width = "1000px"),
        plotOutput("plot8")        
      )
    )
  ),
  
  # 메뉴 4
  tabItems(
    tabItem(
      tabName = "randomforest",
      fluidRow(
          h2("랜덤포레스트 회귀 모델 : 월세가격예측"),
          h3(textOutput("caption")),
          hr(),
          h4("특성 중요도"),
          plotOutput("feature", width="350px", height="350px"),
          h4("월세가격 이상탐지"),
          plotOutput("detect1_plot", width="350px", height="350px"),
          hr(),
          h4("실제값과 예측값"),
          DT::dataTableOutput("pred", width ="350px"),

          hr(), hr(),
          
          h2("랜덤포레스트 회귀 모델 : 월세보증금 및 전세가격예측"),
          h3(textOutput("caption2")),
          hr(),
          h4("특성 중요도"),
          plotOutput("feature2", width="350px", height="350px"),
          h4("월세 보증금 및 전세가격 이상탐지"),
          plotOutput("detect2_plot", width="350px", height="350px"),
          hr(),
          h4("실제값과 예측값"),
          DT::dataTableOutput("pred2", width ="350px")
          
      )
    )
  ),
  
  # 메뉴 5
  tabItems(
    tabItem(
      tabName = "tree",
      fluidRow(
          h4("월세가격"),
          plotOutput("tree",  width = "800px", height="300px"),
          h4("월세보증금 및 전세가격"),
          plotOutput("tree2", width = "800px", height="300px")
      )
    )
  )
  
  
)

ui <- dashboardPage(header, sidebar, body)

###############################################################################


server <- function(input, output) {
  
  ######################
  
  # 페이지 새로고침
  observeEvent(input$refresh, {
    js$refresh();
  })
  
  ######################
  
  #  reactive : 데이터셋 함수
  dataInput <- reactive({
    switch(input$dataset,
           "all" = all, 
           "train" = train, 
           "test" = test)
  })
  
  set.seed(42)
  
  ######################
  
  # 로우 데이터 랜더링
  output$table <- DT::renderDataTable({
    DT::datatable(dataInput(), options=list(pageLength=25))
  })
  
  ######################
  
  # EDA 및 시각화
  
  # 전체 분포
  output$plot <- renderPlot({
    
    par(mfcol=c(4,2))
    
    hist(dataInput()$deposit, main = "보증금의 분포", xlab="보증금(만원)");
    boxplot(dataInput()$deposit, horizontal=TRUE);
    
    hist(dataInput()$price , main = "월·전세가격의 분포", xlab="월·전세가격(만원)");
    boxplot(dataInput()$price, horizontal=TRUE);
    
    hist(dataInput()$area, main = "면적의 분포", xlab="면적(m²)");
    boxplot(dataInput()$area, horizontal=TRUE);
    
    hist(dataInput()$floor, main = "층의 분포", xlab="층");
    boxplot(dataInput()$floor, horizontal=TRUE);
    
  })
    
  
  
  # 월세 분포    
  output$plot2 <- renderPlot({
      
    df1 <- subset(dataInput(), priceType=='월세')
      
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df1$yyyymm, 1, 4), "-"), substr(df1$yyyymm, 5, 6)), '-'),df1$day))
    df1$yyyymmdd <- as.Date(tmp)
    
    par(mfcol=c(4,2))

    hist(df1$deposit, main = "보증금의 분포", xlab="보증금(만원)");
    boxplot(df1$deposit, horizontal=TRUE);
    
    hist(df1$price, main = "월세가격의 분포", xlab="월세가격(만원)");
    boxplot(df1$price, horizontal=TRUE);

    hist(df1$area, main = "면적의 분포", xlab="면적(m²)");
    boxplot(df1$area, horizontal=TRUE);

    hist(df1$floor, main = "층의 분포", xlab="층");
    boxplot(df1$floor, horizontal=TRUE)
    
  })
    
  
  
  
  # 전세분포
  output$plot3 <- renderPlot({
      
    df2 <- subset(dataInput(), priceType=='전세')
      
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df2$yyyymm, 1, 4), "-"), substr(df2$yyyymm, 5, 6)), '-'),df2$day))
    df2$yyyymmdd <- as.Date(tmp)
      
    par(mfrow=c(3,2))    
    
    hist(df2$area, main = "면적의 분포", xlab="면적(m²)");
    boxplot(df2$area, horizontal=TRUE);

    hist(df2$floor, main = "층의 분포", xlab="층");
    boxplot(df2$floor, horizontal=TRUE);

    hist(df2$deposit, main = "전세가격의 분포", xlab="전세가격(만원)");
    boxplot(df2$deposit, horizontal=TRUE);
    
  })
    
    
    
  # 시계열
  output$plot4 <- renderPlot({
    df1 <- subset(dataInput(), priceType=='월세')
    df2 <- subset(dataInput(), priceType=='전세')
      
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df1$yyyymm, 1, 4), "-"), substr(df1$yyyymm, 5, 6)), '-'),df1$day))
    df1$yyyymmdd <- as.Date(tmp)
      
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df2$yyyymm, 1, 4), "-"), substr(df2$yyyymm, 5, 6)), '-'),df2$day))
    df2$yyyymmdd <- as.Date(tmp)
      
    sub_s1 <- aggregate(df2$deposit,
                          by = list(YYYYMMDD = df2$yyyymmdd),
                          FUN = median)
    g1 <- ggplot(sub_s1, aes(x=YYYYMMDD, y=x)) + geom_line() + geom_smooth() +
      labs(title = "시계열 : 2016~2019년간 경기도의 전세 가격", x="월", y="가격(만 원)")
      
      
    sub_s2 <- aggregate(df1$price,
                          by = list(YYYYMMDD = df1$yyyymmdd),
                          FUN = median, na.rm=TRUE)
    g2 <- ggplot(sub_s2, aes(x=YYYYMMDD, y=x)) + geom_line() + geom_smooth() +
      labs(title = "시계열 : 2016~2019년간 경기도의 월세 가격", x="월", y="가격(만 원)")
      
      
    sub_s3 <- aggregate(df1$deposit,
                          by = list(YYYYMMDD = df1$yyyymmdd),
                          FUN = median, na.rm=TRUE)
    g3 <- ggplot(sub_s3, aes(x=YYYYMMDD, y=x)) + geom_line() + geom_smooth() + 
      labs(title = "시계열 : 2016~2019년간 경기도의 월세 보증금 가격", x="월", y="가격(만 원)")
      
    grid.arrange(g1, g2, g3, ncol=3)
      
  })
  
  # 월세 박스플롯
  output$plot5 <- renderPlot({
    
    df1 <- subset(dataInput(), priceType=='월세')

    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df1$yyyymm, 1, 4), "-"), substr(df1$yyyymm, 5, 6)), '-'),df1$day))
    df1$yyyymmdd <- as.Date(tmp)

    sub2 <- aggregate(df1$price,
                      by = list(YYYY = as.character(df1$yyyy),
                                Si = as.character(df1$si),
                                AptType = as.character(df1$aptType)),
                      FUN = median, na.rm=TRUE)
    
    g1 <- ggplot(sub2, aes(YYYY, x)) + geom_boxplot(aes(fill=YYYY)) + scale_fill_brewer(palette="Dark2") +
      labs(title = "Boxplot : 연도에 따른 월세 수준", x="연도", y="금액(만 단위)")
    g2 <- ggplot(sub2, aes(AptType, x)) + geom_boxplot(aes(fill=AptType)) +
      labs(title = "Boxplot : 아파트유형에 따른 월세 수준", x="아파트유형", y="금액(만 단위)")
    
    df1_naomit <- na.omit(df1)
    corr <- round(cor(subset(df1_naomit, select=c(deposit, price, area, floor))), 3)
    
    g3 <- ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE, lab_size=3,
                     method="circle", color=c("tomato2", "white", "springgreen3"), title="Correlation : 월세 데이터의 상관관계")
    
    grid.arrange(g1, g2, g3, ncol=3)
  })  
  
  
  output$plot6 <- renderPlot({  
    df1 <- subset(dataInput(), priceType=='월세')
    
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df1$yyyymm, 1, 4), "-"), substr(df1$yyyymm, 5, 6)), '-'),df1$day))
    df1$yyyymmdd <- as.Date(tmp)
    
    sub2 <- aggregate(df1$price,
                      by = list(YYYY = as.character(df1$yyyy),
                                Si = as.character(df1$si),
                                AptType = as.character(df1$aptType)),
                      FUN = median, na.rm=TRUE)
    
    ggplot(sub2, aes(Si, x)) + geom_boxplot(aes(fill=Si)) +
      labs(title = "Boxplot : 도시에 따른 월세 수준", x="도시", y="금액(만 단위)") +
      theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1))    
  })
  
  
  
  #전세 박스플롯
  
  output$plot7 <- renderPlot({
    df2 <- subset(dataInput(), priceType=='전세')
    
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df2$yyyymm, 1, 4), "-"), substr(df2$yyyymm, 5, 6)), '-'),df2$day))
    df2$yyyymmdd <- as.Date(tmp)
    
    sub3 <- aggregate(df2$deposit,
                      by = list(YYYY = as.character(df2$yyyy),
                                Si = as.character(df2$si),
                                AptType = as.character(df2$aptType)),
                      FUN = median, na.rm=TRUE)
    
    g1 <- ggplot(sub3, aes(YYYY, x)) + geom_boxplot(aes(fill=YYYY)) + scale_fill_brewer(palette="Dark2") +
      labs(title = "Boxplot : 연도에 따른 전세 수준", x="연도", y="금액(만 단위)")
    g2 <- ggplot(sub3, aes(AptType, x)) + geom_boxplot(aes(fill=AptType)) +
      labs(title = "Boxplot : 아파트유형에 따른 전세 수준", x="도시", y="금액(만 단위)")
    
    df2_naomit <- na.omit(df2)
    corr <- round(cor(subset(df2_naomit, select=c(deposit, area, floor))), 3)
    
    g3 <- ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE, lab_size=3,
               method="circle", color=c("tomato2", "white", "springgreen3"), title="Correlation : 전세 데이터의 상관관계")
    
    
    grid.arrange(g1, g2, g3, ncol=3)
  })  
  
  
  output$plot8 <- renderPlot({  
    df2 <- subset(dataInput(), priceType=='전세')
    
    tmp <- gsub(' ','',paste(paste(paste(paste(substr(df2$yyyymm, 1, 4), "-"), substr(df2$yyyymm, 5, 6)), '-'),df2$day))
    df2$yyyymmdd <- as.Date(tmp)
    
    sub3 <- aggregate(df2$deposit,
                      by = list(YYYY = as.character(df2$yyyy),
                                Si = as.character(df2$si),
                                AptType = as.character(df2$aptType)),
                      FUN = median, na.rm=TRUE)
    
    ggplot(sub3, aes(Si, x)) + geom_boxplot(aes(fill=Si)) +
      labs(title = "Boxplot : 도시에 따른 전세 수준", x="도시", y="금액(만 단위)") +
      theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1))
  
  })
  
  ######################
  
  
  #  reactive : 랜덤포레스트 모델 함수 : 월세
  rfModel <- reactive({
    randomForest(price~., data=md_train, ntree=100, mtry=4, na.action=na.omit, importancve=TRUE )
  })
  
  #  reactive : 랜덤포레스트 예측값 함수 : 월세
  rfPredict <- reactive({
    fit = rfModel()
    round(predict(fit, newdata = md_test)) # 가격예측이라서 반올림
  })
  
  # 디텍션 분석 1 ##########
  Detect1 <- reactive({
    y_pred = rfPredict()
    true = true
    df = as.data.frame(cbind(true, y_pred))
    e_lm <- lm(true~y_pred, data=df)
    p <- predict(e_lm, interval="prediction", level=0.95)
    p
  })
  
  ######################
  
  #  reactive : 랜덤포레스트 모델 함수 2 : 전세 및 월세보증금
  rfModel2 <- reactive({
    randomForest(deposit~., data=md_train, ntree=100, mtry=4, na.action=na.omit, importancve=TRUE)
  })
  
  #  reactive : 랜덤포레스트 예측값 함수 2 : 전세 및 월세보증금
  rfPredict2 <- reactive({
    fit = rfModel2()
    round(predict(fit, newdata = md_test)) # 가격예측이라서 반올림
  })
  
  # 디텍션 분석 2 ##########
  Detect2 <- reactive({
    y_pred2 = rfPredict2()
    df2 = as.data.frame(cbind(true2, y_pred2))
    e_lm2 <- lm(true2~y_pred2, data=df2)
    p2 <- predict(e_lm2, interval="prediction", level=0.95)
    p2
  })
  
  ######################
  
  # 트리 표시
  output$tree <- renderPlot({
    decision_tree <- rpart(price ~ ., data = md_train, method="anova")
    ptree<-prune(decision_tree, cp=0.028)
    rpart.plot(ptree, cex=1)
  })
  
  # 트리 표시 2
  output$tree2 <- renderPlot({
    decision_tree <- rpart(deposit ~ ., data = md_train, method="anova")
    ptree<-prune(decision_tree, cp=0.1)
    rpart.plot(ptree, cex=1)
  })
  
  ######################
  
  # 예측값을 표시
  output$pred <- DT::renderDataTable({
    d <- as.data.frame(cbind(true, Detect1()))
    up <- d$true > d[,2]
    down <- d$true < d[,3]
    df_pred <- cbind(rfPredict(), d$true, up, down)
    colnames(df_pred) <- c("true", "pred", "상단이상", "하단이상")
    DT::datatable(df_pred, options=list(pageLength=25))
  })

  # MSE, RMSE 표시
  output$caption <- renderText({
    (mse = mean((md_test$price - rfPredict())^2))
    (rmse = sqrt(mse))
    paste("MSE :", round(mse, 5), " / ", "RMSE :", round(rmse, 5))
  })
  
  # 특성중요도 표시
  output$feature <- renderPlot({
    tmp <- round(varImpPlot(rfModel()))
    tmp[order(tmp),]
  })
  
  ######################
  
  # 예측값을 표시 2
  output$pred2 <- DT::renderDataTable({
    d2 <- as.data.frame(cbind(true, Detect2()))
    up <- d2$true > d2[,2]
    down <- d2$true < d2[,3]
    df_pred <- cbind(rfPredict2(), d2$true, up, down)
    colnames(df_pred) <- c("true", "pred", "상단이상", "하단이상")
    DT::datatable(df_pred, options=list(pageLength=25))
  })
  
  # MSE, RMSE 표시 2
  output$caption2 <- renderText({
    (mse = mean((md_test$deposit - rfPredict2())^2))
    (rmse = sqrt(mse))
    paste("MSE :", round(mse, 5), " / ", "RMSE :", round(rmse, 5))
  })
  
  # 특성중요도 표시 2
  output$feature2 <- renderPlot({
    tmp <- round(varImpPlot(rfModel2()))
    tmp[order(tmp),]
  })
  
  ######################
  

  # 디텍션 1 플롯
  output$detect1_plot <- renderPlot({
    y_pred = rfPredict()
    d <- as.data.frame(cbind(true, Detect1()))
    plot(y_pred, true, main="Anomaly Detection Plot 1")
    lines(y_pred, d[,2],lwd=2, col='blue')
    lines(y_pred, d[,3],lwd=2, col='red')
    lines(y_pred, d[,4],lwd=2, col='red')
  })
  

  # 디텍션 2 플롯
  output$detect2_plot <- renderPlot({
    y_pred2 = rfPredict2()
    d2 <- as.data.frame(cbind(true2, Detect2()))
    plot(y_pred2, true2, main="Anomaly Detection Plot 2")
    lines(y_pred2, d2[,2], lwd=2, col='blue')
    lines(y_pred2, d2[,3], lwd=2, col='red')
    lines(y_pred2, d2[,4], lwd=2, col='red')
  })
  
  
}
###############################################################################

shinyApp(ui, server)

