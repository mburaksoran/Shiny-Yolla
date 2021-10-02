library(shinydashboard)
library(shiny)
library(shinyauthr)
library(dplyr)
library(lubridate)
library(plotly)
library(tidyverse)
library(mongolite)
library(jsonlite)

connection_string = 'mongodb+srv://Agulhas:Agulhas@cluster0.xa1nz.mongodb.net/myFirstDatabase?retryWrites=true&w=majority'
collection = mongo(collection="Users", db="Yolla", url=connection_string)
user_base<-collection$find('{}')

ui <- dashboardPage(
    dashboardHeader(title = "YOLLA"),
    dashboardSidebar(fluidRow(column(12,dateInput("date", label = h3("Tarih"), value = Sys.time(),)
      )
     )
    ),
    dashboardBody(shinyauthr::loginUI(
        "login",
    ),
        fluidRow(
            infoBoxOutput("Toplamkazanc"),
            infoBoxOutput("AylikKazanc"),
            infoBoxOutput("GunlukKazanc")
        ),
        fluidRow(
            column(4,plotlyOutput("Encoksatan5urun")),
            column(4,plotlyOutput("Yillikciro")),
            column(4,plotlyOutput("Aylikciro"))
            #splitLayout(cellWidths = c("33%", "33%", "33%"),height=200, plotlyOutput("Encoksatan5?r?n"), plotlyOutput("Yillikciro"),plotlyOutput("Aylikciro")),
            
        ),
        fluidRow(
            column(4,plotlyOutput("Toplamiptal")),
            column(4,plotlyOutput("Odemebicimi")),
            column(4,plotlyOutput("Garsonlaraylik"))
       ),

    )
)


 
server <- function(input, output) {
    # call login module supplying data frame, user and password cols and reactive trigger
    credentials <- shinyauthr::loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        sessionid_col = sessionid,
        log_out = reactive(logout_init())
    )
    
    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
    )
    
    observe({
        if (credentials()$user_auth) {
            shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
        } else {
            shinyjs::addClass(selector = "body", class = "sidebar-collapse")
        }
    })
    
    user_info <- reactive({
        credentials()$info
    })
    
    user_data <- reactive({
        req(credentials()$user_auth)
    
    })
    reader_order <- reactiveFileReader(intervalMillis = 10000000, session = NULL,filePath = 
                                     "./Data/Orders/Orders_Raw.csv", readFunc = read.csv,header = T,fill=T,sep = ",",fileEncoding = "UTF8")

    filtered_reader_df <- reactive({
        df <- reader_order() %>% filter(restName ==user_info()$name)
        df <- df[-1,]
        df$day <- weekdays(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$week<-week(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$daynummonth<-day(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$mnth<-month(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$year<-year(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df
    })
    reader_corder <- reactiveFileReader(intervalMillis = 1000, session = NULL,filePath = "./Data/Corders/Corders_Raw.csv", readFunc = read.csv,header = T,fill=T,sep = ",",fileEncoding = "UTF8")
                           
    
    filtered_readerc_df <- reactive({
        df <- reader_corder() %>% filter(restName ==user_info()$name)
        df <- df[-1,]
        df$day <- weekdays(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$week<-week(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$daynummonth<-day(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$mnth<-month(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df$year<-year(as.POSIXct(df$orderTime,tz="Europe/Istanbul",format="%Y-%m-%d %H:%M:%OS"))
        df
    })
    
    
    rv <- reactiveValues()
    
    observe({
        
        rv$time <- input$date
        
    })

    output$Toplamkazanc <- renderInfoBox({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        toplamb<-sum(ts$itemVal)

        infoBox(
            "Toplam Kazanç",paste0(toplamb," tl"), icon = icon("credit-card"),
            color = "blue",fill = TRUE
        )
    })

    output$AylikKazanc <- renderInfoBox({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        n<-month(rv$time)
        ts<-ts %>% filter(mnth == n)
        toplam<-sum(ts$itemVal)

        infoBox(
            "Aylık Kazanç",paste0(toplam," tl"), icon = icon("list"),
            color = "purple", fill = TRUE
        )
     })

    output$GunlukKazanc <- renderInfoBox({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        n<-month(rv$time)
        ts<-ts %>% filter(mnth == n)
        days<-day(rv$time)
        ts<-ts %>% filter(daynummonth == days)
        toplam<-sum(ts$itemVal)

        infoBox(
            "Günlük Kazanç",paste0(toplam," tl"), icon = icon("list"),
            color = "yellow", fill = TRUE
        )
    })

    output$Encoksatan5urun <- renderPlotly({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        n<-data.frame(summary(as.factor(ts$item)))
        gg<-levels(as.factor(ts$item))
        ggg<-data.frame("itemCount"=n[,1],"item"=gg)
        ggg<-ggg[order(ggg$itemCount, decreasing = TRUE),]
        dfp<-ts %>% filter(item %in% as.vector(ggg[1:5,2]))
        monthlist<-list()
        for (i in 1:5) {
            wd<- dfp%>% filter(item == as.vector(ggg[i,2]) )
            f<-wd %>% group_by(month=floor_date(as.Date(orderTime), "month")) %>% summarize(amount=sum(itemCount))
            colnames(f)<-c("Date",as.character(as.vector(ggg[i,2])))
            monthlist[[i]]<-f
        }
        df<-monthlist[[1]]
        for (i in 2:5) {df[,i+1]<-monthlist[[i]][,2]}
        fig <- plot_ly(df, x = ~Date, y = ~df[[colnames(df)[2]]], type = 'bar', name = colnames(df)[2], marker = list(color = 'rgb(204,204,255)'))
        fig <- fig %>% add_trace(y = ~df[[colnames(df)[3]]], name = colnames(df)[3], marker = list(color = 'rgb(255,191,0)'))
        fig <- fig %>% add_trace(y = ~df[[colnames(df)[4]]], name = colnames(df)[4], marker = list(color = 'rgb(255,127,80)'))
        fig <- fig %>% add_trace(y = ~df[[colnames(df)[5]]], name = colnames(df)[5], marker = list(color = 'rgb(222,49,99)'))
        fig <- fig %>% add_trace(y = ~df[[colnames(df)[6]]], name = colnames(df)[6], marker = list(color = 'rgb(100,149,237)'))
        fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45,type="date",tickformat="%Y"),
                              yaxis = list(title = "Satış Adedi"),

                              barmode = 'group')%>% config(locale = "tr")%>% layout(title="En Çok Satan 5 Ürün")
        plotly::ggplotly(fig)
    })

    output$Yillikciro <- renderPlotly({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        ts<- ts%>% filter(year==year(rv$time))
        month_df <- ts %>%
            group_by(month=floor_date(as.Date(orderTime), "month")) %>%
            summarise(value = sum(itemVal)) %>%
            ungroup()
        fig <- plot_ly(data = month_df,x = ~month, y = ~value, mode = 'lines', text = paste("tm", "days from today"))%>% config(locale = "tr")%>% layout(title="Yıllık Ciro",xaxis = list(title = 'Tarih',type="date",tickformat="%m-%Y"), 
                                                                                                                                                         yaxis = list(title = 'Toplam Satış Tutarı(TL)'))

        plotly::ggplotly(fig)
    })

    output$Aylikciro <- renderPlotly({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        ts<- ts%>% filter(mnth==month(rv$time))
        day_df <- ts %>%
            group_by(month=floor_date(as.Date(orderTime), "day")) %>%
            summarise(value = sum(itemVal)) %>%
            ungroup()
        fig <- plot_ly(data = day_df,x = ~month, y = ~value, mode = 'lines', text = paste("tm", "days from today"))%>% config(locale = "tr")%>% layout(title="Aylık Ciro",xaxis = list(title = 'Tarih',type="date",tickformat="%d-%m"), 
                                                                                                                                                       yaxis = list(title = 'Satış Tutarı (TL)'))
        plotly::ggplotly(fig)
    })


    output$Toplamiptal <- renderPlotly({
        req(credentials()$user_auth)
        df_corders<-filtered_readerc_df()
        df_orders<-filtered_reader_df()
        corders_length<-length(df_corders$itemVal)
        ts<-df_orders %>% filter(isPaid == "True")
        paid<-length(ts$isPaid)
        pie_df<-data.frame(durum=c("Ödenen","İptal"),deger=c(paid,corders_length))
        fig <- plot_ly(pie_df, labels = ~durum, values = ~deger, type = 'pie')%>% config(locale = "tr")%>% layout(title="Tamamlanan / İptal Sipariş Oranı")

        plotly::ggplotly(fig)
    })

    output$Odemebicimi <- renderPlotly({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        n<-data.frame(summary(as.factor(ts$paidBy)))
        gg<-levels(as.factor(ts$paidBy))
        gg<-gg[-1]
        aylik_df<-data.frame("itemCount"=n[2:3,1],"item"=gg)
        fig <- plot_ly(aylik_df, labels = ~item, values = ~itemCount, type = 'pie')%>% config(locale = "tr")%>%layout(title="Ödeme Biçimi")
        plotly::ggplotly(fig)
    })

    output$Garsonlaraylik <- renderPlotly({
        req(credentials()$user_auth)
        df_orders<-filtered_reader_df()
        ts<-df_orders %>% filter(isPaid == "True")
        n<-data.frame(summary(as.factor(ts$table)))
        gg<-levels(as.factor(ts$table))
        garson_df<-data.frame("işlem"=n[,1],"Garson"=gg)
        garson_df[garson_df == 0]<-NA
        garson_df<-drop_na(garson_df)
        garson_df<-garson_df %>% filter(işlem > mean(garson_df$işlem))
        fig <- plot_ly(garson_df, labels = ~Garson, values = ~işlem,textinfo="label",hoverinfo="value") %>% config(locale = "tr")
        fig <- fig %>% add_pie(hole = 0.6)
        fig <- fig %>% layout(title = "En Çok Tercih Edilen Masalar",  showlegend = F,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        plotly::ggplotly(fig)
    })

}

shinyApp(ui, server)
