library(shiny)
library(shinyWidgets)
library(ggplot2)
ggplot2::theme(font="auto")
library(rvest)
library(stringr)
library(tidyverse)
library(modelsummary)
library(fBasics)
library(questionr)
library(ggpubr)
library(bslib)
library(shinythemes)
library(thematic)

anycomp <- function(data){
    comp <- read_html(data)
    rank <- comp %>% html_nodes(xpath = '//tr/td[@class = "rank"]') %>% html_text %>% as.numeric
    name <- comp %>% html_nodes(xpath = '//tr/td[@class = "name"]') %>% html_text %>% str_remove_all("Falls:") %>% str_remove_all("Time violation:")
    name <- name[name != ""]
    TSS <- comp %>% html_nodes(xpath = '//tr/td[@class = "totSeg"]') %>% html_text %>% as.numeric
    TES <- comp %>% html_nodes(xpath = '//tr/td[@class = "totElm"]') %>% html_text %>% as.numeric
    bv <- comp %>% html_nodes(xpath = '//tr/td[@class = "tbvv"]') %>% html_text %>% as.numeric
    TCS <- comp %>% html_nodes(xpath = '//tr/td[@class = "totComp"]') %>% html_text %>% as.numeric
    Ded <- comp %>% html_nodes(xpath = '//tr/td[@class = "totDed"]') %>% html_text
    Dat <- data.frame(rank, name, TSS, TES, bv, TCS, Ded)
    Dat$GOE <- Dat$TES - Dat$bv
    Dat <- Dat %>% separate(name, c("Name", "Club"), sep=",")
    return(Dat)
}
Comp1 <- anycomp('https://ijs.usfigureskating.org/leaderboard/results/2017/24267/SEGM038.html')
#2017 US Nationals Mens Short Program
Comp2 <- anycomp('https://ijs.usfigureskating.org/leaderboard/results/2017/24267/SEGM039.html')
#2017 US Nationals Mens Freeskate
Comp3 <- anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM035.html')
#2018 US Nationals Mens Short Program
Comp4 <- anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#2018 US Nationals Mens Freeskate
Comp5 <- anycomp('https://ijs.usfigureskating.org/leaderboard/results/2019/27958/SEGM038.html')
#2019 US Nationals Mens Short Program
Comp6 <- anycomp('https://ijs.usfigureskating.org/leaderboard/results/2019/27958/SEGM040.html')
#2019 US Nationals Mens Freeskate

df <- list(Comp1, Comp2)
var <- c("rank", "name", "club", "TSS", "TES", "bv", "TCS", "Ded")

compsum1 <- function(dat){
    tbl <- datasummary(dat, (TSS+TES+bv+TCS+GOE)~(mean+sd))
    return(tbl)
}

my_theme <- bs_theme(bg="#092B36",fg="#93A1A1", primary = "#FCC780",
                     base_font = font_google("Space Mono"),
                     code_font = font_google("Space Mono"), font_scale = 0.7)

thematic::thematic_shiny()

# UI
ui <- fluidPage(
    theme=my_theme,
    h1("US Figure Skating Championships Event Comparison"),
    fluidRow(
        column(5,
               wellPanel(
                   h3("Select Event:"),
                   selectInput(inputId = "comp1",
                               label = "#1",
                               choices = c("", "2017 Mens Short Program" = "Comp1",
                                           "2017 Mens Freeskate" = "Comp2", "2018 Mens Short Program" = "Comp3",
                                           "2018 Mens Freeskate" = "Comp4", "2019 Mens Short Program" = "Comp5",
                                           "2019 Mens Freeskate" = "Comp6"), selected=NULL, multiple=FALSE),
                   selectInput(inputId = "comp2",
                               label = "#2",
                               choices = c("", "2017 Mens Short Program" = "Comp1",
                                           "2017 Mens Freeskate" = "Comp2", "2018 Mens Short Program" = "Comp3",
                                           "2018 Mens Freeskate" = "Comp4", "2019 Mens Short Program" = "Comp5",
                                           "2019 Mens Freeskate" = "Comp6"), selected=NULL, multiple=FALSE),
                   actionButton("gosuccess", "Select")
               ),
               #textOutput("DataSelect")
               fluidRow(column(12,
                               wellPanel(
                                   h3('Summary Visuals'),
                                   br(),
                                   splitLayout(
                                       cellArgs = list(style='white-space: normal;'),
                                       checkboxGroupInput(inputId="sum1", label="Event 1", choices=c("Data"="Dat1","Quantitative Variables"="QuanSum1", "Qualitative Variables"="QualSum1","Variable Variation for Quantitative Variables"="QuanVar1", "Variable Variation for Qualitative Variables"="QualVar1", "TSS by Skater"="TSSskater1", "BV by Skater"="BVskater1", "GOE by Skater"="GOEskater1"), inline = TRUE),
                                       checkboxGroupInput(inputId="sum2", label="Event 2", choices=c("Data"="Dat2", "Quantitative Variables"="QuanSum2", "Qualitative Variables"="QualSum2","Variable Variation for Quantitative Variables"="QuanVar2", "Variable Variation for Qualitative Variables"="QualVar2", "TSS by Skater"="TSSskater2", "BV by Skater"="BVskater2", "GOE by Skater"="GOEskater2"), inline = TRUE)
                                   )),
                               fluidRow(column(12,
                                               wellPanel(
                                                   h3('Comparison Visuals'),
                                                   #checkboxInput("TSSCompare", "Comparison of Total Segment Scores"),
                                                   checkboxGroupInput(inputId="compare", label="Event 1 & Event 2", choices=c("Skaters in Both Events"="Bothskater", "Total Segment Scores"="TSSCompare", "Base Value Scores"="bvCompare"))

                                               )))))),
        column(6, align="top",
               br(),
               br(),
               br(),
               br(),
               tableOutput("Skater1"),
               tableOutput("Skater2"),
               tableOutput("sumtable1_1"),
               tableOutput("sumtable1_2"),
               tableOutput("sumtable2_1"),
               tableOutput("sumtable2_2"),
               plotOutput("varplot1_1"),
               plotOutput("varplot1_2"),
               plotOutput("varplot2_1"),
               plotOutput("varplot2_2"),
               plotOutput("tssSkater1"),
               plotOutput("tssSkater2"),
               plotOutput("bvSkater1"),
               plotOutput("bvSkater2"),
               plotOutput("goeSkater1"),
               plotOutput("goeSkater2"),
               tableOutput("bothSkater"),
               plotOutput("tssplot"),
               plotOutput("bvplot")
        )),
)



# Define server function
server <- function(input, output, session) {
    #bs_themer()
    #thematic::thematic_shiny()
    dataset1Input <- eventReactive(input$gosuccess,{
        get(input$comp1)
    })
    dataset2Input <- eventReactive(input$gosuccess,{
        get(input$comp2)
    })
    #output$DataSelect <- renderText({
    # if(input$goButton1){
    # print('Data Selected')
    #}
    # })
    observeEvent(input$gosuccess, {
        sendSweetAlert(
            session=session,
            title="Success!",
            type="gosuccess",
            width=500, closeOnClickOutside = TRUE
        )
    })
    Sum1 <- reactive(input$sum1)
    Sum2 <- reactive(input$sum2)
    Compare <- reactive(input$compare)
    output$Skater1 <- renderTable({
        dat <- dataset1Input()
        if("Dat1" %in% Sum1()){
            dat
        }
    }, rownames = TRUE, caption = "Skaters who Participated in Event 1",
    caption.placement = getOption("xtable.caption.placement", "top")
    )
    output$Skater2 <- renderTable({
        dat <- dataset2Input()
        if("Dat2" %in% Sum2()){
            dat
        }
    }, rownames = TRUE, caption = "Skaters who Participated in Event 2",
    caption.placement = getOption("xtable.caption.placement", "top")
    )
    output$sumtable1_1 <- renderTable({
        dat <- dataset1Input()
        dat1 <- dat %>% select(TSS, TES, bv, TCS, GOE)
        if("QuanSum1" %in% Sum1()){
            t(basicStats(dat1)[c("Mean", "Stdev"),])
        }
    }, rownames = TRUE, caption = "Event 1: Summary Table for Quantitative Variables",
    caption.placement = getOption("xtable.caption.placement", "top")
    )
    output$sumtable1_2 <- renderTable({
        dat <- dataset1Input()
        if("QualSum1" %in% Sum1()){
            questionr::freq(dat$Ded)
        }
    }, rownames = TRUE, caption = "Event 1: Summary Table for Deductions",
    caption.placement = getOption("xtable.caption.placement", "top"))
    output$sumtable2_1 <- renderTable({
        dat <- dataset2Input()
        dat1 <- dat %>% select(TSS, TES, bv, TCS, GOE)
        if("QuanSum2" %in% Sum2()){
            t(basicStats(dat1)[c("Mean", "Stdev"),])
        }
    }, rownames = TRUE, caption = "Event 2: Summary Table for Quantitative Variables",
    caption.placement = getOption("xtable.caption.placement", "top"))
    output$sumtable2_2 <- renderTable({
        dat <- dataset2Input()
        if("QualSum2" %in% Sum2()){
            questionr::freq(dat$Ded)
        }
    }, rownames = TRUE, caption = "Event 2: Summary Table for Deductions",
    caption.placement = getOption("xtable.caption.placement", "top"))
    output$varplot1_1 <- renderPlot({
        dat <- dataset1Input()
        if("QuanVar1" %in% Sum1()){
            boxplot(dat$TSS, dat$TES, dat$bv, dat$TCS, dat$GOE, main="Event 1: Quantitative Variable Variation", names=c("TSS", "TES", "bv", "TCS", "GOE"), xlab="Score Type", ylab="Scores")
        }
    })
    output$varplot1_2 <- renderPlot({
        dat <- dataset1Input()
        if("QualVar1" %in% Sum1()){
            df <- dat %>% group_by(Ded) %>% summarise(counts=n())
            ggplot(df, aes(x=Ded, y=counts)) + geom_bar(fill="#0073C2FF", stat="identity") + geom_text(aes(label=counts), vjust=-0.3) + ggtitle('Event 1: Deduction Variation') + xlab("Deductions") + ylab("Count")
        }
    })
    output$varplot2_1 <- renderPlot({
        dat <- dataset2Input()
        if("QuanVar2" %in% Sum2()){
            boxplot(dat$TSS, dat$TES, dat$bv, dat$TCS, dat$GOE, main="Event 2: Quantitative Variable Variation", names=c("TSS", "TES", "bv", "TCS", "GOE"), xlab="Score Type", ylab="Scores")
        }
    })
    output$varplot2_2 <- renderPlot({
        dat <- dataset2Input()
        if("QualVar2" %in% Sum2()){
            df <- dat %>% group_by(Ded) %>% summarise(counts=n())
            ggplot(df, aes(x=Ded, y=counts)) + geom_bar(fill="#0073C2FF", stat="identity") + geom_text(aes(label=counts), vjust=-0.3) + ggtitle('Event 2: Deduction Variation') + xlab("Deductions") + ylab("Count")
        }
    })
    output$tssSkater1 <- renderPlot({
        dat <- dataset1Input()
        if("TSSskater1" %in% Sum1()){
            ggplot(dat, aes(x=reorder(Name, rank), y=TSS)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Event 1: Total Segment Scores by Skater in Rank Order') + ylab('Total Segment Scores') + xlab("Skater")
        }
    })
    output$tssSkater2 <- renderPlot({
        dat <- dataset2Input()
        if("TSSskater2" %in% Sum2()){
            ggplot(dat, aes(x=reorder(Name, rank), y=TSS)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Event 2: Total Segment Scores by Skater in Rank Order') + ylab('Total Segment Scores') + xlab("Skater")
        }
    })
    output$bvSkater1 <- renderPlot({
        dat <- dataset1Input()
        if("BVskater1" %in% Sum1()){
            ggplot(dat, aes(x=reorder(Name, rank), y=bv)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Event 1: Base Value Scores by Skater in Rank Order') + ylab('BV Scores') + xlab("Skater")
        }
    })
    output$bvSkater2 <- renderPlot({
        dat <- dataset2Input()
        if("BVskater2" %in% Sum2()){
            ggplot(dat, aes(x=reorder(Name, rank), y=bv)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Event 2: Base Value Scores by Skater in Rank Order') + ylab('BV Scores') + xlab("Skater")
        }
    })
    output$goeSkater1 <- renderPlot({
        dat <- dataset1Input()
        if("GOEskater1" %in% Sum1()){
            ggplot(dat, aes(x=reorder(Name, rank), y=GOE)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Event 1: GOE Scores by Skater in Rank Order') + ylab('GOE Scores') + xlab("Skater")
        }
    })
    output$goeSkater2 <- renderPlot({
        dat <- dataset2Input()
        if("GOEskater2" %in% Sum2()){
            ggplot(dat, aes(x=reorder(Name, rank), y=GOE)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Event 2: GOE Scores by Skater in Rank Order') + ylab('GOE Scores') + xlab("Skater")
        }
    })
    output$bothSkater <- renderTable({
        dat1 <- dataset1Input()
        dat2 <- dataset2Input()
        if("Bothskater" %in% Compare()){
            dat <- merge(dat1, dat2, by=c('Name','Club'))
            names(dat)[3] <- "Rank1"
            names(dat)[4] <- "TSS1"
            names(dat)[5] <- "TES1"
            names(dat)[6] <- "bv1"
            names(dat)[7] <- "TCS1"
            names(dat)[8] <- "Ded1"
            names(dat)[9] <- "GOE1"
            names(dat)[10] <- "Rank2"
            names(dat)[11] <- "TSS2"
            names(dat)[12] <- "TES2"
            names(dat)[13] <- "bv2"
            names(dat)[14] <- "TCS2"
            names(dat)[15] <- "Ded2"
            names(dat)[16] <- "GOE2"
            dat[, c(1,2,3,10,4,11,5,12,6,13,7,14,8,15,9,16)]
        }
    }, rownames = TRUE, caption = "Skaters who Participated in Both Events",
    caption.placement = getOption("xtable.caption.placement", "top")
    )
    output$tssplot <- renderPlot({
        dat1 <- dataset1Input()
        dat2 <- dataset2Input()
        if("TSSCompare" %in% Compare()){
            boxplot(dat1$TSS, dat2$TSS, main="TSS by Event", names=c("1", "2"), xlab="Event", ylab="Scores")
        }
    })
    output$bvplot <- renderPlot({
        dat1 <- dataset1Input()
        dat2 <- dataset2Input()
        if("bvCompare" %in% Compare()){
            boxplot(dat1$bv, dat2$bv, main="Base Value Scores by Event", names=c("1", "2"), xlab="Event", ylab="Scores")
        }
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
