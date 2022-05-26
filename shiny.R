#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(shinyWidgets)
library(reticulate)
library(shinycssloaders)

pd <- import("pandas")
np <- import("numpy")
re <- import("re")
gensim <- import("gensim")
Doc2Vec <- gensim$models$doc2vec$Doc2Vec
Phrases <- gensim$models$phrases$Phrases
Phraser <- gensim$models$phrases$Phraser
lev <- import("Levenshtein")
# prereqs_file<-read.delim('pre_reqs.txt')
# 
# prereqs_courses <- as.vector(unlist(t(as.matrix(prereqs_file))))

# use python source code
source_python("functions.py")


# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "))), 
  
 setBackgroundColor(
   color = c("#eeeeee") ),

    # Application title

  titlePanel(title=div(img(src= "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/UCLA_Bruins_logo.svg/2560px-UCLA_Bruins_logo.svg.png",width=100), 
                       "Find Similar Classes!", style = "color:0E3E5D")),
  

p(a(href = "https://registrar.ucla.edu/academics/course-descriptions", "Click here to check all available classes at UCLA.")),
    

    sidebarLayout(
        sidebarPanel(
          p(h4("FILTERS")),
          helpText("Note: Sorted by Similarity"),
          p(h5("Use Course Name To Find Similar Courses")),

          textInput("number", label = "Course Name", value = ""),
          
          textInput("prereq", label = "Pre-Requisite", value = ""),
          div(style = "margin-top:-27.5px"),
          textInput("prereq2", label = "", value = ""),
          div(style = "margin-top:-27.5px"),
          textInput("prereq3", label = "", value = ""),
          
              # selectInput(inputId = "prereq",
              #         label = "Pre Requisite",
              #         choices = prereqs_courses),
          
          tags$div(align = "left", class = "multicol",
                   checkboxGroupInput(inputId = "boxtyp", label = "Class Type", 
                                      choices = c("lecture","discussion","laboratory","seminar","research","tutorial",
                                                  "studio","field","clinic","activity","recitation"))),
          
          selectInput(inputId = "level", label = "Level of Class", 
                      choices = c("Undergraduate", "Graduate","Law")),
              
          checkboxInput(inputId = "Impacted", 
                        label = strong("Show non-impacted courses only"), 
                        value = FALSE),
              
          checkboxInput(inputId = "Hours", 
                        label = strong("Show courses with same hours"), 
                        value = FALSE ),
          
          checkboxInput(inputId = "Department",
                        label = strong("Order by Department Similarity"),
                        value = FALSE),
              
          actionButton("update", "Start", 
                       style="color:#FFFF ; background-color:  #2774AE; border-color:  #2774AE"),
          br(),
          # p(h5("Use Course Descrption Phrase Find Similar")),
          p(h5(" ")),
          p(h5(" ")),
          
          textInput("phrase",
                    label = "Course Description",
                    value = ""),
          
          actionButton("update2", "Start",style="color:#FFFF ; 
                           background-color:  #2774AE; border-color:  #2774AE"),
              
             # plotOutput(outputId = "main_plot", height = "300px")
              
              # Display this only if the density is shown
              # conditionalPanel(condition = "input.density == true", 
              #                  sliderInput(inputId = "bw_adjust",
              #                              label = "Bandwidth adjustment:",
              #                              min = 0.2, max = 2, value = 1, step = 0.2)
              # )
          ),
        
        mainPanel(
          tabsetPanel(
            id = 'dataset',
            tabPanel("Use Course Number Find Similar", DT::dataTableOutput("mytable"),icon = icon("book")),
         
            tabPanel("Use Course Descrption Phrase Find Similar", DT::dataTableOutput("mytable2"),icon = icon("book")),
            tabPanel("Help",icon = icon("question-circle"),
            h3("Course Not Found"),
            p( "If you’re unable to find a course on the website, please check the complete information of all classes 
               at UCLA:",tags$a(href = "https://registrar.ucla.edu/academics/course-descriptions",
                       "https://registrar.ucla.edu/academics/course-descriptions")),
            br(),
            h4("Contact Us"),
            p("Phone: (310)862-9183"),
            p("Email: feedbackStats141XP@gmail.com"),
            br(),
            p( h4("More Information about the Website"),tags$a(href = "https://drive.google.com/drive/folders/1UC9zEDsMvI-tS81ynulciJrDxmL306r0?usp=sharing",
                                                              "https://drive.google.com/drive/folders/1UC9zEDsMvI-tS81ynulciJrDxmL306r0?usp=sharing")),
               tags$a(href = "https://github.com/caoalbert/course_similarity",
                  "https://github.com/caoalbert/course_similarity")))

        )
    )
)
 


# Define server logic required to draw a histogram
server <- function(input, output) {
  text_reactive <- eventReactive( input$update,{
    input$number
  })
  similar_course_data <- reactiveVal()
  observe({
    
    typ = r_to_py(input$boxtyp)
    
    
    
    
    similar = course_find_similar(input$number,input$prereq, input$prereq2, input$prereq3, typ, input$Hours,input$Department,input$level,input$Impacted)

    newcoursedata = data.frame()
    for (i in 1:length(similar)){
      similarcourse = courseData[which(courseData["course_num"]==similar[i]),]
      newcoursedata = rbind(newcoursedata,similarcourse)
    }
    similar_course_data(newcoursedata)
    })
  
  text_reactive2 <- eventReactive( input$update2,{
    input$phrase
  })
  
  
  
  similar_course_data2 <- reactiveVal()
  observe({
    similar2 = phrase_find_similar(input$phrase)
    if (length(similar2) == 0) {
      newcoursedata2 = data.frame(matrix(ncol = 7, nrow = 0))
      x <- c("course_num", "subj_area_cd", "crs_career_lvl_cd", "crs_act_typ_cd", "crs_grd_typ_cd", "hours", "impacted_crs_fl")
      colnames(newcoursedata2) <- x
    }

    else {
      newcoursedata2 = data.frame()
      for (i in 1:length(similar2)){
        similarcourse = courseData[which(courseData["course_num"]==similar2[i]),]
        newcoursedata2 = rbind(newcoursedata2,similarcourse)
      }
    }
    similar_course_data2(newcoursedata2)
  })
  
  
  output$mytable <- DT::renderDataTable({
    DT::datatable(similar_course_data(), text_reactive(),
                  colnames = c("Class Name"= "course_num", 'Department' = 'subj_area_cd',
                                                    'Class Level' = 'crs_career_lvl_cd','Class Type' = 'crs_act_typ_cd',
                                                    'Grade Type' = "crs_grd_typ_cd",
                                                    'Hours' = 'hours','Impacted' = 'impacted_crs_fl'), 
                  options = list(lengthMenu = c(3, 5, 10), pageLength = 10))
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(similar_course_data2(), text_reactive2(),
                  colnames = c("Class Name"= "course_num", 'Department' = 'subj_area_cd',
                               'Class Level' = 'crs_career_lvl_cd','Class Type' = 'crs_act_typ_cd',
                               'Grade Type' = "crs_grd_typ_cd",
                               'Hours' = 'hours','Impacted' = 'impacted_crs_fl'), 
                  options = list(lengthMenu = c(3, 5, 10), pageLength = 10))
  })
  

}

# Run the application 
courseData <- read.csv("parsed_coursenum.csv")
courseData <- courseData %>% select(course_num,subj_area_cd,crs_career_lvl_cd,crs_act_typ_cd,
                                    crs_grd_typ_cd,hours,impacted_crs_fl)
shinyApp(ui = ui, server = server)

