# A Module for configuring user dropdown input for the dashboard

# Create dataframe for storing aesthetic album details
albumname <- c("The College Dropout","Late Registration",
               "Graduation","808s & Heartbreak",
               "My Beautiful Dark Twisted Fantasy", "Yeezus",
               "The Life of Pablo","Ye","Jesus Is King")
sv_path <- c("the_college_dropout.jpg",
             "Late_registration.jpg",
             "Graduation.jpg",
             "808s_Heartbreak.png",
             "MBDTF.jpg",
             "Yeezus.png",
             "The_life_of_pablo.jpg",
             "Ye.jpg",
             "Jesus_Is_King.png")

album_fill <- c("#e8982e", "#633119","#b25186",
                "#b72124", "#ad2110","#fe0000",
                "#f58b57", "#00FF00", "#0000fe")
album_border <- c("#411218", "#7398aa", "#853082",
                  "#c2cece", "#ffd700", "#C0C0C0",
                  "#412517","#87ceeb","#ffd700")
chart_config <- data.frame(albumname,sv_path, album_fill,
                           album_border)
# Module UI
dropdownUI <-
  function(id, lab, dataIn) {
    selectInput(NS(id,"varname"), label = lab, choices = dataIn)
  }

# Module server
dropdownserver <- function(input, output, session){
  
  #Function checks what namespace id is before returning
  #reactively either a df with album details or word list
  
  return (
    reactive(
      if (grepl("album", session$ns("tmp"), fixed = TRUE)){
            chart_config %>% filter(albumname== input$varname)
      }
      else{input$varname}
      )
    )
  #print(session$ns("tmp"))
  #return(reactive({session$ns}))
  

}
