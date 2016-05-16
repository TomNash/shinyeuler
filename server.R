list.of.packages <- c("venneuler", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(venneuler)
library(stringr)

shinyServer(function(input, output, session) {
  
  remove.whitespace <- function(input.list) {
    if(is.null(input.list))
      return(NA)
    filtered.list <- unique(strsplit(input.list, "\n")[[1]])
    filtered.list <- str_trim(filtered.list)
    filtered.list <- filtered.list[filtered.list != ""]
    return(filtered.list)  
  }
  
  check.input <- reactive({
    venn.listA <- remove.whitespace(input$listA)
    venn.listB <- remove.whitespace(input$listB)
    #venn.listC <- remove.whitespace(input$list3)
    
    # if (length(venn.listC) > 0) {
    #   if (length(venn.listB) == 0 | length(venn.listA) == 0) {
    #     validate(
    #       need(length(venn.listB) > 0 & length(venn.listA) > 0, "Use List 1 and 2 before List 3")
    #     ) 
    #   } else {
    #     validate(
    #       need(length(venn.listC) > 1, "List 3 must have more than one element")
    #     )
    #   }
    # }
    validate(
      need(length(venn.listA) > 1, "List A must have more than one element"),
      need(length(venn.listB) > 1, "List B must have more than one element"),
      need(length(intersect(venn.listA,venn.listB)) > 0,
           "No intersection exists between the List A and List B"),
      need(!identical(input$list.A.name, input$list.B.name), 
           "List A and B must have different names"),
      need(nchar(input$title) > 0, "The plot must have a title")
    )
    return(list(A=venn.listA, B=venn.listB)) #, C=venn.listC))
  })
  
  make.venn <- function(listA,listB) { #,listC) {
    venn.names <- make.names(c(input$list.A.name, input$list.B.name))
    
    listAB <- intersect(listA,listB)
    
    # if(!length(listC) == 0) {
    #   listAC <- intersect(listA,listC)
    #   listBC <- intersect(listB,listC)
    #   listABC <- intersect(listAB,intersect(listAC,listBC))
    #   
    #   listAinBC <- c(which(listA %in% listAB | listA %in% listAC))
    #   listBinAC <- c(which(listB %in% listAB | listB %in% listBC))
    #   listCinAB <- c(which(listC %in% listAC | listC %in% listBC))
    #   listABinABC <- c(which(listAB %in% listABC))
    #   listACinABC <- c(which(listAC %in% listABC))
    #   listBCinABC <- c(which(listBC %in% listABC))
    #   
    #   listAunique <- listA[-listAinBC]
    #   listBunique <- listB[-listBinAC]
    #   listCunique <- listC[-listCinAB]
    #   
    #   listABunique <- listAB[-listABinABC]
    #   listACunique <- listAC[-listACinABC]
    #   listBCunique <- listBC[-listBCinABC]
    #   
    #   venn.names <- c(venn.names,"C","A&B","A&C","B&C","A&B&C")
    #   venn <- venneuler(combinations = venn.names,
    #                     weights=c(length(listAunique),length(listBunique),
    #                               length(listCunique),length(listABunique),
    #                               length(listACunique),length(listBCunique),
    #                               length(listABC)))
    # 
    #   venn$labels <- c(paste0(input$list.A.name,"\n",length(listA)),
    #                    paste0(input$list.B.name,"\n",length(listB)),
    #                    paste0(input$list.C.name,"\n",length(listC)))
    #   
    #   return(list(diagram=venn,A=listAunique,B=listBunique,C=listCunique,
    #               AB=listABunique,
    #               AC=listACunique,
    #               BC=listBCunique,
    #               ABC=listABC))
    #   
    # } else {
      listA <- listA[-c(which(listA %in% listAB))]
      listB <- listB[-c(which(listB %in% listAB))]

      diagram <- venneuler(
        combinations = c(venn.names, paste0(venn.names[1], "&", venn.names[2])),
        weights=c(length(listA),length(listB),length(listAB)))

      diagram$labels <- c(paste0(input$list.A.name,"\n",length(listA)),
                       paste0(input$list.B.name,"\n",length(listB)))
       
      right.circle <- which.max(diagram$centers[, "x"])
      left.circle <- which.min(diagram$centers[, "x"])
      
      right.list.name <- names(which.max(diagram$centers[, "x"]))
      left.list.name <- names(which.min(diagram$centers[, "x"]))

      right.list <- list(listA, listB)[[grep(right.list.name, venn.names)]]
      left.list <- list(listA, listB)[[grep(left.list.name, venn.names)]]

      right.border <- diagram$centers[right.circle, "x"] - 
        diagram$diameters[right.circle]/2
      left.border <- diagram$centers[left.circle, "x"] + 
        diagram$diameters[left.circle]/2
      
      diagram.midpoint <- mean(c(left.border, right.border))
      
      return(list(diagram=diagram,
                  right.list=right.list,
                  left.list=left.list,
                  right.list.name=right.list.name,
                  left.list.name=left.list.name,
                  right=right.circle,
                  left=left.circle,
                  intersect=listAB,
                  midpoint=diagram.midpoint))
    #}
  }
  
  plotInput <- function() {
    venn.lists <- check.input()
    venn <<- make.venn(venn.lists$A, venn.lists$B) #, venn.lists$C)

    label.x.right <- venn$diagram$centers[venn$right,"x"] * 1.2
    label.x.left <-  venn$diagram$centers[venn$left,"x"] * 0.8
    label.y.right <- venn$diagram$centers[venn$right,"y"] - 
      venn$diagram$diameters[venn$right]/2 - 0.05
    label.y.left <- venn$diagram$centers[venn$left,"y"] - 
      venn$diagram$diameters[venn$left]/2 - 0.05
    
    count.x.right <- venn$diagram$centers[venn$right,"x"] - 
      (venn$diagram$diameters[venn$right]/2 * cos(35)) - 
      0.25*venn$diagram$diameters[venn$right]/2
    count.x.left <- venn$diagram$centers[venn$left,"x"] + 
      (venn$diagram$diameters[venn$left]/2 * cos(35)) +
      0.25*venn$diagram$diameters[venn$left]/2
    count.y.right <- venn$diagram$centers[venn$right,"y"] + 
      (venn$diagram$diameters[venn$right]/2 * sin(35)) + 
      0.25*venn$diagram$diameters[venn$right]/2
    count.y.left <- venn$diagram$centers[venn$left,"y"] + 
      (venn$diagram$diameters[venn$left]/2 * sin(35)) +
      0.25*venn$diagram$diameters[venn$left]/2
    
    venn$diagram$labels <- NA
    par(mar=c(5,4,4,2) + 1, xpd=T)
    plot(venn$diagram, main = input$title)
    text(x=venn$midpoint, y=0.5, as.character(length(venn$intersect)), 
         adj=c(0.5, NA))
    text(x=label.x.left, y=label.y.left, venn$left.list.name, adj=c(1,NA))
    text(x=label.x.right, y=label.y.right, venn$right.list.name, adj=c(0,NA))
    text(x=count.x.left, y=count.y.left, 
         as.character(length(venn$left.list)), adj=c(1,NA))
    text(x=count.x.right, y=count.y.right, 
         as.character(length(venn$right.list)), adj=c(0,NA))
  }
  
  output$plot <- renderPlot({
    plotInput()
  })

  output$info <- renderUI({
    # With base graphics, need to tell it what the x and y variables are.
    x <- input$plot_click$x
    y <- input$plot_click$y
    if(!is.null(input$plot_click)) {
      
      in.right <- sqrt((venn$diagram$centers[venn$right,1] - x)^2 + 
                         (venn$diagram$centers[venn$right,2]-y)^2) <= venn$diagram$diameters[venn$right]/2
      in.left <- sqrt((venn$diagram$centers[venn$left,1] - x)^2 + 
                        (venn$diagram$centers[venn$left,2]-y)^2) <= venn$diagram$diameters[venn$left]/2
      
      if(nrow(venn$diagram$centers) == 2) { # if 2 circles
        if (!in.right & !in.left) {
          tags$textarea(id="txtarea",rows=7, cols=83,style="resize:none",
                        paste0("Click within a valid region on the venn diagram"))
        } 
        else {
          if (in.left) {
            if (in.right) {
              result.group <- paste0("from the intersection of ", venn$left.list.name, 
                              " and ", venn$right.list.name)
              result.list <- venn$intersect
            } else {
              result.group <- paste0("unique to ", venn$left.list.name)
              result.list <- venn$left.list
            }
          } else {
            result.group <- paste0("unique to ", venn$right.list.name)
            result.list <- venn$right.list
          }
          
          tags$textarea(id="txtarea",rows=7, cols=83, style="resize:none",
                        paste0(length(result.list), " elements ",result.group,"\n",
                               paste(result.list, collapse = '\n')))
        }
      }
      # } else { # if 3 circles
      #   in3 <- sqrt((venn$diagram$centers[3,1] - x)^2 + 
      #                 (venn$diagram$centers[3,2]-y)^2) <= venn$diagram$diameters[3]/2
      #   if (!in1 & !in2 & !in3) {
      #     tags$textarea(id="txtarea",rows=7, cols=83,style="resize:none",
      #                   paste0("Click within a valid region on the venn diagram"))
      #   } else {
      #     if (in1) {
      #       if (in2) {
      #         if(in3) {
      #           group <- paste0(paste0("from the intersection of ", 
      #                                  input$list.A.name, " and ", 
      #                                  input$list.B.name, " and ",
      #                                  input$list.C.name))  
      #           list <- venn$ABC
      #         } else {
      #           group <- paste0("unique to the intersection of ",
      #                           input$list.A.name, " and ", input$list.B.name)
      #           list <- venn$AB
      #         }
      #       } else if (in3) {
      #         group <- paste0("unique to the intersection of ",
      #                         input$list.A.name, " and ", input$list.C.name)
      #         list <- venn$AC
      #       } else {
      #         group <- paste0("unique to ", input$list.A.name)
      #         list <- venn$A
      #       }
      #     } else if (in2) {
      #       if (in3) {
      #         group <- paste0("unique to the intersection of ",
      #                         input$list.B.name, " and ", input$list.C.name) 
      #         list <- venn$BC
      #       } else {
      #         group <- paste0("unique to ", input$list.B.name)
      #         list <- venn$B
      #       }
      #     } else if (in3) {
      #       group <- paste0("unique to ", input$list.C.name)
      #       list <- venn$C
      #     }
      #     tags$textarea(id="txtarea",rows=7, cols=83, style="resize:none",
      #                   paste0(length(list), " elements ",group,"\n",paste(list, collapse = '\n')))
      #   }
      # }
    } else {
      tags$textarea(id="txtarea",rows=7,cols=83,style="resize:none",
                    paste0("No selection has been made or plot has been updated"))
    }
  })
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0(input$title, '.png') },
    content = function(file) {
      
      png(file, width=800, height=600, unit="px", pointsize = 12, res=NA)
      plotInput()
      dev.off()
    }
  )
})
