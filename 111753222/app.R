library(shiny)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)

data(iris)

ui <- fluidPage(
  titlePanel("111753222 NCCU CS ChunAnLi PCA Homework"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "analysis",
                  label = "Choose analysis",
                  choices = c("PCA", "CA"),
                  selected = "PCA"),
      conditionalPanel(
        condition = "input.analysis == 'PCA'",
        selectInput(inputId = "PC",
                    label = "Choose PC component to show",
                    choices = c("PC1", "PC2", "PC3", "PC4"),
                    selected = "PC1")
      ),
      conditionalPanel(
        condition = "input.analysis == 'CA'",
        selectInput(inputId = "variable",
                    label = "Choose variable to show",
                    choices = c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                    selected = "Species")
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$analysis == "PCA") {
      log.ir <- log(iris[, 1:4])
      ir.species <- iris[, 5]
      ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
      
      if (input$PC == "PC1") {
        ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species) +
          scale_color_discrete(name = '') +
          theme(legend.direction = 'horizontal', legend.position = 'top') +
          ggtitle("PC1")
      } else if (input$PC == "PC2") {
        ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species) +
          scale_color_discrete(name = '') +
          theme(legend.direction = 'horizontal', legend.position = 'top') +
          ggtitle("PC2")
      } else if (input$PC == "PC3") {
        ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species) +
          scale_color_discrete(name = '') +
          theme(legend.direction = 'horizontal', legend.position = 'top') +
          ggtitle("PC3")
      } else if (input$PC == "PC4") {
        ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species) +
          scale_color_discrete(name = '') +
          theme(legend.direction = 'horizontal', legend.position = 'top') +
          ggtitle("PC4")
      }
    } else if (input$analysis == "CA") {  iris_ca <- CA(iris[, 1:4], graph = FALSE)
    
    if (input$variable == "Species") {
      fviz_ca_col(iris_ca, col.var = "contrib", gradient.cols = c("#FF0000", "#0000FF"), repel = TRUE) +
        ggtitle("Species")
    } else if (input$variable == "Sepal.Length") {    fviz_ca_row(iris_ca, axes = c(1, 2), geom = "point", pointsize = 2) +
        ggtitle("Sepal.Length")
    } else if (input$variable == "Sepal.Width") {
      fviz_ca_row(iris_ca, axes = c(1, 3), geom = "point", pointsize = 2) +
        ggtitle("Sepal.Width")
    } else if (input$variable == "Petal.Length") {
      fviz_ca_row(iris_ca, axes = c(1, 4), geom = "point", pointsize = 2) +
        ggtitle("Petal.Length")
    } else if (input$variable == "Petal.Width") {
      fviz_ca_row(iris_ca, axes = c(2, 3), geom = "point", pointsize = 2) +
        ggtitle("Petal.Width")
    }
    }
  })
}

shinyApp(ui = ui, server = server)
      