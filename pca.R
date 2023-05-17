library(shiny)
library(factoextra)
library(ggplot2)

# 读取数据集
data(iris)

# 创建PCA模型
pca_model <- prcomp(iris[, 1:4], scale. = TRUE)

# 创建UI
ui <- fluidPage(
  titlePanel("Interactive PCA analysis for Iris data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("component", "Select a component:", 
                  choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC1")
    ),
    mainPanel(
      plotOutput("pca_plot")
    )
  )
)

# 创建Server
server <- function(input, output) {
  # 根据选择的成分绘制PCA图
  output$pca_plot <- renderPlot({
    if (input$component == "PC1") {
      fviz_pca_var(pca_model, axes = 1, geom = "point", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, labelsize = 8)
    } else if (input$component == "PC2") {
      fviz_pca_var(pca_model, axes = 2, geom = "point", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, labelsize = 8)
    } else if (input$component == "PC3") {
      fviz_pca_var(pca_model, axes = 3, geom = "point", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, labelsize = 8)
    } else if (input$component == "PC4") {
      fviz_pca_var(pca_model, axes = 4, geom = "point", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE, labelsize = 8)
    }
  })
}

# 运行App
shinyApp(ui = ui, server = server)
