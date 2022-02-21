#MMID Coding Workshop (Feb 23/22)
#Instructor: MacKenzie Wilke 

#Load in the libraries
library(dplyr) #install.packages("dyplr")
library(readxl) #install.packages("readxl")
library(snakecase) #install.packages("snakecase")
library(ggplot2) #install.packages("ggplot2")
library(cowplot) #install.packages("cowplot")
library(plotly) #install.packages("plotly")


#Step 1. Read in Excel File 
#Note: CLUE output via Josset et al. supplementary table 5

clue_results<-read_excel("/Users/msarvis/Desktop/MMID/mmid_workshop.xls", sheet=1) %>% 
  rename_with(to_snake_case)

#See what it looks like
glimpse(clue_results)

#Step 2. Change the perturbagen names into a factor and give it levels
#This is to sort the perturbagens in a way that isn't alphabetic 
clue_results$perturbagen <- factor(clue_results$perturbagen, levels = clue_results$perturbagen)

#Step 3. Create a basic plot
basic_plot<-ggplot(clue_results, aes(x=median_connectivity_score, y=perturbagen)) +
  geom_point()

#View the plot
basic_plot

#Step 4. Change the colour of the perturbagens based on their connectivity score
#Blue is when the score is less than 0 and red is when it is greater than 0, save it as value
colours <- ifelse(clue_results$median_connectivity_score < 0, "blue", "red")

#Step 5. Adding in extra details to our plot (i.e. colour, shape and size)
drug_repurposing_plot<-basic_plot + geom_point(aes(colour=gene_expression_profile, shape= perturbagen_type, size= perturbagen_type)) + 
  theme_cowplot(12) + ggtitle("Top Antiviral Drug Repurposing Results")+ 
    xlab("Median Connectivity Score") +ylab ("Perturbagen") + xlim(-1,1) +
      theme(plot.title = element_text(size=20, hjust = 0.5), axis.text.y = element_text(hjust = 1, colour=colours),
            legend.title = element_text(face = "bold")) +
               scale_shape_manual(values= c(19,18,17,15))+
                  scale_color_manual(values = c("Disimilar" = "blue", "Similar" = "red")) +
                      scale_size_manual(values=c(3,4,3,3)) + guides(shape=guide_legend("Perturbagen Type")) +
                        guides(size=guide_legend("Perturbagen Type")) +
                         guides(color=guide_legend("Gene Expression Profile"))

#View the plot
drug_repurposing_plot

#See it interactively
ggplotly(drug_repurposing_plot)

#Step 6. Add in other plots so that we can use cow_plot
#Make another type of plot for our antiviral drug repurposing data -> bar plot

#6a. Filter results to perturbagens inducing dissimilar (and similar) gene expression profiles (i.e. blue and red perturbagens)
#Can also filter during the ggplot step if you want with a pipe
disimilar_perturbagens<-clue_results %>% filter(median_connectivity_score<0)
similar_perturbagens<-clue_results %>% filter(median_connectivity_score>0)

#6b. Make a basic bar graph
basic_bar_graph<-ggplot(disimilar_perturbagens, aes(x=perturbagen_type, fill=perturbagen)) +
  geom_bar()

#6c. Add in different elements you want to have
drug_repurposing_bar_plot_dissimilar<-basic_bar_graph + geom_bar() + theme_cowplot(12) + 
                              ggtitle("Breakdown of perturbagens inducing dissimilar gene expression profiles") +
                                  xlab("Perturbagen Type") + ylab ("Count") +
                                    theme (plot.title = element_text(size=20,hjust = 0.5), 
                                        legend.title = element_text(face = "bold")) +
                                            scale_fill_viridis_d()

#Bonus: See it interactively 
ggplotly(drug_repurposing_bar_plot_dissimilar)

##Now repeat steps 6a-c for the similar expression perturbagens

basic_bar_graph_similar<-ggplot(similar_perturbagens, aes(x=perturbagen_type, fill=perturbagen)) +
  geom_bar()

drug_repurposing_bar_plot_similar<-basic_bar_graph_similar + geom_bar() + theme_cowplot(12) + 
  ggtitle("Breakdown of perturbagens inducing similar gene expression profiles") +
  xlab("Perturbagen Type") + ylab ("Count") +
  theme (plot.title = element_text(size=20,hjust = 0.5), legend.title = element_text(face = "bold")) +
  scale_fill_viridis_d()

ggplotly(drug_repurposing_bar_plot_similar)

#Step 7. Use cow_plot (via plot_grid) to add the two plots together and produce a pdf image to save on your desktop 

#Can also do ncol=1 to have them on the same row instead of same column
#Other types of files you can save like .png, .tiff, etc
#Can also change the dpi for if the figure is for a poster, online presentation, etc

#We are creating the bottom row that we want to have, formatting how we want the layout to look
#We want the two bar charts to be on the bottom and the scatterplot on the top
bottom_row <- plot_grid(drug_repurposing_bar_plot_dissimilar, drug_repurposing_bar_plot_similar, 
                        labels = c('2', '3'), label_size = 12)

plot_grid(drug_repurposing_plot, bottom_row,
          labels = c('1', ''), label_size=12, ncol = 1) 

ggsave("/Users/msarvis/Desktop/MMID/drug_repurposing.pdf", height=15, width=25, units='in', dpi=300)

##The end!