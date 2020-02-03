
############################################################################################################################
# Loading data
orig_data <- read_excel("data/grwth_county.xlsx")
grwth_county <- read_excel("data/grwth_county.xlsx")

############################################################################################################################
# data prepocessing
grwth_county$tax_in <- grwth_county$`Income Tax`/grwth_county$Total_population
grwth_county$tax_sr <- grwth_county$`Sales and Gross Receipts Tax`/grwth_county$Total_population
grwth_county$pct_wrk_class <- (grwth_county$Age_group_18to34 + grwth_county$Age_group_35to49+ grwth_county$Age_group_50to74)
grwth_county$pct_dep_class <- (grwth_county$Age_group_75andover + grwth_county$Age_group_5to17)

# dropping irrelevant variables
grwth_county <- grwth_county %>% select(-Id,-State,-Total_population,-(Age_group_5to17:Age_group_75andover),
                                        -pct_Less_than_9th_grade,-`pct_Some_college,no_degree`,
                                        - Employed,- `Income Tax`, - `Sales and Gross Receipts Tax`)



############################################################################################################################
# renaming colnames
colnames(grwth_county) <- c("id","county","sex_ratio","pct_empd",
                            "pct_occ01","pct_occ02","pct_occ03","pct_occ04","pct_occ05",
                            "pct_cow01","pct_cow02","pct_cow03","pct_cow04",
                            "income","pct_bpl",
                            "pct_lessthan_highgrad","pct_highschool_grad","pct_bachdeg_or_highr",
                            "tax_in","tax_sr","pct_wrk_class","pct_dep_class")

# now we have 19 predictor variables with target variable as income

#########################################################################################
# Mutiplot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################################################################################################################
# Univariate data exploration

#saved as boxplot_1 in plots folder
p1 <- ggplot(grwth_county,aes("Sex Ratio",sex_ratio)) + geom_boxplot()
p2 <- ggplot(grwth_county,aes("Percentage Employed",pct_empd)) + geom_boxplot()
p3 <- ggplot(grwth_county,aes("Percentage Below Poverty Line",pct_bpl)) + geom_boxplot()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

#saved as boxplot_2 in plots folder  
# confused with pct values here as they appear to be ratio
p1 <- ggplot(grwth_county,aes("Management, business, science, and arts occupations",pct_occ01)) + geom_boxplot()
p2 <- ggplot(grwth_county,aes("Service occupations",pct_occ02)) + geom_boxplot()
p3 <- ggplot(grwth_county,aes("Sales and office occupations",pct_occ03)) + geom_boxplot()
p4 <- ggplot(grwth_county,aes("Natural resources, construction, and maintenance occupations",pct_occ04)) + geom_boxplot()
p5 <- ggplot(grwth_county,aes("Production, transportation, and material moving occupations",pct_occ05)) + geom_boxplot()
layout <- matrix(c(1,2,3,4,5,5),3,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,p5,layout = layout)

#saved as boxplot_3 in plots folder
p1 <- ggplot(grwth_county,aes("Private wage and salary workers",pct_cow01)) + geom_boxplot()
p2 <- ggplot(grwth_county,aes("Government workers",pct_cow02)) + geom_boxplot()
p3 <- ggplot(grwth_county,aes("Self-employed in own not incorporated business workers",pct_cow03)) + geom_boxplot()
p4 <- ggplot(grwth_county,aes("Unpaid family workers",pct_cow04)) + geom_boxplot()
layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,layout = layout)

#saved as boxplot_4 in plots folder
p1 <- ggplot(grwth_county,aes("Less than high school graduate",pct_lessthan_highgrad)) + geom_boxplot()
p2 <- ggplot(grwth_county,aes("High school graduate ",pct_highschool_grad)) + geom_boxplot()
 p3 <- ggplot(grwth_county,aes("Bachelor's degree or higher",pct_bachdeg_or_highr)) + geom_boxplot()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

#saved as boxplot_5 in plots folder
p1 <- ggplot(grwth_county,aes("Income Tax",tax_in)) + geom_boxplot()
p2 <- ggplot(grwth_county,aes("Sales and Gross Receipts Tax",tax_sr)) + geom_boxplot()
p3 <- ggplot(grwth_county,aes("Working Class",pct_wrk_class)) + geom_boxplot()
p4 <- ggplot(grwth_county,aes("Dependent Class",pct_dep_class)) + geom_boxplot()
layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,layout = layout)

# saved as summary_n_iqr_1 in snapshots folder
summary(grwth_county$sex_ratio)
IQR(grwth_county$sex_ratio)
summary(grwth_county$pct_empd)
IQR(grwth_county$pct_empd)
summary(grwth_county$pct_bpl)
IQR(grwth_county$pct_bpl)

# saved as summary_n_iqr_2 in snapshots folder
summary(grwth_county$pct_occ01)
IQR(grwth_county$pct_occ01)
summary(grwth_county$pct_occ02)
IQR(grwth_county$pct_occ02)
summary(grwth_county$pct_occ03)
IQR(grwth_county$pct_occ03)
summary(grwth_county$pct_occ04)
IQR(grwth_county$pct_occ04)
summary(grwth_county$pct_occ05)
IQR(grwth_county$pct_occ05)

# saved as summary_n_iqr_3 in snapshots folder
summary(grwth_county$pct_cow01)
IQR(grwth_county$pct_cow01)
summary(grwth_county$pct_cow02)
IQR(grwth_county$pct_cow02)
summary(grwth_county$pct_cow03)
IQR(grwth_county$pct_cow03)
summary(grwth_county$pct_cow04)
IQR(grwth_county$pct_cow04)

# saved as summary_n_iqr_4 in snapshots folder
summary(grwth_county$pct_lessthan_highgrad)
IQR(grwth_county$pct_lessthan_highgrad)
summary(grwth_county$pct_highschool_grad)
IQR(grwth_county$pct_highschool_grad)
summary(grwth_county$pct_bachdeg_or_highr)
IQR(grwth_county$pct_bachdeg_or_highr)

# saved as summary_n_iqr_5 in snapshots folder
summary(grwth_county$tax_in)
IQR(grwth_county$tax_in)
summary(grwth_county$tax_sr)
IQR(grwth_county$tax_sr)
summary(grwth_county$pct_wrk_class)
IQR(grwth_county$pct_wrk_class)
summary(grwth_county$pct_dep_class)
IQR(grwth_county$pct_dep_class)

# Check for an NA value
sum(is.na(grwth_county)) # this gives that we do not have any NA value in our data

############################### Histograms ############################################

p1 <- ggplot(grwth_county,aes(sex_ratio)) + geom_histogram() + xlab("Sex Ratio")
p2 <- ggplot(grwth_county,aes(pct_empd)) + geom_histogram() + xlab("Percentage Employed")
p3 <- ggplot(grwth_county,aes(pct_bpl)) + geom_histogram() + xlab("Percentage Below Poverty Line")
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

p1 <- ggplot(grwth_county,aes(pct_occ01)) + geom_histogram() + xlab("Management, business, science, and arts occ")
p2 <- ggplot(grwth_county,aes(pct_occ02)) + geom_histogram() + xlab("Service occ")
p3 <- ggplot(grwth_county,aes(pct_occ03)) + geom_histogram() + xlab("Sales and office occ")
p4 <- ggplot(grwth_county,aes(pct_occ04)) + geom_histogram() + xlab("Natural resources, Maintenance occ")
p5 <- ggplot(grwth_county,aes(pct_occ05)) + geom_histogram() + xlab("Production, transportation, and material moving occ")
layout <- matrix(c(1,2,3,4,5,5),3,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,p5,layout = layout)


p1 <- ggplot(grwth_county,aes(pct_cow01)) + geom_histogram() + xlab("Private wage and salary workers")
p2 <- ggplot(grwth_county,aes(pct_cow02)) + geom_histogram() + xlab("Government workers")
p3 <- ggplot(grwth_county,aes(pct_cow03)) + geom_histogram() + xlab("Self-employed in own not incorporated business workers")
p4 <- ggplot(grwth_county,aes(pct_cow04)) + geom_histogram() + xlab("Unpaid family workers")
layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,layout = layout)


p1 <- ggplot(grwth_county,aes(pct_lessthan_highgrad)) + geom_histogram() + xlab("Less than high school graduate")
p2 <- ggplot(grwth_county,aes(pct_highschool_grad)) + geom_histogram() + xlab("High school graduate")
p3 <- ggplot(grwth_county,aes(pct_bachdeg_or_highr)) + geom_histogram() + xlab("Bachelor's degree or higher")
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)


p1 <- ggplot(grwth_county,aes(tax_in)) + geom_histogram() + xlab("Income Tax")
p2 <- ggplot(grwth_county,aes(tax_sr)) + geom_histogram() + xlab("Sales and Gross Receipts Tax")
p3 <- ggplot(grwth_county,aes(pct_wrk_class)) + geom_histogram() + xlab("Working Class")
p4 <- ggplot(grwth_county,aes(pct_dep_class)) + geom_histogram() + xlab("Dependent Class")
layout <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,layout = layout)

