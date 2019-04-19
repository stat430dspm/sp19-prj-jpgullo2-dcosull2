# sp19-prj-jpgullo2-dcosull2
Private Project for STAT430 DSPM (Spring 2019) by jpgullo2 and dcosull2

For our final project, we are looking into a comprehensive historical dataset of baseball statistics. We want to first appropriately read in the file and load it as a data.frame, this step will potentially include scraping websites such as baseball-reference. We will evaluate the data for any irregularities or errors, and fix any issues using the appropriate cleaning techniques. To get a good understanding of our data we want to start by plotting different variables to see if there are any trends and obvious relationships among the data. Anything notable right off the bat will be cleaned up and put in our final report. 

Our hope is to use dplyr to pull out various portions of the data, for example we could use mutate to add a column for total home runs in a year, and then sort the years by home-runs. As baseball fans, we know there will be years with high home runs (like the steroid era) and potentially low home runs (early years). Utilizing dplyr will give us lots of options when evaluating the data. 

The biggest focus of our project is going to be a shiny app that will allow the user to be able to select options like year, names, positions, and various metrics like avg,obp,ops and shw corresponding visualizations. This final product will make it simple to check something like total homeruns in 1987 for first basemen, or batting average for short-stops over the last 30 years as a trend. There are infinite questions we can try and answer with this dataset.

If time allows we may also attempt to create more advanced visualizations utilizing shiny with advanced Statcast data.
