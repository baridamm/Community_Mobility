# Community_Mobility

The goal of this project is to analyze the effect of COVID-19 on communities in different countries. For this purpose we use community mobility reports data. These Community Mobility Reports aim to provide insights into what has changed in response to policies aimed at combating COVID-19. The reports chart movement trends over time by geography, across different categories of places such as retail and recreation, groceries and pharmacies, parks, transit stations, workplaces, and residential.

The first task of the project is utilizing unsupervised machine learning to group similar 
countries together. We use K-means and hierarchical clustering for this purpose. Using internal validation metrics (Dunn index, Silhouette coefficient) we find the best number of clusters for each algorithm. The K-mean algorithm divides the countries into 3 groups. Scatter plots are used to show the difference between countries of different clusters. 
The hierarchical clustering divides the countries into 2 groups.

The second task of this project is time series analysis. We first use time series graphs to compare retail and recreation percent change from baseline between countries in the same cluster. It can be seen that countries in different clusters follow different trends. After that,
the different features for Canada is analysed. Using the ACF plot, it can be shown that the time series follow a trend and are not seasonal. Next we compare the top 10 days with the lowest percentage in different categories and we confirm that different features are related to each other. Finally, we forecast the retail and recreation percent change from the baseline for Canada for the next 30 days. For this purpose we use an ARIMA model.
