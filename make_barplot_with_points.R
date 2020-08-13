function (fig_nr) 
{
	library(ggplot2)
	library(gridExtra)
	library(dplyr)
	library(backports)

	options(stringsAsFactors=FALSE)

	df=data.frame()
		
	if(fig_nr=="fig5b"){
		df=rbind(df,c(0.7705731113,0.7415643271,0.8674854709,0,0,0.01207467568,0,0.1392852684,0.1964968491,0,0.004436568626,0.08633179229))
		df=rbind(df,c(0.3409276464,0.7231941937,0.4874863575,0,0.01268447176,0.009027046473,0,0,0.3241040042,0,0.003433504929,1.394213981))
		df=rbind(df,c(0.1197334367,0.6329541928,0.7087559025,0,0.02966898742,0.009372887834,0,0.09237810423,0.3309958996,0,0.003610755005,0.1180197531))
		colnames(df)=c("PanicleMeristem 1_1","PanicleMeristem 1_2","PanicleMeristem 1_3","Dev.leaf (1st+2nd)","Internode","Node","Root","Lemma","Palea","Stamen","Ovule","Glume")
	}

	df2=data.frame()
	for(x in 1:dim(df)[1]){
		for(y in 1:dim(df)[2]){
			df2=rbind(df2,c(colnames(df)[y],df[x,y]))
		}
	}
	df2[,2]=as.numeric(df2[,2])

	colnames(df2)=c("A","B")

	print(df2)

	se <- function(x){sd(x)/sqrt(length(x))}
	my_dat <- summarise(group_by(df2, A), my_mean = mean(B), my_se = se(B))

	p1 <- ggplot() + 
	  geom_bar(data = my_dat,
	           aes(y = my_mean, x = A,
	               ymin = my_mean - my_se,
	               ymax = my_mean + my_se), stat="identity", width=0.75) + 
	  geom_errorbar(data = my_dat,
	                aes(y = my_mean, x = A,
	                    ymin = my_mean - my_se,
	                    ymax = my_mean + my_se), stat="identity", width=0.75) + 
	  geom_point(data = df2, aes(y = B, x = A, fill = "red")) +
  	theme_classic() + ylim(c(0,2))
	p1
}
