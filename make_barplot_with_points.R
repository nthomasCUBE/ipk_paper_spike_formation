function (fig_nr) 
{
	library(ggplot2)
	library(gridExtra)
	library(dplyr)
	library(backports)
	library(showtext)


	options(stringsAsFactors=FALSE)

	df=data.frame()
		
	if(fig_nr=="fig5g"){
		df=rbind(df,c(0.7705731113,0.7415643271,0.8674854709,0,0,0.01207467568,0,0.1392852684,0.1964968491,0,0.004436568626,0.08633179229))
		df=rbind(df,c(0.3409276464,0.7231941937,0.4874863575,0,0.01268447176,0.009027046473,0,0,0.3241040042,0,0.003433504929,1.394213981))
		df=rbind(df,c(0.1197334367,0.6329541928,0.7087559025,0,0.02966898742,0.009372887834,0,0.09237810423,0.3309958996,0,0.003610755005,0.1180197531))
		colnames(df)=c("PanicleMeristem 1_1","PanicleMeristem 1_2","PanicleMeristem 1_3","Dev.leaf (1st+2nd)","Internode","Node","Root","Lemma","Palea","Stamen","Ovule","Glume")
	}else if(fig_nr=="fig6i"){
		df=data.frame()
		df=rbind(df,c(0.1208492383,0,0.04189538913,0,0,0,0,0,0,0,0))
		df=rbind(df,c(0.03734358606,0.09154496087,0,0,0,0,0,0,0.1936070355,0,0))
		df=rbind(df,c(0.1151393026,0.074961541,0.1005298806,0,0,0,0,0,0.171326222,0,0))
		colnames(df)=c("Floret.M","Terminal.SM","BM","Dev.leaf (1st+2nd)","Internode","Node","Tiller.Buds","Lemma","Palea","Stamen","Ovule")
	}else if(fig_nr=="fig7a"){
		df=data.frame()
		df=rbind(df,c(0.6206792344,0.8679024398,0.4982339667,0.3175868945))
		df=rbind(df,c(0.5769694292,0.6464155392,0.4946391322,0.2342548438))
		df=rbind(df,c(0.6425017481,0.4606949806,0.3172029921,0.3852454398))
		colnames(df)=c("Triple Mound","Glume Primordium","Stamen Primordium","Awn Primordium")
	}else if(fig_nr=="fig7b"){
		df=data.frame()
		df=rbind(df,c(0.000,0.805,0.078,0.142,0.000,1.861,0.498,0.359))
		df=rbind(df,c(0.000,0.030,0.020,0.000,0.000,NA,NA,NA))
		df=rbind(df,c(0.000,1.024,0.195,0.000,0.000,NA,NA,NA))
		colnames(df)=c("DL","TB","IN","N","R","IS-B","IS-C","IS-A")
	}else if(fig_nr=="fig8o"){
		df=data.frame()
		df=rbind(df,c(0.9451904382,0.7028056476,1.683656337,1.985032241,1.936736173,1.424544254))
		df=rbind(df,c(1.840139882,1.442164705,1.760995765,2.062078407,1.98231769,1.054917875))
		df=rbind(df,c(2.47287307,2.760651335,2.857246865,1.543723038,2.236258847,1.116897614))
		colnames(df)=c("Triple Mound (Bowman)","Triple Mound (com1.a)","Glume Primordium (Bowman)", "Glume Primordium (com1.a)","Stamen+Lemma Primordia (Bowman)","Stamen+Lemma Primordia (com1.a)") 
	}

	df2=data.frame()
	for(x in 1:dim(df)[1]){
		for(y in 1:dim(df)[2]){
			df2=rbind(df2,c(colnames(df)[y],df[x,y]))
		}
	}
	df2[,2]=as.numeric(df2[,2])
	colnames(df2)=c("A","B")

	print(df)

	se <- function(x){sd(x)/sqrt(length(x))}
	my_dat <- summarise(group_by(df2, A), my_mean = mean(B), my_se = se(B))

	pdf(paste0(fig_nr,".pdf"),width=10,height=5)
	p1 <- ggplot() + 
	  geom_bar(data = my_dat,
	           aes(y = my_mean, fill = as.factor(A), label=my_label,x = A,
	               ymin = my_mean - my_se,
	               ymax = my_mean + my_se),stat="identity", width=0.75) + 
	  geom_errorbar(data = my_dat,
	                aes(y = my_mean, x = A,
	                    ymin = my_mean - my_se,
	                    ymax = my_mean + my_se), stat="identity", width=0.75) + 
	  geom_point(data = df2, aes(y = B, x = A)) +
  	ylim(c(0,2))
	p1=p1+theme(axis.text.x = element_text(angle = 45, hjust=1))
	p1=p1+ylab(expression(paste("Relative expression to ",italic("BdActin"))))
	p1=p1+scale_x_discrete(limits=colnames(df))
	print(p1)
	dev.off()
}
