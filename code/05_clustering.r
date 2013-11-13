# 05_clustering.r
# :purpose: Run GMM via EM to find clusters
# :author: jon.sedar@applied.ai
# :date: Mon 04 Nov 2013



# Initial plot and model
p1 <- ggplot(cst[seq(1,nrow(cst),10)]) +
    geom_point(aes(x=prodctpb_max,y=popcat_nbaskD))
plot(p1)

sptclst <- Mclust(cst[seq(1,nrow(cst),40),list(prodctpb_max,popcat_nbaskD)],G=(1:6))
summary(sptclst,parameters=T)
plot(sptclst)



### Limit to subsample of rows and first 3 PCs keep most of the variance, takes less time
moddata <- cstPCA[round(seq(1,nrow(cstPCA),10)),(2:6),with=F]

# exploratory run
cstPCAclst <- Mclust(moddata)
summary(cstPCAclst)
plot(cstPCAclst)

### arbitrary limit to N clusters
modclust <- (4)      

### VII: spher|var(vol)|eq(shape)
### VVI: diago|var(vol)|var(shape)|coord(orientation)           ### much easier to explain against PCs
### VVV: ellip|var(vol)|var(shape)|var(orientation)
modnames <- c("VVV") #,"VVI","VVV")

### exploratory runs
cstPCAclst <- Mclust(moddata,G=modclust,modelNames=modnames)
summary(cstPCAclst)
#plot(cstPCAclst,what="BIC")
plot(cstPCAclst,what="classification")          # /workspace/segmentation/img/splot_stats9_PC5_C5_VVV_2.pdf
                                                # /workspace/segmentation/img/splot_stats9_PC5_C4_VVV_1.pdf
summary(sptPCAclst,parameters=T)                # /workspace/segmentation/notes/summary_stats9_PC5_C5_VVV_2.txt
# str(sptPCAclst)


dev.copy(pdf,file=paste(dir,file,sep="/")




### output class labels --------------------------------------------------------

usrclstr <- usr[,list(user_id=id)]
usrclstr[,value_date:=format(today(),"%Y-%m-%d")]
usrclstr[,value:=c(sample(1:4,nrow(usr),replace=T))]
usrclstr[,value_strength:=runif(nrow(usr))]
usrclstr[,type:=get_random_word(),by=user_id]


set_data(usrclstr,"user_cluster")