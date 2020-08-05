                install.packages('readxl')
                install.packages('dplyr')
                install.packages('countrycode')
                library('readxl')
                library(dplyr)
                my_data<-read_excel('/Users/shelleyxiang/Documents/MBA last spring term 第三学期/Marketing Big Data/Data_Extract_From_ICP_2017.xlsx')
                
                df<-data.frame(my_data)
                str(df)
                
                df1=subset(df,select=-c( X2015..YR2015., X2016..YR2016.,X2014..YR2014.,X2013..YR2013.,X2012..YR2012.,X2011..YR2011.))
                head(df1)
                
                sapply(df1, function(x) sum(is.na(x))) 
                #Each varibles only has 3-5 missing data, which does not have big impact.
                #delete the data with low missing rate
                df1<-df1[complete.cases(df1),]
                sapply(df1, function(x) sum(is.na(x))) 
                # No missing data for df1
                unique(df1$Classification.Name)
                
                df1$X2017..YR2017.<-as.numeric(df1$X2017..YR2017.)
                cols<-c(df1$X2017..YR2017.)
                df2<-df1 %>% select(Country.Name,Classification.Name,Series.Name,X2017..YR2017.) %>% rename(Y2017=X2017..YR2017.)
                
                df3<-df2 %>% filter(Classification.Name == "Price level index (world = 100)"& Series.Name=="1000000:GROSS DOMESTIC PRODUCT")%>% group_by(Country.Name )%>% rename(PRICE2017=Y2017) %>% select(Country.Name,PRICE2017)
                
                df4<-df2 %>% filter(Classification.Name == "Real expenditures (U.S. dollars)" & Series.Name=="1000000:GROSS DOMESTIC PRODUCT")%>% group_by(Country.Name )%>% rename(GDP2017=Y2017)%>%select(Country.Name,GDP2017)
                
                df5<-df2 %>% filter(Classification.Name == "Population" & Series.Name=="SP.POP.TOTL.ICP:Population")%>% group_by(Country.Name )%>% rename(Population2017=Y2017)%>%select(Country.Name,Population2017)
                
                df34 <-merge(df3,df4)
                PPPdf5 <-merge(df34,df5)
                PPPdf5$AverageGDP<-(PPPdf5$GDP2017/PPPdf5$Population2017)*10^9
                PPPdf5[is.na(PPPdf5)]=0
                PPPdf5$AverageGDP<-round(PPPdf5$AverageGDP,0)
                PPPdf5$GDP2017<-round(PPPdf5$GDP2017,1)
                library(ggplot2)
                library(dplyr)
                library(countrycode)
                
                df6<-c(PPPdf5$Country.Name)
                
                PPPdf5$continent <- countrycode(sourcevar =df6,
                                    origin = "country.name",
                                    destination = "region")
                unique(PPPdf5$continent) # check whether some values are NA
                Finaldf5=PPPdf5[complete.cases(PPPdf5), ] # delete the rows with NA
                
                # data visualization:
                
                
                gg<-ggplot(Finaldf5, aes(x=(AverageGDP), y=PRICE2017, col=continent)) +
                geom_jitter(aes(size=GDP2017)) 
                
                # Set color to vary based on continent categories
                
                gg1<-gg+labs(title="   PPP-based GDP per capital and GDP price level index, 2017",  x="PPP-based GDP per \n    capita,(US$)", 
                y="",caption="Bubble size for each economy shows its relative PPP-based GDP \n  Source:ICP 2017                                                ") +
                scale_color_manual(breaks = c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean","Middle East & North Africa","North America" ,"South Asia","Sub-Saharan Africa" ), values=c("#179270", "#988DCE", "#B91498","#FF8DAB","#1A7391","#8AB3E6","#2CC988"))+
                theme_bw() +
                theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(color="#757171",size = 0.5)) +
                
                theme(plot.title=element_text(size=15, 
                                               face="bold", 
                                               family="Arial",
                                               color="black",
                                               hjust=-0.9,
                                               vjust=1),
                    
                     plot.caption=element_text(size=9,hjust=-0.08,vjust=0.6,face="italic"),  # caption
                     axis.title.x=element_text(vjust=20.5, 
                                               hjust=1, 
                                               size=8,
                                               face='bold',
                                               color ="#757171"),
                                               
                                              # X axis title
                       # Y axis title
                     axis.text.x=element_text(size=9),  # X axis text
                     axis.text.y=element_text(size=9) )# Y axis text 
                gg2<-gg1+theme(legend.position="top",legend.direction = "horizontal") +
                guides(size = FALSE) +
                theme(legend.title = element_blank(),legend.box.margin = margin(0, 0, 0, -100))+
                scale_x_continuous(expand=c(0,0),breaks=c(500,1000,2000,5000,10000,20000,50000,100000),labels= c("$500","$1,000","$2,000","$5,000","$10,000","$20,000","$50,000","$100,000"),trans="log2",limits=c(500,128000))+scale_y_continuous(expand=c(0,0),limits=c(0,210)) +geom_hline(yintercept = 100,linetype=2,col="red")+geom_vline(xintercept = 16596,linetype=2,col="dark green") +
                
                annotate("text", x=14152 , y=92.83366 ,label="China", vjust=0.8, hjust=1.4,size=3,fontface='bold',col="#179270" )+annotate("text", x=112701 , y=143.06 ,label="Luxemberg", vjust=0.8, hjust=0.79,size=3,fontface='bold',col="#988DCE")+annotate("text", x=59984 , y=149.97 ,label="USA", vjust=-0.3, hjust=-0.3,size=3,fontface='bold',col="#1A7391" )+annotate("text", x=55492 , y=192.73 ,label="Iceland", vjust=1.2, hjust=1.2,size=3,fontface='bold',col="#988DCE" )+annotate("text", x=72356 , y=204.92 ,label="Bermuda", vjust=1, hjust=0.4,size=3,fontface='bold',col="#1A7391"  )+annotate("text", x=67139, y=179.82 ,label="Switzerland", vjust=0.2, hjust=-0.1,col="#988DCE",size=3,fontface='bold') +annotate("text", x=40827, y=140.89 ,label="Japan", vjust=0.2, hjust=1.3,size=3,fontface='bold',col="#179270")+annotate("text", x=59907, y=115.68 ,label="Hong kong", vjust=0.2, hjust=-0.2,size=3,fontface='bold',col="#179270")+annotate("text", x=93981, y=96.22,label="Singapore", vjust=1.2, hjust=0.5,size=3,fontface='bold',col="#179270")+annotate("text", x=48015, y=65.95,label="Saudi Arabia", vjust=1.6, hjust=0.5,size=3,fontface='bold',col="#FF8DAB")+annotate("text", x=6149, y=47.55,label="India", vjust=2.2, hjust=0.5,size=3,fontface='bold',col="#8AB3E6")+annotate("text", x=14941, y=165.19,label="Barbdos", vjust=0.7, hjust=1.2,size=3,fontface='bold',col="#B91498")+annotate("text", x=1877, y=67.11,label="Haiti", vjust=-0.5, hjust=0.5,size=3,fontface='bold',col="#B91498")+annotate("text", x=847, y=66.59,label="Niger", vjust=-0.1, hjust=1.2,size=3,fontface='bold',col="#2CC988")+annotate("text", x=784, y=56.80,label="Burundi", vjust=1.2, hjust=0.8,size=3,fontface='bold',col="#2CC988")+annotate("text", x=13327, y=27.45,label="Egypt", vjust=1.5, hjust=0.5,size=3,fontface='bold',col="#FF8DA8")
                
                gg2
                
                gg3<-gg2+geom_text (aes(x=16596, label="   Mean GDP per Capita\n= $16596  ",y=200), colour="#19617B", angle=0, vjust = 1.2,hjust=1.1 ,size=2)+ geom_text(aes(x=1000, label="GDP PLI:World = 100", y=100), colour="red", angle=0, vjust = -1.5,hjust=0.5 ,size=2)+theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank() ) + geom_text(aes(x=500, label="GDP Price Level \n Index           ", y=200), angle=0, vjust = 0.75,hjust=-0.05 ,size=3,color="#757171")+ guides(colour = guide_legend(override.aes = list(shape = 15,size = 4))) 
                
                     gg3
                
                
                
                
