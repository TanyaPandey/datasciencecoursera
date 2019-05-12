clean_it<-function(get_file){
        get_file[complete.cases(get_file),]
}

getid<-function(id){
        ndigits<-nchar( trunc( abs(id) ) )
        if (ndigits==3){
                ret_id<-(paste(id,".csv",sep = ""))
                return(gsub(" ","",ret_id,fixed=TRUE))
        }
        if(ndigits==2){
                ret_id<- return(paste("0",id,".csv",sep = ""))
                return(gsub(" ","",ret_id,fixed=TRUE))
        }
        if(ndigits==1){
                ret_id<-(paste("00",id,".csv",sep = ""))
                return(gsub(" ","",ret_id,fixed=TRUE))
        }
}

pollutantmean<-function(directory="specdata",pollutant,id=1:332){
        m<-vector('numeric')
        for(i in id){
                pollutant_file<-read.csv(getid(i))
                cleaned_polutant_file<-clean_it(pollutant_file)
                m<-c(m,mean(cleaned_polutant_file[,pollutant]))
        }
        mean(m,na.rm = TRUE)
        
}

complete<-function(directory,id=1:332){
        complete_matrix<-data.frame()
        for (i in id){
                n<-clean_it(read.csv(getid(i)))
                nobs<-nrow(n)
                c_matrix<-data.frame(i,nobs)
                complete_matrix<-rbind(complete_matrix,c_matrix)
        }
        complete_matrix
}

corr<-function(directory,threshold=0, id=1:332){
        c_val<-numeric()
        complete_matrix<-numeric()
        for (i in id){
                cleaned_data<-clean_it(read.csv(getid(i)))
                if (nrow(cleaned_data)>threshold)
                {
                        c_val<-cor(cleaned_data[,2],cleaned_data[,3])
                        
                }
                
                else next()
                
                complete_matrix<-cbind(complete_matrix,c_val)
        
        }
        if(length(complete_matrix)==0) {
                return()
        }
        else {
                return(c(complete_matrix))
                
        }
}