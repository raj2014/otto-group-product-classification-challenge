getensembleMatrix<-function()
{
  final<-data.frame(matrix(1:144368, nrow = 144368,ncol=1)) 
  #v<-c(0.1,0.15,0.25,0.25,0.25) #combination for V11
  v<-c(0.2,0.2,0.2,0.2,0.2)
  for(i in 1:5)
  {
    filename<-paste('ensemble3/','en_',i,'.csv',sep='')
    data<-read.csv(file=filename,header=TRUE)
    
    if(i==1)
    {
      final$Class_1<-data$Class_1*v[i]
      final$Class_2<-data$Class_2*v[i]
      final$Class_3<-data$Class_3*v[i]
      final$Class_4<-data$Class_4*v[i]
      final$Class_5<-data$Class_5*v[i]
      final$Class_6<-data$Class_6*v[i]
      final$Class_7<-data$Class_7*v[i]
      final$Class_8<-data$Class_8*v[i]
      final$Class_9<-data$Class_9*v[i]
      
    }
    else
    {
      final$Class_1<-final$Class_1+(data$Class_1)*v[i]
      final$Class_2<-final$Class_2+(data$Class_2)*v[i]
      final$Class_3<-final$Class_3+(data$Class_3)*v[i]
      final$Class_4<-final$Class_4+(data$Class_4)*v[i]
      final$Class_5<-final$Class_5+(data$Class_5)*v[i]
      final$Class_6<-final$Class_6+(data$Class_6)*v[i]
      final$Class_7<-final$Class_7+(data$Class_7)*v[i]
      final$Class_8<-final$Class_8+(data$Class_8)*v[i]
      final$Class_9<-final$Class_9+(data$Class_9)*v[i]
    }
    
    
    
  }
  
  return (final)
  
}