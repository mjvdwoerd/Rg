library(splines);library(MASS);library(changepoint);library(ggplot2)

shinyServer(function(input, output, session) {
  sliderValue <- reactive({input$slider1})
  RgRange <- reactive({input$RgRange})
  lowAngle <- reactive({input$lowAngle})
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$choice)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    Dataset <- read.table(input$choice$datapath, header=input$hdr, 
                          sep=input$sp,quote=input$qt)
    return(Dataset)
  })
  
  plotInput = function() {
    if(is.null(input$choice)){
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, 
           bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, 
           paste("Please upload an input file before you try to plot it."), 
           cex = 1.6, col = "black")
      return(NULL)
    }
    len<- length(Dataset()[,1])
    data <- as.matrix(Dataset()[1:len,],ncol=3)
    par(mfrow=c(2,1))
#    par(mar=c(3,3,3,3), oma=c(3,3,3,3))
    plot(data[,1],log(data[,2]),xlab="S",ylab="log(Intensity)",
         type=input$plotType)
#    plot(data[,1],data[,2],xlab="S",ylab="Intensity",type=input$plotType)
  }

  output$plot = renderPlot({
    name = paste0(input$filename,".png")
    if(input$saveplot){
      cat('Here now\n', file=stderr())
      png(filename = name)
      print(plotInput())
      dev.off()
    }
    else print(plotInput())
  }, height=700, width=600)
  
  output$downloadData <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      png(file, width = 1000, height = 800, units = "px", pointsize = 12, bg = "white", res = NA)
      plotInput()
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
    
  
#  output$plot <- renderPlot({
#    if(is.null(input$choice)){
#      par(mar = c(0,0,0,0))
#      plot(c(0, 1), c(0, 1), ann = F, 
#           bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
#      text(x = 0.5, y = 0.5, 
#           paste("Please upload an input file before you try to plot it."), 
#           cex = 1.6, col = "black")
#      return(NULL)
#    }
#    len<- length(Dataset()[,1])
#    data <- as.matrix(Dataset()[1:len,],ncol=3)
#    
#    plot(data[,1],log(data[,2]), xlab="S", ylab="log(Intensity)",
#         type=input$plotType)
#  })
  
  output$Analysis2 <- renderText({
    if(is.null(input$choice)){
      return(NULL)
    }
    
    len<- length(Dataset()[,1])
    M <- as.matrix(Dataset()[1:len,],ncol=3)
    #check numer of columns in input file
    num <- length(M[2,])
    if( num == 3 )
    {
    paste("Number of columns in input file is ", 
                  num,"(correct).")
    }
    else
    {
      "Incorrect number of data columns in input file."
    }
  })
  
  output$Analysis3 <- renderText({
    if(is.null(input$choice)){
      return(NULL)
    }
    
    len<- length(Dataset()[,1])
    M <- as.matrix(Dataset()[1:len,],ncol=3)
    #check for zero values in M[,2]
      first = TRUE
      if(M[1,2] == 0 && first == TRUE)
      {
        first = FALSE
        i = 1
        while(M[i+1,2] == 0 && i < length(M[,2]))
        {
        i = i + 1
        }
      num = i
     }
     if(first == FALSE)
     {
     temp_M = M[(num+1):length(M[,1]),]
     len = length(M[,1]) - num
     M = matrix(rep(0,(len)*3),ncol = 3)
     M[1:len,1] = temp_M[,1]
     M[1:len,2] = temp_M[,2]
     M[1:len,3] = temp_M[,3]
     n = len
     paste("Warning: first ", num, " intensity values equal zero. These values were stripped from the data.",sep="")
     }
    else
    {
      "No zero-intesity values detected in input file."
    }
  })
  
  output$Analysis4 <- renderText({
    if(is.null(input$choice)){
      return(NULL)
    }
      
    #check for negative values in M[,2]
    first = TRUE
    for(i in 1:length(M[,2]))
    {
      if(M[i,2] < 0 && first == TRUE)
      {
        n = i - 1
        first = FALSE
        "Warning: negative intensity values found."
      }
    }
    if(first == TRUE)
      {
      n = length(M[,2])
      "No negative intensities found."
      }
  })
    

   output$Guinier <- renderPlot({
#This is a fake Guinier plot

     len<- length(Dataset()[,1])
     M <- as.matrix(Dataset()[1:len,],ncol=3)
     first = TRUE
     if(M[1,2] == 0 && first == TRUE)
     {
       first = FALSE
       i = 1
       while(M[i+1,2] == 0 && i < length(M[,2]))
       {
         i = i + 1
       }
       num = i
     }
     if(first == FALSE)
     {
       temp_M = M[(num+1):length(M[,1]),]
       len = length(M[,1]) - num
       M = matrix(rep(0,(len)*3),ncol = 3)
       M[1:len,1] = temp_M[,1]
       M[1:len,2] = temp_M[,2]
       M[1:len,3] = temp_M[,3]
       n = len
#       paste("Warning: first ", num, " intensity values equal zero. These values were stripped from the data.",sep="")
     }

    X = M[1:30,1]^2
    Y = log(M[1:30,2])
    Begin <- c(X[1],Y[1])
    End <- c(X[30],Y[30])
    LineX <- c(X[sliderValue()],X[30])
    LineY <- c(Y[sliderValue()],Y[30])
    opt <- options("scipen"=5)
    plot(X,Y,xlab="S^2",ylab="log(Int)")
    options(opt)
    lines(LineX,LineY,type="l")
  })
  
  output$value <- renderPrint({ 
    #This is a fake Rg value
    Slope=(Y[30]-Y[sliderValue()])/(X[30]-X[sliderValue()])
    print(sqrt(-3*Slope)) })

  output$Cody <- renderPlot({
    
    len<- length(Dataset()[,1])
    M <- as.matrix(Dataset()[1:len,],ncol=3)
    len<- length(Dataset()[,1])
    M <- as.matrix(Dataset()[1:len,],ncol=3)
    first = TRUE
    if(M[1,2] == 0 && first == TRUE)
    {
      first = FALSE
      i = 1
      while(M[i+1,2] == 0 && i < length(M[,2]))
      {
        i = i + 1
      }
      num = i
    }
    if(first == FALSE)
    {
      temp_M = M[(num+1):length(M[,1]),]
      len = length(M[,1]) - num
      M = matrix(rep(0,(len)*3),ncol = 3)
      M[1:len,1] = temp_M[,1]
      M[1:len,2] = temp_M[,2]
      M[1:len,3] = temp_M[,3]
      n = len
      #       paste("Warning: first ", num, " intensity values equal zero. These values were stripped from the data.",sep="")
    }
    
     
    estimate_Rg1 = function(M)
    {
      
      #check for negative values in M[,2]
      first = TRUE
      for(i in 1:length(M[,2]))
      {
        if(M[i,2] < 0 && first == TRUE)
        {
          n = i - 1
          first = FALSE
        }
      }
      if(first == TRUE)
      {
        n = length(M[,2])
      }

      #check for correct concavity of intensity curve
      ##
        
      y3 = diff(diff(diff(log(M[1:n,2]))))
      v = cpt.var(y3,know.mean=TRUE,mu=0,
                  test.stat="Normal",method="PELT",
                  penalty="Manual",pen.value=7*log(n))
      cp2 = cpts(v)[1]

# **********      
      
      cp1 = lowAngle()
cat('lowAngle: ',cp1,'\n', file=stderr())
cat('highAngle: ',cp2,'\n', file=stderr())
      
      output = fit_cubic_splines(M[lowAngle():n,1],M[lowAngle():n,2],M[lowAngle():n,3],cp1,cp2)
      
      Rg=output[1]
      seRg=output[2]
      t=output[3]
      line=output[4:(4+t-1)]

#      n = length(M[,1])
      
#      xspace = (max(M[lowAngle():RgRange(),1]^2)-min(M[lowAngle():RgRange(),1]^2))/10
#      yspace = (max(log(M[lowAngle():RgRange(),2]))-min(log(M[lowAngle():RgRange(),2])))/100
#      cat('xspace: ', xspace, '\n',file=stderr())
#      cat('yspace: ', yspace, '\n',file=stderr())
#      xspace = (max(d1[lowAngle():RgRange()]^2)-min(d1[lowAngle():RgRange()]^2))/10
#      yspace = (max(log(d2[lowAngle():RgRange()]))-min(log(d2[lowAngle():RgRange()])))/100
#      
#      plot(M[(t+1):cp2,1]^2,log(M[(t+1):cp2],2),xlim=c(min(M[1:cp2,1]^2),max(M[1:cp2,1]^2)),ylim=c(min(log(M[1:cp2,2])),max(log(M[1:cp2,2]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
#      plot(M[(t+1):RgRange(),1]^2,log(M[(t+1):RgRange(),2]),xlim=c(min(M[lowAngle():RgRange(),1]^2),max(M[lowAngle():RgRange(),1]^2)),ylim=c(min(log(M[lowAngle():RgRange(),2])),max(log(M[lowAngle():RgRange(),2]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
#      points(M[lowAngle():t,1]^2,log(M[lowAngle():t,2]),col="red",pch=20,cex=.5)  
#      lines(M[lowAngle():t,1]^2,line,lwd=2)
#      legend("topright", c("Data points used to fit curve","Fitted curve"),cex=.8,pch=c(20,NA), col=c("red","black"), lty=0:1);
#      rg = round(output[1],1) ;text((M[lowAngle(),1]^2+xspace),(log(M[RgRange(),2])+yspace*15),bquote(hat(R[g]) == .(rg)))
#      serg = round(output[2],2) ; text((M[lowAngle(),1]^2+xspace*1.9),(log(M[RgRange(),2])+yspace*9.8),paste("Std. Deviation = ",serg,sep=""))     
#      cat('Output: ', output, '\n',file=stderr())
    
    }
    
    fit_cubic_splines = function(d1,d2,d3,cp1,cp2)
    {
      if(cp2 < 30)
      {
        cp2 = 30
      }
      
      #initialize variables
      f = rep(10000000,cp2)
      var_est = rep(10000000,cp2)
      int_bias2_avg = rep(10000000,cp2)
      sum_bias2_avg = rep(10000000,cp2)
      output = c(0,0)
      
      #set number of knots for cubic spline fit of data
      df = 8
      
      #rescale s to start at angle 0
      s = d1
      delta.s=mean(diff(s))
      s = s - s[1]+delta.s 
      
      #first fit data by ordinary least squares
      Y_beta = log(d2[1:cp2])
      z = s[1:cp2]
      fit_0 = lm( Y_beta ~ ns(z,df))
      
      #estimate gamma_n by fitting AR to resids
      resid_0 = summary(fit_0)$resid
      ar_0 = ar(resid_0,order.max=5)
      
      #extract ar data and put into variable a_0
      if(ar_0$order == 0)
      {
        phi_temp = 0
      }
      if(ar_0$order > 0)
      {
        phi_temp = ar_0$ar
      }
      a_0 = list(phi = phi_temp, theta = c(0), sigma2 = ar_0$var.pred)
      
      #create autocovariance vector
      g_0 = unlist(ARMAacf(a_0$phi,lag.max = cp2)*ar_0$var.pred)
      
      #create correlation matrix
      gamma_0 = create_gamma_matrix(g_0,cp2)
      
      #fit model again using WLS
      X = cbind(rep(1,cp2),ns(z,df))  
      fit_1 = lm.gls(Y_beta~X-1, W = gamma_0,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_1 = fit_1$residuals
      ar_1 = ar(resid_1,order.max=5)
      
      #extract ar data and put into variable a_1
      if(ar_1$order == 0)
      {
        phi_temp = 0
      }
      if(ar_1$order > 0)
      {
        phi_temp = ar_1$ar
      }
      a_1 = list(phi = phi_temp, theta = c(0), sigma2 = ar_1$var.pred)
      
      #create autocovariance vector
      g_1 = unlist(ARMAacf(a_1$phi,lag.max = cp2)*ar_1$var.pred)
      
      #create correlation matrix
      gamma_1 = create_gamma_matrix(g_1,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_2=lm.gls(Y_beta~X-1, W = gamma_1,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_2 = fit_2$residuals
      ar_2 = ar(resid_2,order.max=5)
      
      #extract ar data and put into variable a_2
      if(ar_2$order == 0)
      {
        phi_temp = 0
      }
      if(ar_2$order > 0)
      {
        phi_temp = ar_2$ar
      }
      a_2 = list(phi = phi_temp, theta = c(0), sigma2 = ar_2$var.pred)
      
      
      #create autocovariance vector
      g_2 = unlist(ARMAacf(a_2$phi,lag.max = cp2)*ar_2$var.pred)
      
      #create correlation matrix
      gamma_2 = create_gamma_matrix(g_2,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_3 = lm.gls(Y_beta~X-1, W = gamma_2,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_3 = fit_3$residuals
      ar_3 = ar(resid_3,order.max=5)
      
      #extract ar data and put into variable a_3
      if(ar_3$order == 0)
      {
        phi_temp = 0
      }
      if(ar_3$order > 0)
      {
        phi_temp = ar_3$ar
      }
      a_3 = list(phi = phi_temp, theta = c(0), sigma2 = ar_3$var.pred)
      
      #create autocovariance vector
      g_3 = unlist(ARMAacf(a_3$phi,lag.max = cp2)*ar_3$var.pred)
      
      #create correlation matrix
      gamma_3 = create_gamma_matrix(g_3,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_4 = lm.gls(Y_beta~X-1, W = gamma_3,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_4 = fit_4$residuals
      ar_4 = ar(resid_4,order.max=5)
      
      #extract ar data and put into variable a_4
      if(ar_4$order == 0)
      {
        phi_temp = 0
      }
      if(ar_4$order > 0)
      {
        phi_temp = ar_4$ar
      }
      a_4 = list(phi = phi_temp, theta = c(0), sigma2 = ar_4$var.pred)
          
      #create autocovariance vector
      g_4 = unlist(ARMAacf(a_4$phi,lag.max = cp2)*ar_4$var.pred)
      
      #create correlation matrix
      gamma_4 = create_gamma_matrix(g_4,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_5 = lm.gls(Y_beta~X-1, W = gamma_4,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_5 = fit_5$residuals
      ar_5 = ar(resid_5,order.max=5)
      
      #extract ar data and put into variable a_5
      if(ar_5$order == 0)
      {
        phi_temp = 0
      }
      if(ar_5$order > 0)
      {
        phi_temp = ar_5$ar
      }
      a_5 = list(phi = phi_temp, theta = c(0), sigma2 = ar_5$var.pred)
         
      #create autocovariance vector
      g_5 = unlist(ARMAacf(a_5$phi,lag.max = cp2)*ar_5$var.pred)
      
      #create correlation matrix
      gamma_5 = create_gamma_matrix(g_5,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_6 = lm.gls(Y_beta~X-1, W = gamma_5,inverse="T")
      
      #extract parameter estimates from final fit of the data
      beta_est = fit_6$coefficients
      b = beta_est
      
      #final estimates for the autoregressive structure 
      #of the residuals 
      nsp = ns(z,df)
      gamma_est = gamma_5
      a_5_0 = a_5
      
      #determine optimum bias-variance criterion change point
      t = b_v_tradeoff(delta.s,d2,s,cp2,gamma_est,nsp,b)

# ********
      cat('t: ',t,'\n',file=stderr())
          
      #for the final estimate of Rg and its variance use only the data
      #specified by the bias-variance criterion
      gamma_est = gamma_est[lowAngle():t,lowAngle():t]
      s = d1
      Y = log(d2[lowAngle():t])
#      X = cbind(rep(lowAngle(),t),s[lowAngle():t]^2)
      X = cbind(rep(1,t-lowAngle()+1),s[lowAngle():t]^2)
      
      #estimate Rg using standard regression technique
      alpha = solve(t(X) %*% solve(gamma_est) %*% X) %*% t(X) %*% solve(gamma_est) %*% Y  
      
      #check for negative Rg
      if( alpha[2] > 0 )
      {
        stop("Negative Rg value found. Program stopped.")
      }
      
      output[1] = sqrt(-3*alpha[2])
      
      #calculate the variance of Rg^2 hat using standard regression 
      #technique
      var_alpha = solve(t(X) %*% solve(gamma_est) %*% X)
      
      #approximate the variance of Rg hat using Taylor linearization
      output[2]= sqrt(-3/(4*alpha[2])*var_alpha[2,2])
      
      #store final Rg estimate in Rg_0
      Rg_0 = output[1]
      
      #store final fit of the log intensity curve in beta_curve
      beta_curve = b[1]+b[2]*nsp[1:cp2,1]+
                  b[3]*nsp[1:cp2,2]+b[4]*nsp[1:cp2,3]+
                  b[5]*nsp[1:cp2,4]+b[6]*nsp[1:cp2,5]+
                  b[7]*nsp[1:cp2,6]+b[8]*nsp[1:cp2,7]+
                  b[9]*nsp[1:cp2,8]
      
      #calculate bootstrap variance using 100 iterations
      N = 2
      Rg = rep(0,N)
      output_matrix = matrix(rep(0,(10+cp2-1)*(N+1)),nrow=(N+1))
      for(w in 1:N)
      {
        #simulate new intensity curve from AR structure
        I_new = simulate_noise(sig2,a_5_0,beta_curve,cp2,ar_5)
        
        #estimate Rg from the simulated intensity curve
        output_matrix[w+1,] = est_cor_iter(d1,I_new,s,cp2,delta.s)
        Rg[w] = output_matrix[w+1,1]
      }
      
      #output bootstrap variance
      output[3] = var(Rg)
      
      #bias-variance criterion change point
      output[4] = t
      
      #line for plotting
      line = X%*%alpha
#      cat('X ', X, '\n', file=stderr())
#      cat('Alpha:', alpha, '\n', file=stderr())
#      cat('Product:', line, '\n', file=stderr())
#      
      n = length(d1)
#      
#      xspace = (max(d1[lowAngle():RgRange()]^2)-min(d1[lowAngle():RgRange()]^2))/10
#      yspace = (max(log(d2[lowAngle():RgRange()]))-min(log(d2[lowAngle():RgRange()])))/100
#      plot(d1[(t+1):cp2]^2,log(d2[(t+1):cp2]),xlim=c(min(d1[1:cp2]^2),max(d1[1:cp2]^2)),ylim=c(min(log(d2[1:cp2])),max(log(d2[1:cp2]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
#      plot(d1[(t+1):RgRange()]^2,log(d2[(t+1):RgRange()]),xlim=c(min(d1[lowAngle():RgRange()]^2),max(d1[lowAngle():RgRange()]^2)),ylim=c(min(log(d2[lowAngle():RgRange()])),max(log(d2[lowAngle():RgRange()]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
#      points(d1[lowAngle():t]^2,log(d2[lowAngle():t]),col="red",pch=20,cex=.5)  
#      lines(d1[lowAngle():t]^2,line,lwd=2)
#      legend("topright", c("Data points used to fit curve","Fitted curve"),cex=.8,pch=c(20,NA), col=c("red","black"), lty=0:1);
#      rg = round(output[1],1) ;text((d1[lowAngle()]^2+xspace),(log(d2[RgRange()])+yspace*15),bquote(hat(R[g]) == .(rg)))
#      serg = round(output[2],2) ; text((d1[lowAngle()]^2+xspace*1.9),(log(d2[RgRange()])+yspace*9.8),paste("Std. Deviation = ",serg,sep=""))
      
      #      windows()
#      dev.copy(png,'test2.png')
      rg = round(output[1],1)
      serg = round(output[2],2)
      xspace = (max(d1)-min(d1))/10
      yspace = (max(log(d2))-min(log(d2)))/100

      plot(d1[-(lowAngle():t)],log(d2[-(lowAngle():t)]),xlim=c(min(d1),max(d1)),ylim=c(min(log(d2)),max(log(d2))),xlab="S",ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
      lines(d1[lowAngle():t],line)
      text((d1[n]-xspace*4),max(log(d2[c(1,n)])-yspace*20),bquote(hat(R[g]) == .(rg)))
      text((d1[n]-xspace*3.1),(max(log(d2[c(1,n)]))-yspace*26),paste("Std. Deviation = ",serg,sep=""))
      points(d1[lowAngle():t],log(d2[lowAngle():t]),col="red",pch=20,cex=.5)
      lines(d1[lowAngle():t],line,lwd=1.7)
      legend("topright", c("Data points used to fit curve","Fitted curve","Bias-variance criterion range"),cex=.8,pch=c(20,NA,NA), col=c("red","black","black"), lty=0:2);
      lines(d1[lowAngle():cp2],rep(max(log(d2))+.2,(cp2-lowAngle()+1)),lwd=2,lt=2)

#      dev.off()

      #return output data
      return(c(output[1],output[2],t,line))

    }
    
    create_gamma_matrix = function(g,p)
    {
      x=rep(0,p^2)
      gamma=matrix(x,p)
      for( i in 1:p )
      {
        for( j in 1:p )
        {
          gamma[i,j] = g[abs(i-j)+1]
        }
      }
      return(gamma)
    }
    
    simulate_noise = function(sig2,a_5_0,beta_curve,cp2,ar_5)
    {
      noise=rep(0,cp2)
      
      #store variance
      level = sqrt(a_5_0$sigma2)
      
      #simulate from only white noise if AR order is 0
      if(ar_5$order == 0)
      {
        for(i in 1:cp2){
          noise[i] = rnorm(1,0,level)
        }
      }
      
      #simulate from AR structure of final residuals
      if(ar_5$order > 0)
      {
        for(i in 1:ar_5$order){
          noise[i] =  rnorm(1,0,level)
        }
        for(i in (ar_5$order+1):cp2){
          noise[i] = sum(a_5_0$phi*rev(noise[(i-ar_5$order):(i-1)])) + rnorm(1,0,level)
        }
      }
      
      #new simulated intensity curve
      I = exp(beta_curve + noise)
      
      return(I)
    }
    
    est_cor_iter = function(d1,d2,s,cp2,delta.s)
    {
      df = 8
      
      #first fit data by ordinary least squares
#  ************
      Y_beta = log(d2[1:cp2])
      z = s[1:cp2]
      fit_0 = lm( Y_beta ~ ns(z,df))
      
      #estimate gamma_n by fitting AR to resids
      resid_0 = summary(fit_0)$resid
      ar_0 = ar(resid_0,order.max=5)
      
      #extract ar data and put into variable a_0
      if(ar_0$order == 0)
      {
        phi_temp = 0
      }
      if(ar_0$order > 0)
      {
        phi_temp = ar_0$ar
      }
      a_0 = list(phi = phi_temp, theta = c(0), sigma2 = ar_0$var.pred)
      
      #create autocovariance vector
      g_0 = unlist(ARMAacf(a_0$phi,lag.max = cp2)*ar_0$var.pred)
      
      #create correlation matrix
      gamma_0 = create_gamma_matrix(g_0,cp2)
      
      #fit model again using WLS
      X = cbind(rep(1,cp2),ns(z,df))  
      fit_1 = lm.gls(Y_beta~X-1, W = gamma_0,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_1 = fit_1$residuals
      ar_1 = ar(resid_1,order.max=5)
      
      #extract ar data and put into variable a_1
      if(ar_1$order == 0)
      {
        phi_temp = 0
      }
      if(ar_1$order > 0)
      {
        phi_temp = ar_1$ar
      }
      a_1 = list(phi = phi_temp, theta = c(0), sigma2 = ar_1$var.pred)
      
      #create autocovariance vector
      g_1 = unlist(ARMAacf(a_1$phi,lag.max = cp2)*ar_1$var.pred)
      
      #create correlation matrix
      gamma_1 = create_gamma_matrix(g_1,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_2=lm.gls(Y_beta~X-1, W = gamma_1,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_2 = fit_2$residuals
      ar_2 = ar(resid_2,order.max=5)
      
      #extract ar data and put into variable a_2
      if(ar_2$order == 0)
      {
        phi_temp = 0
      }
      if(ar_2$order > 0)
      {
        phi_temp = ar_2$ar
      }
      a_2 = list(phi = phi_temp, theta = c(0), sigma2 = ar_2$var.pred)
      
      #create autocovariance vector
      g_2 = unlist(ARMAacf(a_2$phi,lag.max = cp2)*ar_2$var.pred)
      
      #create correlation matrix
      gamma_2 = create_gamma_matrix(g_2,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_3 = lm.gls(Y_beta~X-1, W = gamma_2,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_3 = fit_3$residuals
      ar_3 = ar(resid_3,order.max=5)
      
      #extract ar data and put into variable a_3
      if(ar_3$order == 0)
      {
        phi_temp = 0
      }
      if(ar_3$order > 0)
      {
        phi_temp = ar_3$ar
      }
      a_3 = list(phi = phi_temp, theta = c(0), sigma2 = ar_3$var.pred)
      
      #create autocovariance vector
      g_3 = unlist(ARMAacf(a_3$phi,lag.max = cp2)*ar_3$var.pred)
      
      #create correlation matrix
      gamma_3 = create_gamma_matrix(g_3,cp2)  
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_4 = lm.gls(Y_beta~X-1, W = gamma_3,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_4 = fit_4$residuals
      ar_4 = ar(resid_4,order.max=5)
      
      #extract ar data and put into variable a_4
      if(ar_4$order == 0)
      {
        phi_temp = 0
      }
      if(ar_4$order > 0)
      {
        phi_temp = ar_4$ar
      }
      a_4 = list(phi = phi_temp, theta = c(0), sigma2 = ar_4$var.pred)
      
      #create autocovariance vector
      g_4 = unlist(ARMAacf(a_4$phi,lag.max = cp2)*ar_4$var.pred)
      
      #create correlation matrix
      gamma_4 = create_gamma_matrix(g_4,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_5 = lm.gls(Y_beta~X-1, W = gamma_4,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_5 = fit_5$residuals
      ar_5 = ar(resid_5,order.max=5)
      
      #extract ar data and put into variable a_5
      if(ar_5$order == 0)
      {
        phi_temp = 0
      }
      if(ar_5$order > 0)
      {
        phi_temp = ar_5$ar
      }
      a_5 = list(phi = phi_temp, theta = c(0), sigma2 = ar_5$var.pred)
      
      #create autocovariance vector
      g_5 = unlist(ARMAacf(a_5$phi,lag.max = cp2)*ar_5$var.pred)
      
      #create correlation matrix
      gamma_5 = create_gamma_matrix(g_5,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_6 = lm.gls(Y_beta~X-1, W = gamma_5,inverse="T")
      
      #extract parameter estimates from final fit of the data
      beta_est = fit_6$coefficients
      b = beta_est
      
      #final estimates for the autoregressive structure 
      #of the residuals 
      nsp = ns(z,df)
      gamma_est = gamma_5
      a_5_0 = a_5
      
      #determine optimum bias-variance criterion change point
      t = b_v_tradeoff(delta.s,d2,s,cp2,gamma_est,nsp,b)
      
      #final gamma matrix
      gamma_est = gamma_est[1:t,1:t]
      
      #calculate Rg estimate using bias-variance change point
      s = d1
      Y = log(d2[1:t])
      X = cbind(rep(1,t),s[1:t]^2)
      
      alpha = solve(t(X) %*% solve(gamma_est) %*% X) %*% t(X) %*% solve(gamma_est) %*% Y  
      out1 = sqrt(-3*alpha[2])
      
      out = c(out1,0,t,cp2,0,0,0,0,a_5$sigma2,d2[1:cp2])
      return(out)
    }
######    


b_v_tradeoff = function(delta.s,d2,s,cp2,gamma_est,nsp,b)
{
  #initialize variables
  f = rep(10000000,cp2)
  var_est = rep(10000000,cp2)
  sum_bias2_avg = rep(10000000,cp2)
  
  #loop though data up to cp2
  for(k in (14+lowAngle()):cp2)
  {
    #fit quadratic curve over data
    Y = log(d2[lowAngle():k])
    X = cbind(rep(1,k-lowAngle()+1),s[lowAngle():k]^2)
    alpha = solve(t(X) %*% solve(gamma_est[lowAngle():k,lowAngle():k]) %*% X) %*% t(X) %*% solve(gamma_est[lowAngle():k,lowAngle():k]) %*% Y
    
    #calculate the average bias
    sum_bias2_avg[k] = 1/k*sum((alpha[1]+alpha[2]*s[lowAngle():k]^2-(b[1]+b[2]*nsp[lowAngle():k,1]+b[3]*nsp[lowAngle():k,2]+b[4]*nsp[lowAngle():k,3]+b[5]*nsp[lowAngle():k,4]+b[6]*nsp[lowAngle():k,5]+b[7]*nsp[lowAngle():k,6]+b[8]*nsp[lowAngle():k,7]+b[9]*nsp[lowAngle():k,8]))^2)
    
    #calculate scaled variance of Rg
    a = cbind(0,-3) 
    var_est[k] =  (k*delta.s)^4 * a %*% solve(t(X) %*% X) %*% t(X) %*% gamma_est[lowAngle():k,lowAngle():k] %*% X %*% solve(t(X) %*% X) %*% t(a)   
    
    #bias-variance criterion
    f[k] = var_est[k]+sum_bias2_avg[k]
  }
  
  f = var_est+sum_bias2_avg
  
  #calculate minimum of bias-variance criterion
  t = which(f == min(f))
  return(t)
}


######
#    b_v_tradeoff = function(delta.s,d2,s,cp2,gamma_est,nsp,b)
#    {
      #initialize variables
#      f = rep(10000000,cp2)
#      var_est = rep(10000000,cp2)
#      sum_bias2_avg = rep(10000000,cp2)
      
      #loop though data up to cp2
#      for(k in 25:cp2)
#      {
        #fit quadratic curve over data
#        Y = log(d2[1:k])
#        X = cbind(rep(1,k),s[1:k]^2)
#        alpha = solve(t(X) %*% solve(gamma_est[1:k,1:k]) %*% X) %*% t(X) %*% solve(gamma_est[1:k,1:k]) %*% Y
#        
        #calculate the average bias
#        sum_bias2_avg[k] = 1/k*sum((alpha[1]+alpha[2]*s[1:k]^2-(b[1]+b[2]*nsp[1:k,1]+b[3]*nsp[1:k,2]+b[4]*nsp[1:k,3]+b[5]*nsp[1:k,4]+b[6]*nsp[1:k,5]+b[7]*nsp[1:k,6]+b[8]*nsp[1:k,7]+b[9]*nsp[1:k,8]))^2)
#        
        #calculate scaled variance of Rg
#        a = cbind(0,-3) 
#        var_est[k] =  (k*delta.s)^4 * a %*% solve(t(X) %*% X) %*% t(X) %*% gamma_est[1:k,1:k] %*% X %*% solve(t(X) %*% X) %*% t(a)   
        
        #bias-variance criterion
#        f[k] = var_est[k]+sum_bias2_avg[k]
#      }
      
      #smooth bias data
#      bias2 = c(rep(1000000,24),(supsmu(25:cp2,sum_bias2_avg[25:cp2]))$y)
      
#      f = var_est + bias2
#      f = var_est+sum_bias2_avg
      
      #calculate minimum of bias-variance criterion
#      t = which(f == min(f))
#      return(t)
#    }
    
     estimate_Rg1(M)
    
  })#end output$Cody

  output$plotRg <- renderPlot({
    len<- length(Dataset()[,1])
    M <- as.matrix(Dataset()[1:len,],ncol=3)
    len<- length(Dataset()[,1])
    M <- as.matrix(Dataset()[1:len,],ncol=3)
    first = TRUE
    if(M[1,2] == 0 && first == TRUE)
    {
      first = FALSE
      i = 1
      while(M[i+1,2] == 0 && i < length(M[,2]))
      {
        i = i + 1
      }
      num = i
    }
    if(first == FALSE)
    {
      temp_M = M[(num+1):length(M[,1]),]
      len = length(M[,1]) - num
      M = matrix(rep(0,(len)*3),ncol = 3)
      M[1:len,1] = temp_M[,1]
      M[1:len,2] = temp_M[,2]
      M[1:len,3] = temp_M[,3]
      n = len
      #       paste("Warning: first ", num, " intensity values equal zero. These values were stripped from the data.",sep="")
    }
    
    
    estimate_Rg2 = function(M)
    {
      
      #check for negative values in M[,2]
      first = TRUE
      for(i in 1:length(M[,2]))
      {
        if(M[i,2] < 0 && first == TRUE)
        {
          n = i - 1
          first = FALSE
        }
      }
      if(first == TRUE)
      {
        n = length(M[,2])
      }
      
      #check for correct concavity of intensity curve
      ##
      
      y3 = diff(diff(diff(log(M[1:n,2]))))
      v = cpt.var(y3,know.mean=TRUE,mu=0,
                  test.stat="Normal",method="PELT",
                  penalty="Manual",pen.value=7*log(n))
      cp2 = cpts(v)[1]
      
      # **********      
      
      cp1 = lowAngle()
      cat('lowAngle: ',cp1,'\n', file=stderr())
      cat('highAngle: ',cp2,'\n', file=stderr())
      
      output = fit_cubic_splines(M[lowAngle():n,1],M[lowAngle():n,2],M[lowAngle():n,3],cp1,cp2)
      
      Rg=output[1]
      seRg=output[2]
      t=output[3]
      line=output[4:(4+t-1)]
      
      #      n = length(M[,1])
      
      #      xspace = (max(M[lowAngle():RgRange(),1]^2)-min(M[lowAngle():RgRange(),1]^2))/10
      #      yspace = (max(log(M[lowAngle():RgRange(),2]))-min(log(M[lowAngle():RgRange(),2])))/100
      #      cat('xspace: ', xspace, '\n',file=stderr())
      #      cat('yspace: ', yspace, '\n',file=stderr())
      #      xspace = (max(d1[lowAngle():RgRange()]^2)-min(d1[lowAngle():RgRange()]^2))/10
      #      yspace = (max(log(d2[lowAngle():RgRange()]))-min(log(d2[lowAngle():RgRange()])))/100
      #      
      #      plot(M[(t+1):cp2,1]^2,log(M[(t+1):cp2],2),xlim=c(min(M[1:cp2,1]^2),max(M[1:cp2,1]^2)),ylim=c(min(log(M[1:cp2,2])),max(log(M[1:cp2,2]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
      #      plot(M[(t+1):RgRange(),1]^2,log(M[(t+1):RgRange(),2]),xlim=c(min(M[lowAngle():RgRange(),1]^2),max(M[lowAngle():RgRange(),1]^2)),ylim=c(min(log(M[lowAngle():RgRange(),2])),max(log(M[lowAngle():RgRange(),2]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
      #      points(M[lowAngle():t,1]^2,log(M[lowAngle():t,2]),col="red",pch=20,cex=.5)  
      #      lines(M[lowAngle():t,1]^2,line,lwd=2)
      #      legend("topright", c("Data points used to fit curve","Fitted curve"),cex=.8,pch=c(20,NA), col=c("red","black"), lty=0:1);
      #      rg = round(output[1],1) ;text((M[lowAngle(),1]^2+xspace),(log(M[RgRange(),2])+yspace*15),bquote(hat(R[g]) == .(rg)))
      #      serg = round(output[2],2) ; text((M[lowAngle(),1]^2+xspace*1.9),(log(M[RgRange(),2])+yspace*9.8),paste("Std. Deviation = ",serg,sep=""))     
      #      cat('Output: ', output, '\n',file=stderr())
      
    }
    
    fit_cubic_splines = function(d1,d2,d3,cp1,cp2)
    {
      if(cp2 < 30)
      {
        cp2 = 30
      }
      
      #initialize variables
      f = rep(10000000,cp2)
      var_est = rep(10000000,cp2)
      int_bias2_avg = rep(10000000,cp2)
      sum_bias2_avg = rep(10000000,cp2)
      output = c(0,0)
      
      #set number of knots for cubic spline fit of data
      df = 8
      
      #rescale s to start at angle 0
      s = d1
      delta.s=mean(diff(s))
      s = s - s[1]+delta.s 
      
      #first fit data by ordinary least squares
      Y_beta = log(d2[1:cp2])
      z = s[1:cp2]
      fit_0 = lm( Y_beta ~ ns(z,df))
      
      #estimate gamma_n by fitting AR to resids
      resid_0 = summary(fit_0)$resid
      ar_0 = ar(resid_0,order.max=5)
      
      #extract ar data and put into variable a_0
      if(ar_0$order == 0)
      {
        phi_temp = 0
      }
      if(ar_0$order > 0)
      {
        phi_temp = ar_0$ar
      }
      a_0 = list(phi = phi_temp, theta = c(0), sigma2 = ar_0$var.pred)
      
      #create autocovariance vector
      g_0 = unlist(ARMAacf(a_0$phi,lag.max = cp2)*ar_0$var.pred)
      
      #create correlation matrix
      gamma_0 = create_gamma_matrix(g_0,cp2)
      
      #fit model again using WLS
      X = cbind(rep(1,cp2),ns(z,df))  
      fit_1 = lm.gls(Y_beta~X-1, W = gamma_0,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_1 = fit_1$residuals
      ar_1 = ar(resid_1,order.max=5)
      
      #extract ar data and put into variable a_1
      if(ar_1$order == 0)
      {
        phi_temp = 0
      }
      if(ar_1$order > 0)
      {
        phi_temp = ar_1$ar
      }
      a_1 = list(phi = phi_temp, theta = c(0), sigma2 = ar_1$var.pred)
      
      #create autocovariance vector
      g_1 = unlist(ARMAacf(a_1$phi,lag.max = cp2)*ar_1$var.pred)
      
      #create correlation matrix
      gamma_1 = create_gamma_matrix(g_1,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_2=lm.gls(Y_beta~X-1, W = gamma_1,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_2 = fit_2$residuals
      ar_2 = ar(resid_2,order.max=5)
      
      #extract ar data and put into variable a_2
      if(ar_2$order == 0)
      {
        phi_temp = 0
      }
      if(ar_2$order > 0)
      {
        phi_temp = ar_2$ar
      }
      a_2 = list(phi = phi_temp, theta = c(0), sigma2 = ar_2$var.pred)
      
      
      #create autocovariance vector
      g_2 = unlist(ARMAacf(a_2$phi,lag.max = cp2)*ar_2$var.pred)
      
      #create correlation matrix
      gamma_2 = create_gamma_matrix(g_2,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_3 = lm.gls(Y_beta~X-1, W = gamma_2,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_3 = fit_3$residuals
      ar_3 = ar(resid_3,order.max=5)
      
      #extract ar data and put into variable a_3
      if(ar_3$order == 0)
      {
        phi_temp = 0
      }
      if(ar_3$order > 0)
      {
        phi_temp = ar_3$ar
      }
      a_3 = list(phi = phi_temp, theta = c(0), sigma2 = ar_3$var.pred)
      
      #create autocovariance vector
      g_3 = unlist(ARMAacf(a_3$phi,lag.max = cp2)*ar_3$var.pred)
      
      #create correlation matrix
      gamma_3 = create_gamma_matrix(g_3,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_4 = lm.gls(Y_beta~X-1, W = gamma_3,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_4 = fit_4$residuals
      ar_4 = ar(resid_4,order.max=5)
      
      #extract ar data and put into variable a_4
      if(ar_4$order == 0)
      {
        phi_temp = 0
      }
      if(ar_4$order > 0)
      {
        phi_temp = ar_4$ar
      }
      a_4 = list(phi = phi_temp, theta = c(0), sigma2 = ar_4$var.pred)
      
      #create autocovariance vector
      g_4 = unlist(ARMAacf(a_4$phi,lag.max = cp2)*ar_4$var.pred)
      
      #create correlation matrix
      gamma_4 = create_gamma_matrix(g_4,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_5 = lm.gls(Y_beta~X-1, W = gamma_4,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_5 = fit_5$residuals
      ar_5 = ar(resid_5,order.max=5)
      
      #extract ar data and put into variable a_5
      if(ar_5$order == 0)
      {
        phi_temp = 0
      }
      if(ar_5$order > 0)
      {
        phi_temp = ar_5$ar
      }
      a_5 = list(phi = phi_temp, theta = c(0), sigma2 = ar_5$var.pred)
      
      #create autocovariance vector
      g_5 = unlist(ARMAacf(a_5$phi,lag.max = cp2)*ar_5$var.pred)
      
      #create correlation matrix
      gamma_5 = create_gamma_matrix(g_5,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_6 = lm.gls(Y_beta~X-1, W = gamma_5,inverse="T")
      
      #extract parameter estimates from final fit of the data
      beta_est = fit_6$coefficients
      b = beta_est
      
      #final estimates for the autoregressive structure 
      #of the residuals 
      nsp = ns(z,df)
      gamma_est = gamma_5
      a_5_0 = a_5
      
      #determine optimum bias-variance criterion change point
      t = b_v_tradeoff(delta.s,d2,s,cp2,gamma_est,nsp,b)
      
      # ********
      cat('t: ',t,'\n',file=stderr())
      
      #for the final estimate of Rg and its variance use only the data
      #specified by the bias-variance criterion
      gamma_est = gamma_est[lowAngle():t,lowAngle():t]
      s = d1
      Y = log(d2[lowAngle():t])
      #      X = cbind(rep(lowAngle(),t),s[lowAngle():t]^2)
      X = cbind(rep(1,t-lowAngle()+1),s[lowAngle():t]^2)
      
      #estimate Rg using standard regression technique
      alpha = solve(t(X) %*% solve(gamma_est) %*% X) %*% t(X) %*% solve(gamma_est) %*% Y  
      
      #check for negative Rg
      if( alpha[2] > 0 )
      {
        stop("Negative Rg value found. Program stopped.")
      }
      
      output[1] = sqrt(-3*alpha[2])
      
      #calculate the variance of Rg^2 hat using standard regression 
      #technique
      var_alpha = solve(t(X) %*% solve(gamma_est) %*% X)
      
      #approximate the variance of Rg hat using Taylor linearization
      output[2]= sqrt(-3/(4*alpha[2])*var_alpha[2,2])
      
      #store final Rg estimate in Rg_0
      Rg_0 = output[1]
      
      #store final fit of the log intensity curve in beta_curve
      beta_curve = b[1]+b[2]*nsp[1:cp2,1]+
        b[3]*nsp[1:cp2,2]+b[4]*nsp[1:cp2,3]+
        b[5]*nsp[1:cp2,4]+b[6]*nsp[1:cp2,5]+
        b[7]*nsp[1:cp2,6]+b[8]*nsp[1:cp2,7]+
        b[9]*nsp[1:cp2,8]
      
      #calculate bootstrap variance using 100 iterations
      N = 2
      Rg = rep(0,N)
      output_matrix = matrix(rep(0,(10+cp2-1)*(N+1)),nrow=(N+1))
      for(w in 1:N)
      {
        #simulate new intensity curve from AR structure
        I_new = simulate_noise(sig2,a_5_0,beta_curve,cp2,ar_5)
        
        #estimate Rg from the simulated intensity curve
        output_matrix[w+1,] = est_cor_iter(d1,I_new,s,cp2,delta.s)
        Rg[w] = output_matrix[w+1,1]
      }
      
      #output bootstrap variance
      output[3] = var(Rg)
      
      #bias-variance criterion change point
      output[4] = t
      
      #line for plotting
      line = X%*%alpha
      #      cat('X ', X, '\n', file=stderr())
      #      cat('Alpha:', alpha, '\n', file=stderr())
      #      cat('Product:', line, '\n', file=stderr())
      #      
      n = length(d1)

      xspace = (max(d1[lowAngle():RgRange()]^2)-min(d1[lowAngle():RgRange()]^2))/10
      yspace = (max(log(d2[lowAngle():RgRange()]))-min(log(d2[lowAngle():RgRange()])))/100
      plot(d1[(t+1):cp2]^2,log(d2[(t+1):cp2]),xlim=c(min(d1[1:cp2]^2),max(d1[1:cp2]^2)),ylim=c(min(log(d2[1:cp2])),max(log(d2[1:cp2]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
      plot(d1[(t+1):RgRange()]^2,log(d2[(t+1):RgRange()]),xlim=c(min(d1[lowAngle():RgRange()]^2),max(d1[lowAngle():RgRange()]^2)),ylim=c(min(log(d2[lowAngle():RgRange()])),max(log(d2[lowAngle():RgRange()]))),xlab=expression(S^2),ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
      points(d1[lowAngle():t]^2,log(d2[lowAngle():t]),col="red",pch=20,cex=.5)  
      lines(d1[lowAngle():t]^2,line,lwd=2)
      legend("topright", c("Data points used to fit curve","Fitted curve"),cex=.8,pch=c(20,NA), col=c("red","black"), lty=0:1);
      rg = round(output[1],1) ;text((d1[lowAngle()]^2+xspace),(log(d2[RgRange()])+yspace*15),bquote(hat(R[g]) == .(rg)))
      serg = round(output[2],2) ; text((d1[lowAngle()]^2+xspace*1.9),(log(d2[RgRange()])+yspace*9.8),paste("Std. Deviation = ",serg,sep=""))

      cat('sRg limit: ',d1[t]*output[1],'\n',file=stderr())
      
      #      windows()
#      rg = round(output[1],1)
#      serg = round(output[2],2)
#      xspace = (max(d1)-min(d1))/10
#      yspace = (max(log(d2))-min(log(d2)))/100
#      plot(d1[-(lowAngle():t)],log(d2[-(lowAngle():t)]),xlim=c(min(d1),max(d1)),ylim=c(min(log(d2)),max(log(d2))),xlab="S",ylab="Log(Intensity)",pch=1,col="blue",cex=.5)
#      lines(d1[lowAngle():t],line)
#      text((d1[n]-xspace*4),max(log(d2[c(1,n)])-yspace*20),bquote(hat(R[g]) == .(rg)))
#      text((d1[n]-xspace*3.1),(max(log(d2[c(1,n)]))-yspace*26),paste("Std. Deviation = ",serg,sep=""))
#      points(d1[lowAngle():t],log(d2[lowAngle():t]),col="red",pch=20,cex=.5)
#      lines(d1[lowAngle():t],line,lwd=1.7)
#      legend("topright", c("Data points used to fit curve","Fitted curve","Bias-variance criterion range"),cex=.8,pch=c(20,NA,NA), col=c("red","black","black"), lty=0:2);
#      lines(d1[lowAngle():cp2],rep(max(log(d2))+.2,(cp2-lowAngle()+1)),lwd=2,lt=2)
      
      
      #return output data
      return(c(output[1],output[2],t,line))
      
    }
    
    create_gamma_matrix = function(g,p)
    {
      x=rep(0,p^2)
      gamma=matrix(x,p)
      for( i in 1:p )
      {
        for( j in 1:p )
        {
          gamma[i,j] = g[abs(i-j)+1]
        }
      }
      return(gamma)
    }
    
    simulate_noise = function(sig2,a_5_0,beta_curve,cp2,ar_5)
    {
      noise=rep(0,cp2)
      
      #store variance
      level = sqrt(a_5_0$sigma2)
      
      #simulate from only white noise if AR order is 0
      if(ar_5$order == 0)
      {
        for(i in 1:cp2){
          noise[i] = rnorm(1,0,level)
        }
      }
      
      #simulate from AR structure of final residuals
      if(ar_5$order > 0)
      {
        for(i in 1:ar_5$order){
          noise[i] =  rnorm(1,0,level)
        }
        for(i in (ar_5$order+1):cp2){
          noise[i] = sum(a_5_0$phi*rev(noise[(i-ar_5$order):(i-1)])) + rnorm(1,0,level)
        }
      }
      
      #new simulated intensity curve
      I = exp(beta_curve + noise)
      
      return(I)
    }
    
    est_cor_iter = function(d1,d2,s,cp2,delta.s)
    {
      df = 8
      
      #first fit data by ordinary least squares
      #  ************
      Y_beta = log(d2[1:cp2])
      z = s[1:cp2]
      fit_0 = lm( Y_beta ~ ns(z,df))
      
      #estimate gamma_n by fitting AR to resids
      resid_0 = summary(fit_0)$resid
      ar_0 = ar(resid_0,order.max=5)
      
      #extract ar data and put into variable a_0
      if(ar_0$order == 0)
      {
        phi_temp = 0
      }
      if(ar_0$order > 0)
      {
        phi_temp = ar_0$ar
      }
      a_0 = list(phi = phi_temp, theta = c(0), sigma2 = ar_0$var.pred)
      
      #create autocovariance vector
      g_0 = unlist(ARMAacf(a_0$phi,lag.max = cp2)*ar_0$var.pred)
      
      #create correlation matrix
      gamma_0 = create_gamma_matrix(g_0,cp2)
      
      #fit model again using WLS
      X = cbind(rep(1,cp2),ns(z,df))  
      fit_1 = lm.gls(Y_beta~X-1, W = gamma_0,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_1 = fit_1$residuals
      ar_1 = ar(resid_1,order.max=5)
      
      #extract ar data and put into variable a_1
      if(ar_1$order == 0)
      {
        phi_temp = 0
      }
      if(ar_1$order > 0)
      {
        phi_temp = ar_1$ar
      }
      a_1 = list(phi = phi_temp, theta = c(0), sigma2 = ar_1$var.pred)
      
      #create autocovariance vector
      g_1 = unlist(ARMAacf(a_1$phi,lag.max = cp2)*ar_1$var.pred)
      
      #create correlation matrix
      gamma_1 = create_gamma_matrix(g_1,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_2=lm.gls(Y_beta~X-1, W = gamma_1,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_2 = fit_2$residuals
      ar_2 = ar(resid_2,order.max=5)
      
      #extract ar data and put into variable a_2
      if(ar_2$order == 0)
      {
        phi_temp = 0
      }
      if(ar_2$order > 0)
      {
        phi_temp = ar_2$ar
      }
      a_2 = list(phi = phi_temp, theta = c(0), sigma2 = ar_2$var.pred)
      
      #create autocovariance vector
      g_2 = unlist(ARMAacf(a_2$phi,lag.max = cp2)*ar_2$var.pred)
      
      #create correlation matrix
      gamma_2 = create_gamma_matrix(g_2,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_3 = lm.gls(Y_beta~X-1, W = gamma_2,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_3 = fit_3$residuals
      ar_3 = ar(resid_3,order.max=5)
      
      #extract ar data and put into variable a_3
      if(ar_3$order == 0)
      {
        phi_temp = 0
      }
      if(ar_3$order > 0)
      {
        phi_temp = ar_3$ar
      }
      a_3 = list(phi = phi_temp, theta = c(0), sigma2 = ar_3$var.pred)
      
      #create autocovariance vector
      g_3 = unlist(ARMAacf(a_3$phi,lag.max = cp2)*ar_3$var.pred)
      
      #create correlation matrix
      gamma_3 = create_gamma_matrix(g_3,cp2)  
      
      #fit model again using WLS with updated 
      #gamma matrix  
      fit_4 = lm.gls(Y_beta~X-1, W = gamma_3,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_4 = fit_4$residuals
      ar_4 = ar(resid_4,order.max=5)
      
      #extract ar data and put into variable a_4
      if(ar_4$order == 0)
      {
        phi_temp = 0
      }
      if(ar_4$order > 0)
      {
        phi_temp = ar_4$ar
      }
      a_4 = list(phi = phi_temp, theta = c(0), sigma2 = ar_4$var.pred)
      
      #create autocovariance vector
      g_4 = unlist(ARMAacf(a_4$phi,lag.max = cp2)*ar_4$var.pred)
      
      #create correlation matrix
      gamma_4 = create_gamma_matrix(g_4,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_5 = lm.gls(Y_beta~X-1, W = gamma_4,inverse="T")
      
      #estimate gamma_n by fitting AR to resids
      resid_5 = fit_5$residuals
      ar_5 = ar(resid_5,order.max=5)
      
      #extract ar data and put into variable a_5
      if(ar_5$order == 0)
      {
        phi_temp = 0
      }
      if(ar_5$order > 0)
      {
        phi_temp = ar_5$ar
      }
      a_5 = list(phi = phi_temp, theta = c(0), sigma2 = ar_5$var.pred)
      
      #create autocovariance vector
      g_5 = unlist(ARMAacf(a_5$phi,lag.max = cp2)*ar_5$var.pred)
      
      #create correlation matrix
      gamma_5 = create_gamma_matrix(g_5,cp2)
      
      #fit model again using WLS with updated 
      #gamma matrix 
      fit_6 = lm.gls(Y_beta~X-1, W = gamma_5,inverse="T")
      
      #extract parameter estimates from final fit of the data
      beta_est = fit_6$coefficients
      b = beta_est
      
      #final estimates for the autoregressive structure 
      #of the residuals 
      nsp = ns(z,df)
      gamma_est = gamma_5
      a_5_0 = a_5
      
      #determine optimum bias-variance criterion change point
      t = b_v_tradeoff(delta.s,d2,s,cp2,gamma_est,nsp,b)
      
      #final gamma matrix
      gamma_est = gamma_est[1:t,1:t]
      
      #calculate Rg estimate using bias-variance change point
      s = d1
      Y = log(d2[1:t])
      X = cbind(rep(1,t),s[1:t]^2)
      
      alpha = solve(t(X) %*% solve(gamma_est) %*% X) %*% t(X) %*% solve(gamma_est) %*% Y  
      out1 = sqrt(-3*alpha[2])
      
      out = c(out1,0,t,cp2,0,0,0,0,a_5$sigma2,d2[1:cp2])
      return(out)
    }
####


b_v_tradeoff = function(delta.s,d2,s,cp2,gamma_est,nsp,b)
{
  #initialize variables
  f = rep(10000000,cp2)
  var_est = rep(10000000,cp2)
  sum_bias2_avg = rep(10000000,cp2)
  
  #loop though data up to cp2
  
  for(k in (14+lowAngle()):cp2)
  {
    #fit quadratic curve over data
    Y = log(d2[lowAngle():k])
    X = cbind(rep(1,k-lowAngle()+1),s[lowAngle():k]^2)
    alpha = solve(t(X) %*% solve(gamma_est[lowAngle():k,lowAngle():k]) %*% X) %*% t(X) %*% solve(gamma_est[lowAngle():k,lowAngle():k]) %*% Y
    
    #calculate the average bias
    sum_bias2_avg[k] = 1/k*sum((alpha[1]+alpha[2]*s[lowAngle():k]^2-(b[1]+b[2]*nsp[lowAngle():k,1]+b[3]*nsp[lowAngle():k,2]+b[4]*nsp[lowAngle():k,3]+b[5]*nsp[lowAngle():k,4]+b[6]*nsp[lowAngle():k,5]+b[7]*nsp[lowAngle():k,6]+b[8]*nsp[lowAngle():k,7]+b[9]*nsp[lowAngle():k,8]))^2)
    
    #calculate scaled variance of Rg
    a = cbind(0,-3) 
    var_est[k] =  (k*delta.s)^4 * a %*% solve(t(X) %*% X) %*% t(X) %*% gamma_est[lowAngle():k,lowAngle():k] %*% X %*% solve(t(X) %*% X) %*% t(a)   
    
    #bias-variance criterion
    f[k] = var_est[k]+sum_bias2_avg[k]
  }
  
  f = var_est+sum_bias2_avg
  
  #calculate minimum of bias-variance criterion
  t = which(f == min(f))
  return(t)
}


####
#    b_v_tradeoff = function(delta.s,d2,s,cp2,gamma_est,nsp,b)
#    {
      #initialize variables
#      f = rep(10000000,cp2)
#      var_est = rep(10000000,cp2)
#      sum_bias2_avg = rep(10000000,cp2)
      
      #loop though data up to cp2
#      for(k in 25:cp2)
#      {
        #fit quadratic curve over data
#        Y = log(d2[1:k])
#        X = cbind(rep(1,k),s[1:k]^2)
#        alpha = solve(t(X) %*% solve(gamma_est[1:k,1:k]) %*% X) %*% t(X) %*% solve(gamma_est[1:k,1:k]) %*% Y
        
        #calculate the average bias
#        sum_bias2_avg[k] = 1/k*sum((alpha[1]+alpha[2]*s[1:k]^2-(b[1]+b[2]*nsp[1:k,1]+b[3]*nsp[1:k,2]+b[4]*nsp[1:k,3]+b[5]*nsp[1:k,4]+b[6]*nsp[1:k,5]+b[7]*nsp[1:k,6]+b[8]*nsp[1:k,7]+b[9]*nsp[1:k,8]))^2)
        
        #calculate scaled variance of Rg
#        a = cbind(0,-3) 
#        var_est[k] =  (k*delta.s)^4 * a %*% solve(t(X) %*% X) %*% t(X) %*% gamma_est[1:k,1:k] %*% X %*% solve(t(X) %*% X) %*% t(a)   
        
        #bias-variance criterion
#        f[k] = var_est[k]+sum_bias2_avg[k]
#      }
      
      #smooth bias data
#      bias2 = c(rep(1000000,24),(supsmu(25:cp2,sum_bias2_avg[25:cp2]))$y)
      
#      f = var_est + bias2
#      f = var_est+sum_bias2_avg
      
      #calculate minimum of bias-variance criterion
#      t = which(f == min(f))
#      return(t)
#    }
    
    estimate_Rg2(M)
    
  })#end output$plotRg
})#end       
