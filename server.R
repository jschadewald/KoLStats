library(shiny)
library(ggplot2)
source("piles.R")
source("bones.R")

# Run on Startup

# Lookup table. Rows are the k-values and columns are the l-values from 
# pKcompleteStrings() in bones.R. Used to determine probability of getting
# the unique bones needed given the number of random bones projected to be
# received.
# n=100 total unique bones in the game
# k= number of unique bones that the player still needs
# l= number of random bones received from using dusty piles
#kl<-sapply(1:1000, #length
#           function(i) {
#               sapply(1:100, #k
#                      function(j) {
#                          pKcompleteStrings(l=i, n=100, k=j)
#                      }
#               )
#           }
#    )
#kl[kl<0]<-0 #Correction for floating point errors that result in negatives.
#saveRDS(kl, file="kl.rds")
kl<-readRDS(file="kl.rds")

kl_CDF<-function(ubNeeded, numRB) {
    if (numRB > 1000) {
        1
    } else if (numRB > 0) {
        kl[ubNeeded, numRB]
    } else {
        0
    }
}


shinyServer(function(input, output, session) {
    
    # INPUT VALIDATION
    validPct<-reactive({
        if(is.numeric(input$percentile) && !is.na(input$percentile)
           && length(input$percentile)==1 && input$percentile>=50 
           && input$percentile<100) {
            input$percentile/100
        } else {
            .95
        }
    })
    
    validBones<-reactive({
        if(is.numeric(input$bones) && !is.na(input$bones)
           && length(input$bones)==1 &&  input$bones>=0 && input$bones<100) {
            round(input$bones,0)
        } else {
            0
        }
    })
    
    validPiles<-reactive({
        if(is.numeric(input$piles) && !is.na(input$piles)
           && length(input$piles)==1 && input$piles>0 && input$piles<Inf) {
            round(input$piles,0)
        } else {
            1
        }
    })
    
    ## Auto-update Input Values
    observe({
        temp<-validPiles() != as.numeric(input$piles)
        if(is.na(temp) || temp) {
            updateNumericInput(session, "piles", value=validPiles())
        }
    })
    
    
    # INTERMEDIATE CALCULATIONS
    
    ## Use normal approximation if piles>=100; otherwise, calculate explicitly.
    ## Returns a vector of length 2, containing the bounds of the middle 
    ## quantile, based on validPct().
    rbQuantiles<-reactive({
        piles<-validPiles()
        pct<-validPct()
        alpha<-1-pct
        if(piles>99) { #Normal approximation
            mu<-.2625*piles
            sigma<-sqrt(.2185937*piles)
            return(round(c(-1,1)*qnorm(1-alpha/2, sd=sigma) + mu,0))
        } else { #Direct discrete calculation
            cdf<-cdfFromPiles(piles)
            lb_mid<-qFromPiles(alpha/2, cdf=cdf)
            ub_mid<-qFromPiles(1-alpha/2, cdf=cdf)
            c(lb_mid, ub_mid)
        }
    })
    
    # Probability Mass Function for unique bones generated from random bones,
    # only counting bones that the player is missing.
    ubExpected<-reactive({
        #Use the mean or 1000. Values >1000 cause NaN and Inf to start
        #popping up inconveniently.
        rbExpected<-min(as.integer(.2625*validPiles()), 1000)
        ubNeeded<-100-validBones()
        
        prob<-sapply(1:ubNeeded,
                     function(i) {
                         ukStrings(l=rbExpected, n=100, k=ubNeeded, u=i)
                     })
        data.frame(nUnique=1:ubNeeded, prob=prob)
    })
    
    
    # OUTPUTS
    
    ## Text Outputs
    
    output$preRbDensity<-renderText({
        paste0("Using ", validPiles(), " piles of dusty animal bones will ",
               "produce between ", rbQuantiles()[1], " and ", rbQuantiles()[2],
               " random dusty animal bones according to a ", validPct()*100,
               "% interval centered at the mean (green shaded area below).")
    })
    
    # Text that precedes the CDF of unique bones.
    output$preUbCDF<-renderText({
        paste0("In particular, the expected number of random bones obtained ",
               "from ", validPiles(), " piles is ", round(.2625*validPiles(),0),
               ". The probability that you will obtain a complete skeleton ",
               "by using ", validPiles(), " piles when you have ", validBones(),
               " unique pieces of the skeleton already is roughly ",
               round(kl_CDF(100-validBones(),round(.2625*validPiles(),0))*100,2),
               "%, as shown ",
               "by the green lines on the graph below. You will need ",
               qDiscreteCDF(validPct(),kl[100-validBones(),]),
               " random bones in order to have a ", validPct()*100, 
               "% chance of obtaining the entire skeleton, as shown ",
               "by the magenta lines.")
    })
    
    output$sorry<-renderText({
        expected<-ceiling(.2625*validPiles())
        if(expected > 1000) {
            paste0("The expected number of random bones, ", expected,
                  ", is larger than 1000, which causes some computational ",
                  "nastiness. So, a maximum value of 1000 will be used below.",
                  " Yes, it's still buggy.")
        } else {
            paste("The graph below assumes that using", validPiles(),
                  "pile(s) of dusty animal bones yields a total of",
                  expected, "\"random bones.\" Yes, it's still buggy.")
        }
    })
    
    
    ## Plot Outputs
    
    ## How many random bones will we get from the piles?
    output$plotRbExpected<-renderPlot({
        piles<-validPiles()
        mu<-.2625*piles
        sigma<-sqrt(.2185937*piles)
        
        # Graphical boundaries to ensure we only plot the "interesting" part.
        xmin<-max(0, mu - 6*sigma)
        xmax<-min(mu + 6*sigma, piles*2)
        
        if(piles > 99) { #Use normal approximation
            x<-seq(xmin,xmax,length.out=200)
            y<-dnorm(x, mean=mu, sd=sigma)
        } else { #Use direct calculation
            x<-0:(piles*2)
            y<-pmfFromPiles(piles)
        }
        
        plotData<-data.frame(rbones=x, prob=y)
        
        #Base plot and labels
        rbPlot<-ggplot(plotData, aes(x=rbones, y=prob)) + 
            geom_line() + geom_point(color="red") + 
            coord_cartesian(xlim=c(xmin,xmax)) +
            labs(x="Random Bones", y="Probability Mass",
                 title=paste(
                     "Probability Mass Function of Obtaining\n x Random Bones",
                     "from", piles, "Piles of Dusty Animal Bones"))
        
        #Show intervals as shading under the curve
        intervalData<-plotData[plotData$rbones>=rbQuantiles()[1] &
                               plotData$rbones<=rbQuantiles()[2],]
        rbPlot <- rbPlot + 
            geom_ribbon(data=intervalData, fill="green", alpha=.5,
                        aes(ymin=0,ymax=prob))
        
        rbPlot
    })
    
    
    # What's the probability of getting all of the remaining skeleton
    # pieces needed?
    output$plotGetRest<-renderPlot({
        plotData<-data.frame(numRb=1:1000, cProb=kl[100-validBones(),])
        restPlot<-ggplot(plotData, aes(x=1:1000, y=cProb)) + 
            geom_point(color="red") + geom_line() +
            coord_cartesian(xlim=c(0,1000)) +
            labs(x="Number of Random Bones",
                 y="Probability of Completing the Set",
                 title=paste(
                     "Cumulative Distribution Function for Entire Set of 100\n",
                     "Unique Bones as a Function of Random Bones Obtained"))
        
        xconf<-qDiscreteCDF(validPct(),plotData$cProb)
        yconf<-plotData$cProb[xconf+1]
        
        xpred<-round(.2625*validPiles(),0)
        ypred<-round(kl_CDF(100-validBones(),xpred),2)
        
        
        lines<-data.frame(qc=xconf, pc=yconf, qp=xpred, pp=ypred)
        
        restPlot + 
            geom_vline(data=lines, aes(xintercept=qc), color="magenta") +
            geom_hline(data=lines, aes(yintercept=pc), color="magenta") +
            geom_vline(data=lines, aes(xintercept=qp), color="green") +
            geom_hline(data=lines, aes(yintercept=pp), color="green")
    })
    
    
    ## How many of the unique bones that we need will we get from the
    ## random bones that were generated?
    output$plotUbExpected<-renderPlot({
        ubPlotData<-ubExpected()
        ubPlot<-ggplot(ubPlotData, aes(x=nUnique, y=prob)) +
            geom_line() + geom_point(color="red") +
            labs(x="New Unique Animal Bones", y="Probability",
                 title=paste(
                     "Probability of Obtaining x out of the", 100-validBones(),
                     "Dusty Animal Bones that You Still Need"))
        
        ubPlot
    })
    
})