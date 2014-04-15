# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# April 2014, Jurgen Symynck
# Copyright 2014, Jurgen Symynck
#
# For more info, visit http://www.openreliability.org/
#
# For the latest version of this file, check the Subversion repository at
# http://r-forge.r-project.org/projects/abernethy/
#
# Disclaimer:
#    The author is not affiliated with Dr. Abernethy or Wes Fulton - CEO of
#    Fulton Findings(TM) and author of the software package SuperSMITH
#-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# +-----------------------------------+
# |  execute this software with R:    |
# |  http://www.r-project.org/        |
# +-----------------------------------+

calculateSingleFit <- function(x,...){
    # x is a single Abrem object

    #########################
    #  auxiliary functions  #
    #########################
    opadata <- x$options
    opafit <- modifyList(opadata,list(...))
    vm <- function(vlevel,mess)if(opafit$verbosity >= vlevel)message(mess)
    debug1 <- function()vm(2,paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),
            '), pp = \"',opafit$pp,'\" fit...'))
    debug2 <- function()vm(2,paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),") fit..."))
    #ds <- function(level,mess)if(opafit$verbosity >= level)stop(mess)
    neededcolumns <- function(ppp=NULL){
        rankcolumn <- function(colname,ppp){
            na <- unlist(strsplit(tolower(colname),".",TRUE))
            identical(na[1],"rank") && identical(na[2],ppp)
        }
        basis <- x$data[,c("time","event")]
        if(is.null(ppp)){
            basis
        }else{
            wh <- which(sapply(names(x$data),rankcolumn,ppp,USE.NAMES=FALSE))
            cbind(basis,rank=x$data[,wh])
        }
    }
    goodness_of_fit <- function(){
        if(!is.null(x$fit[[i]])){
            if(is.null(x$fit[[i]]$gof)){
                vm(2,"calculateSingleFit: calculating r^2 using cor()...")
                x$fit[[i]]$gof <<- list()
                x$fit[[i]]$gof$r2 <<- cor(times, ranks, use = "complete.obs")^2
            }else vm(2,"calculateSingleFit: r^2 was already set...")
                # if !NULL, then it was already set by the cpp version of the fitting method
    
            if(identical(x$fit[[i]]$gof$r2,1)){
                vm(2,"calculateSingleFit: r^2 is exactly 1, bypassing prr and ccc^2 calculations...")
                    x$fit[[i]]$gof$prr <<- Inf
            }else{
                vm(2,"calculateSingleFit: r^2 is lower than 1 ...")
                x$fit[[i]]$gof$S <<- opafit$S

                vm(2,"calculateSingleFit: Calculating prr and ccc^2...")
                if(tolower(opafit$dist) %in% c("weibull","weibull2p"))
                    distri <- "pivotalMCw2p"
                if(tolower(opafit$dist) %in% c("lognormal","lognormal2p"))    
                    distri <- "pivotalMCln2p"  
                if(!any(c("mle","mle-rba","mle2","mle2-rba","mle3","mle3-rba") %in% tolower(opafit$method.fit))){      
                    prrval <- .Call(distri,
                        na.omit(x$fit[[i]]$data$rank),
                        c(x$fit[[i]]$gof$r2,0.0,1.0,1.0),S=x$fit[[i]]$gof$S,
                        seed=sample.int(.Machine$integer.max,1),
                        Bval=0.5,ProgRpt=FALSE,PACKAGE= "pivotals")
                        # TODO: this doesn't work when using MLE, because there are no ranks.
    
                        # TODO: takes into account xony and yonx?
                    x$fit[[i]]$gof$prr  <<- prrval[[1]]
                    x$fit[[i]]$gof$ccc2 <<- prrval[[2]]
                }else{message("calculateSingleFit:::goodness_of_fit: ",
                    "Goodness of fit calculation using pivotals:::prrval() for mle fits ",
                    "is not supported. ")
                }
            }
        }else{
            vm(1,"calculateSingleFit: no fit available for goodness-of-fit calculation.")
        }
    }
    
    ########################
    #  main function body  #
    ########################
    i <- 1
    atleastonefit <- FALSE
    if(is.null(x$fit)){
        vm(2,"calculateSingleFit: Creating the first fit in the abrem object...")
        i <- 1
        x$fit <- list()
    }else{
        vm(2,"calculateSingleFit: Appending a new fit to the existing abrem object...")
        i <- length(x$fit)+1
    }
    x$fit[[i]] <- list()
    op <- unique(c(names(x$options),names(opafit)))
        # this is needed to add options from opafit into li that
        # are NULL in x$options
        # TODO:tolower() needed?
    if(length(li <- opafit[sapply(op,function(y){
        !identical(x$options[[y]], opafit[[y]])})]) > 0){
        x$fit[[i]]$options <- li
        # the above enlists only options that are different from the abrems
        # 'main' options. This excludes options$dist and options$method.fit
    }
    x$fit[[i]]$n    <- x$n
    x$fit[[i]]$fail <- x$fail
    x$fit[[i]]$cens <- x$cens


    if(!is.null(opafit$importfile)){
        if(opafit$verbosity >= 1)message("calculateSingleFit : ",
            "importing fit results from superSMITH report file\n",opafit$importfile)
        try(fi <- file(opafit$importfile))
        if(!is.null(fi)){
            try(re <- readLines(fi))
            if(!is.null(re)){
                extract <- function(string)
                    na.omit(as.numeric(unlist(strsplit(gsub(",",".",string),"[^0123456789.]"))))
                he <- data.frame(do.call("rbind",lapply(re[1:10],extract)))
                atleastonefit       <<- TRUE

                x$fit[[i]]$options$dist        <- "weibull2p"
                x$fit[[i]]$options$method.fit <- "superSMITH"
                x$fit[[i]]$eta      <- he[3,3]
                x$fit[[i]]$beta     <- he[3,4]
                x$fit[[i]]$gof      <- list()
                x$fit[[i]]$gof$r2   <- he[2,3]
                x$fit[[i]]$gof$prr  <- he[2,1]
                x$fit[[i]]$gof$ccc2 <- he[2,5]

                x$fit[[i]]$n    <- he[5,1]
                x$fit[[i]]$fail <- he[5,1]-he[5,2]
                x$fit[[i]]$cens <- he[5,2]
            }
            close(fi)
        }
        return(x)
    }
    if(any(c("rr","rr2") %in% tolower(opafit$method.fit))){
        #  ____             _                                       _
        # |  _ \ __ _ _ __ | | __  _ __ ___  __ _ _ __ ___  ___ ___(_) ___  _ __
        # | |_) / _` | '_ \| |/ / | '__/ _ \/ _` | '__/ _ \/ __/ __| |/ _ \| '_ \
        # |  _ < (_| | | | |   <  | | |  __/ (_| | | |  __/\__ \__ \ | (_) | | | |
        # |_| \_\__,_|_| |_|_|\_\ |_|  \___|\__, |_|  \___||___/___/_|\___/|_| |_|
        #                                   |___/
#        if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull-2","weibull2p-2")){
        if(tolower(opafit$dist) %in% c("weibull","weibull2p")){
            # __        __   _ _           _ _
            # \ \      / /__(_) |__  _   _| | |
            #  \ \ /\ / / _ \ | '_ \| | | | | |
            #   \ V  V /  __/ | |_) | |_| | | |
            #    \_/\_/ \___|_|_.__/ \__,_|_|_|
            #prepfitlist()
            debug1()
            x$fit[[i]]$data <- neededcolumns(opafit$pp[1])
            times <- log(x$fit[[i]]$data$time)
            ranks <- log(qweibull(x$fit[[i]]$data$rank,1,1))
            rr_weibull2p <- function(is_xony){
                # TODO: does the "rr" or "rr2" need to be part of the function argument?
                x$fit[[i]]$options$dist <<- "weibull2p"
                if("rr" %in% tolower(opafit$method.fit)){
                    atleastonefit <<- TRUE
                    x$fit[[i]]$options$method.fit <<- c("rr",ifelse(is_xony,"xony","yonx"))
                    if(is_xony) x$fit[[i]]$lm  <<- lm(times ~ ranks,x$fit[[i]]$data)
                    else        x$fit[[i]]$lm  <<- lm(ranks ~ times,x$fit[[i]]$data)
                        # TODO: add error checking
                    B <- coef(x$fit[[i]]$lm)[[2]]
                    A <- coef(x$fit[[i]]$lm)[[1]]
                    x$fit[[i]]$beta <<- ifelse(is_xony,1/B,B)
                    x$fit[[i]]$eta  <<- ifelse(is_xony,exp(A),exp(-A/B))
                }
                if("rr2" %in% tolower(opafit$method.fit)){
                    x$fit[[i]]$options$method.fit <<- c("rr2",ifelse(is_xony,"xony","yonx"))
                    ret <- NULL
                    me <- NULL
                    #if(tolower(opafit$pp[1])=="benard") me <- 0
                    if(tolower(opafit$pp[1])=="median")
                        vm(1,paste0(
                        'calculateSingleFit: \"rr2\" only supports \"benard\" at the moment ->
                        continuing using benard approximations.'))
                    try(ret <- .Call(ifelse(is_xony,"MRRw2pXonY","MRRw2pYonX"),
                        x$fit[[i]]$data$time,
                        x$fit[[i]]$data$event,PACKAGE= "pivotals"))
                        # TODO: method=1 -> exact, method=0 -> benard
                        # it appears that 'method' is not supported anymore?
                    if(!is.null(ret)){
                        atleastonefit       <<- TRUE
                        x$fit[[i]]$eta      <<- ret[[1]]
                        x$fit[[i]]$beta     <<- ret[[2]]
                        x$fit[[i]]$gof      <<- list()
                        x$fit[[i]]$gof$r2   <<- ret[[3]]
                    }else{
                        vm(0,"calculateSingleFit: Fitting failed.")
                        x$fit[i] <<- list(NULL)
                            # note that is.null(x$fit[[i]]) will exit with an error
                            # TODO: replace with x$fit[[i]] <<- list(NULL)

                    }
                }
            }
            if("xony" %in% tolower(opafit$method.fit)) rr_weibull2p(TRUE)
            if("yonx" %in% tolower(opafit$method.fit)) rr_weibull2p(FALSE)
                # MRRw2pYonX does not exists, so an error will be generated.
            goodness_of_fit()
        }
        if(tolower(opafit$dist) %in% c("lognormal","lognormal2p")){
            #  _                                                 _
            # | |    ___   __ _ _ __   ___  _ __ _ __ ___   __ _| |
            # | |   / _ \ / _` | '_ \ / _ \| '__| '_ ` _ \ / _` | |
            # | |__| (_) | (_| | | | | (_) | |  | | | | | | (_| | |
            # |_____\___/ \__, |_| |_|\___/|_|  |_| |_| |_|\__,_|_|
            #             |___/
            debug1()
#            message("EXPERIMENTAL CODE! -> NEEDS TO BE VERIFIED!")
            x$fit[[i]]$data <- neededcolumns(opafit$pp[[1]])
            times <- log(x$fit[[i]]$data$time)
            ranks <- log(qlnorm(x$fit[[i]]$data$rank, 0, 1))

            rr_lognormal2p <- function(is_xony){
                x$fit[[i]]$options$dist <<- "lognormal2p"                
                if( "rr"  %in% tolower(opafit$method.fit)){
                    atleastonefit <<- TRUE
                    x$fit[[i]]$options$method.fit <<- c("rr",ifelse(is_xony,"xony","yonx"))
                    
                    if(is_xony) x$fit[[i]]$lm  <<- lm(times ~ ranks,x$fit[[i]]$data)
                    else        x$fit[[i]]$lm  <<- lm(ranks ~ times,x$fit[[i]]$data)
                    A <- coef(x$fit[[i]]$lm)[[1]]
                    B <- coef(x$fit[[i]]$lm)[[2]]
                    if(is_xony){
                        x$fit[[i]]$meanlog  <<- A
                        x$fit[[i]]$sdlog    <<- B
                    }else{
                        x$fit[[i]]$meanlog  <<- -A/B
                        x$fit[[i]]$sdlog    <<- 1/B
                    }
                }
                if("rr2" %in% tolower(opafit$method.fit)){
                    x$fit[[i]]$options$method.fit <<- c("rr2",ifelse(is_xony,"xony","yonx"))
                    ret <- NULL
                    me <- NULL
                    #if(tolower(opafit$pp[1])=="benard") me <- 0
                    if(tolower(opafit$pp[1])=="median")
                        vm(1,paste0(
                        'calculateSingleFit: \"rr2\" only supports \"benard\" at the moment ->
                        continuing using benard approximations.'))
                    try(ret <- .Call(ifelse(is_xony,"MRRln2pXonY","MRRln2pYonX"),
                        x$fit[[i]]$data$time,
                        x$fit[[i]]$data$event,
                        #method=1, 
                        #method=me, 
                        PACKAGE= "pivotals"))
                        # method=1 -> exact, method=0 -> benard
                        # not supported anymore in pivotals 0.1.9?
                    if(!is.null(ret)){
                        atleastonefit       <<- TRUE
                        x$fit[[i]]$meanlog  <<- ret[[1]]
                        x$fit[[i]]$sdlog    <<- ret[[2]]
                        x$fit[[i]]$gof      <<- list()
                        x$fit[[i]]$gof$r2   <<- ret[[3]]
                    }else{
                        vm(1,"calculateSingleFit: Fitting failed.")
                        x$fit[i] <<- list(NULL)

                    }
                }
            }
            if("xony" %in% tolower(opafit$method.fit)) rr_lognormal2p(TRUE)
            if("yonx" %in% tolower(opafit$method.fit)) rr_lognormal2p(FALSE)
            goodness_of_fit()
        }
        if(tolower(opafit$dist) %in% "weibull3p"){
            # __        __   _ _           _ _ _____
            # \ \      / /__(_) |__  _   _| | |___ / _ __
            #  \ \ /\ / / _ \ | '_ \| | | | | | |_ \| '_ \
            #   \ V  V /  __/ | |_) | |_| | | |___) | |_) |
            #    \_/\_/ \___|_|_.__/ \__,_|_|_|____/| .__/
            #                                       |_|
            rr_weibull3p <- function(is_xony){
                if("rr" %in% tolower(opafit$method.fit)){
                    vm(1,paste0(
                    'calculateSingleFit: \"rr\" is not defined for ",
                    opafit$dist,", defaulting to \"rr2\"...'))}
                x$fit[[i]]$options$method.fit <<- c("rr2",ifelse(is_xony,"xony","yonx"))
                    # rr2 points to the cpp code in pivotal package -> it is good to keep 
                    # the convention
                #prepfitlist()
                debug1()
                x$fit[[i]]$options$dist <<- "weibull3p"
                x$fit[[i]]$data <<- neededcolumns(opafit$pp[1])
                ret <- NULL
                try(ret <- .Call(ifelse(is_xony,"MRRw3pXonY","MRRw3pYonX"),
                    x$fit[[i]]$data$time,
                    x$fit[[i]]$data$event,limit = 1e-5, PACKAGE= "pivotals"))
                    ## TODO: incorporate LIMIT argument in another way
                if(!is.null(ret)){
                    atleastonefit       <<- TRUE
                    x$fit[[i]]$beta     <<- ret[[2]]
                    x$fit[[i]]$eta      <<- ret[[1]]
                    x$fit[[i]]$t0       <<- ret[[3]]
                    x$fit[[i]]$gof      <<- list()
                    x$fit[[i]]$gof$r2   <<- ret[[4]]
                    #goodness_of_fit()
                    if(!is.null(opafit$threshold)){
                        if(is.logical(opafit$threshold) && opafit$threshold)
                             x$options$threshold <<- ret[[3]]
                    }
                        # this overwrites any threshold setting at the data level with a number
                        # TODO: this is not the way to go when trying to implement support for
                        # threshold with plot.abrem()
                }else{
                    vm(1,"calculateSingleFit: Fitting failed.")
                    x$fit[i] <<- list(NULL)

                }
            }
            if("xony" %in% tolower(opafit$method.fit)) rr_weibull3p(TRUE)
            if("yonx" %in% tolower(opafit$method.fit)) rr_weibull3p(FALSE)
                # MRRw3pYonX does not exists, so an error will be generated.            
        }
    }
    if(any(c("mle","mle-rba","mle2","mle2-rba","mle3","mle3-rba") %in% tolower(opafit$method.fit))){
        #   from email David Silkworth <djsilk@openreliability.org> ma 4/11/2013 18:29
        #   The MLE you want to call in abrem is MLEw2p_cpp  This is the fast one in compiled code.
        #
        #   The other two are simply demonstrations in R.  
        #     - MLEw2p_abrem solves the MLE using the method shown in The Handbook. 
        #       The likelihood function has been differentiated analytically  
        #       so as to separate beta from eta (by others as can be found in 
        #       the literature).  Then a relatively simple root finder algorithm
        #       (Newton method) identifies the  beta_hat, followed by 
        #       calculation of eta_hat from the separated function.  
        #        This method is detailed in Appendix C, section C.4 of the Handbook.
        #
        #    - MLEw2p_optim solves the MLE using the optim function the same 
        #       way that surv package would.  The optim function is being 
        #       called with a default for the Nelder-Meade "simplex" algorithm.
        #
        #   For the Cpp implementation it was simpler for me to port the 
        #   Newton method as I approached the Weibull first (before lognormal).
        #   Later, in order to do the Cpp implementation of the Lognormal MLE,
        #   I indeed ended up coding a Nelder-Meade simplex algorithm from
        #   scratch.  That algorithm became the basis of the port that is
        #   called by MLEln2p_cpp. I left the development files in the 
        #   package for eventual tutorial value.

        # "mle"  = MLEw2p_abrem
        # "mle2" = MLEw2p_cpp , should be default
        # "mle3" = MLEw2p_optim
        #  __  __ _     _____
        # |  \/  | |   | ____|
        # | |\/| | |   |  _|
        # | |  | | |___| |___
        # |_|  |_|_____|_____|

        debug2()
        x$fit[[i]]$data <- neededcolumns(opafit$pp[1])



        fa <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==1]
        su <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==0]
        ret <- NULL
        is_3p <- FALSE
        if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull3p")){
            if(tolower(opafit$dist) %in% c("weibull","weibull2p")){
                x$fit[[i]]$options$dist <- "weibull2p"
                if(any(c("mle","mle-rba") %in% tolower(opafit$method.fit))){
                    x$fit[[i]]$options$method.fit <- "mle"
                    try(ret <- debias::MLEw2p_abrem(fa,s=su))
                }
                if(any(c("mle2","mle2-rba") %in% tolower(opafit$method.fit))){
                    x$fit[[i]]$options$method.fit <- "mle2"
                    try(ret <- debias::MLEw2p_cpp(fa,s=su))
                        # TODO: bypass the R code and use the CPP code immediately
                }
                if(any(c("mle3","mle3-rba") %in% tolower(opafit$method.fit))){
                    x$fit[[i]]$options$method.fit <- "mle3"
                    try(ret <- debias::MLEw2p_optim(fa,s=su))
                        # TODO: bypass the R code and use the CPP code immediately
                }
            }
            if(tolower(opafit$dist) %in% c("weibull3p")){
                is_3p <- TRUE
                x$fit[[i]]$options$dist <- "weibull3p"
                if(any(c("mle2","mle2","mle2-rba","mle3-rba") %in% tolower(opafit$method.fit))){
                    vm(1,paste0(
                        'calculateSingleFit: \"mle2\" and \"mle3\" are not defined for ",
                        opafit$dist,", defaulting to \"mle\"...'))
                        x$fit[[i]]$options$method.fit <- "mle"}
                try(ret <- debias::MLEw3p_secant(fa,s=su))
                    # secant: purely  R code ...
            }
            if(!is.null(ret)){
                atleastonefit <- TRUE
                x$fit[[i]]$beta <- ret[[2]]
                x$fit[[i]]$eta  <- ret[[1]]
                if(is_3p){
                    x$fit[[i]]$t0   <- ret[[3]]
                    x$fit[[i]]$gof  <- list()
                    x$fit[[i]]$gof$loglik <- ret[[4]]
                }else{
                    x$fit[[i]]$gof <- list()
                    x$fit[[i]]$gof$loglik <- ret[[3]]
                }
                if(!is.null(opafit$threshold)){
                    if(is.logical(opafit$threshold) && opafit$threshold)
                         x$options$threshold <<- ret[[3]]
                }
                    # this overwrites any threshold setting at the data level with a number
                    # TODO: this is not the way to go when trying to implement support for
                    # threshold with plot.abrem()
                if(any(c("mle-rba","mle2-rba","mle3-rba") %in% tolower(opafit$method.fit))){
                    if("mle-rba" %in% tolower(opafit$method.fit))
                        x$fit[[i]]$options$method.fit <- "mle-rba"
                    if("mle2-rba" %in% tolower(opafit$method.fit))
                        x$fit[[i]]$options$method.fit <- "mle2-rba"
                    if("mle3-rba" %in% tolower(opafit$method.fit))
                        x$fit[[i]]$options$method.fit <- "mle3-rba"
                    vm(2,"calculateSingleFit: Applying Abernethy's Bias Reduction ...")
                    x$fit[[i]]$beta <- ret[[2]]*debias::RBAbeta(length(fa))
                        # TODO: set the option: median or mean bias reduction
                }
                goodness_of_fit()
            }else{
                vm(1,"calculateSingleFit: Fitting failed.")
                x$fit[i] <<- list(NULL)


            }
        }
        if(tolower(opafit$dist) %in% c("lognormal","lognormal2p")){
            x$fit[[i]]$options$dist <- "lognormal2p"
            if(any(c("mle","mle-rba","mle3","mle3-rba") %in% tolower(opafit$method.fit))){
                vm(1,paste0(
                'calculateSingleFit: \"mle\" and \"mle3\" are not defined for ',
                opafit$dist,', defaulting to \"mle2\"...'))}     
            x$fit[[i]]$options$method.fit <- "mle2"
            try(ret <- debias::MLEln2p_cpp(fa,s=su))
            if(!is.null(ret)){
                atleastonefit <- TRUE
                x$fit[[i]]$meanlog  <- ret[[1]]
                x$fit[[i]]$sdlog    <- ret[[2]]
                x$fit[[i]]$gof      <- list()
                x$fit[[i]]$gof$loglik <- ret[[3]]
                if(any(c("mle-rba","mle2-rba","mle3-rba") %in% tolower(opafit$method.fit))){
                    if("mle-rba" %in% tolower(opafit$method.fit))
                        x$fit[[i]]$options$method.fit <- "mle-rba"
                    if("mle2-rba" %in% tolower(opafit$method.fit))
                        x$fit[[i]]$options$method.fit <- "mle2-rba"
                    if("mle3-rba" %in% tolower(opafit$method.fit))
                        x$fit[[i]]$options$method.fit <- "mle3-rba"
                    vm(2,"calculateSingleFit: Applying Abernethy's Median Bias Reduction ...")
                    x$fit[[i]]$sdlog <- ret[[2]]*debias::RBAsigma(length(fa))
                        # with RBAsigma, there are no options...
                }
                goodness_of_fit()
            }else{
                vm(1,"calculateSingleFit: Fitting failed.")
                x$fit[i] <<- list(NULL)

            }
        }
    }
    if(!atleastonefit){
        message("*** calculateSingleFit: Nothing has been fitted.  ***\n",
                '*** Does \"method.fit\" include sensible options?   ***')
        # x$fit[[i]] <- NULL
    }
    if(is.numeric(opafit$threshold))
        x$options$threshold <- opafit$threshold
        # overwrite any previously set data-level-t0 to the one specified as an argument to abrem.fit()
        # Don't know why - here - you MUST use <- in favor of <<- ...
    x
    # return a single Abrem object
}