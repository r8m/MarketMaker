
# Funktciia resheniia uravneniia HJB-QVI metodom obratnoi` induktcii
# plt - struktura, v kotoroi` opredeleny` osnovny`e peremenny`e i politiki
# plt.T - chislo vremenny`kh tochek rascheta, plt.S - chislo znachenii` spreda (=3)
# plt.F- chislo tochek rascheta disbalansa ob``ema, plt.Y - kolichestvo znachenii` otkry`toi` pozitcii
# plt.dF - shag velichiny` disbalansa ob``emov, plt.Fmax - modul` maksimal`nogo znacheniia disbalansa
# plt.ticksize - minimal`ny`i` shag ceny`, plt.comiss - birzhevaia komissiia
# plt.w - massiv znacheniia chislennoi` funktcii vladeniia
# plt.polmk - bulevy`i` massiv, opredeliaiushchii`, kakaia politika budet ispol`zovana pri tekushchikh znacheniiakh [t,y,f,s]
# esli true - limitny`e ordera, false - market ordera
# plt.thtkq - massiv ob``emov market orderov pri dei`stvii politiki market orderov
# plt.thmka, plt.thmkb - massiv znachenii` 0 (vy`stavlenie na luchshuiu cenu) ili 1 (vy`stavlenie na shag luchshe luchshei` ceny`)
# pri dei`stvii politiki limitny`kh orderov
# maxlot - absoliutnoe maksimal`noe znachenie otkry`toi` pozitcii

SolveBackwardInduction<-function()
{   
    # Massiv znachenii` funktcii vladeniia
    # Dvigaemsia vniz po vremennoi` setke
    
    #Pervy`i` shag - vy`chislenie funktcii vladeniia w v konechny`i` moment vremeni T
    for (f in seq(along.with=obMPdf$FF))
        w[obMPdf$NT,seq(along.with=obMPdf$YY) ,f,seq(along.with=obMPdf$SS)] <<- -abs(obMPdf$YY) %*% t(obMPdf$SS * obMPdf$deltaTick / 2 + obMPdf$eps)
    
    
    for (t in obMPdf$NT:1)
    {
        
        #Vy`chislenie vy`razheniia L dlia opredeleniia politiki limit orderov        
        for (s in seq(along.with=obMPdf$SS))
        {
            L <- matrix(data=0,nrow=obMPdf$NY, ncol=obMPdf$NF)
            if (t < obMPdf$NT)
            {
                for (y in seq(along.with=obMPdf$YY))
                {
                    #Dvigaemsia po setke disbalansa ob``emov
                    for (f in seq(along.with=obMPdf$FF))
                    {
                        #V ostal`ny`e momenty` vremeni nahodim znacheniia vektorov L (poka bez umnozheniia na 
                        # differentcial`ny`e matritcy` v pervoi` chasti vy`razheniia dlia L)
                        L[y,f] = LV(y, f, s, t)

                    }
                    
                    #Peremnozhenie matrichnoi` chasti i vektorov L, poluchenny`kh vy`she, v rezul`tate poluchaem
                    # polnost`iu rasschitanny`e vektora L. plt.rmatrix - matrichnaia chast`
                    L[y,]<-plt.Lmatrix %*%  L[y,]
                }
            }
            
            }
            #Vy`chislenie vy`razheniia M*L dlia opredeleniia politiki market orderov
            if (t <obMPdf$NT)
            {
                #Dvigaemsia po setke znacheniia spreda
                for (s in seq(along.with=obMPdf$SS))
                {
                #Dvigaemsia po setke otkry`toi` pozitcii
                for (y in seq(along.with=obMPdf$YY))
                {
                    #Dvigaemsia po setke disbalansa ob``emov
                    for (f in seq(along.with=obMPdf$FF))
                    {
                        #Maksimal`noe znachenie kontraktov, dopustimoe v market ordere na dannom shage
                        dzmin = max(-y+1, -obMPdf$dzetamax)
                        dzmax = min(obMPdf$NY-y,obMPdf$dzetamax)
                        ML = -1000000
                        MLTemp=0
                        #Dvigaemsia po setke ob``ema market ordera
                        for (dz in seq(dzmin,dzmax,by=1))
                        {
                            #Vy`chislenie operatora M*L dlia kazhdogo znacheniia ob``ema market ordera
                            MLTemp=L[y + dz, f] - abs(dz) * (obMPdf$SS[s] * obMPdf$deltaTick / 2 + obMPdf$eps)
                            if( MLTemp> ML)
                            {
                                ML = MLTemp
                                #Zanesenie v politiku market orderov znacheniia ob``ema
                                plt.thtkq[t, y, f,s] <<- dz
                                
                            }
                        }
                        #Esli operator M*L bol`she operatora L pri vsekh ishodny`kh parametrakh, vy`biraetsia politika
                        #market orderov
                        if (ML >  L[y,f])
                        {
                            #Znacheniiu funktcii vladeniia w prisvaivaetsia znachenie operatora M*L
                            w[t,y,f,s] <<- ML
                            plt.polmk[t,y,f,s] <<- FALSE
                        }
                        # Inache - politika limitny`kh orderov
                        else
                        {
                            #Znacheniiu funktcii vladeniia prisvaivaetsia znachenie operatora L
                            w[t, y,f,s] <<- L[y,f]
                            plt.polmk[t, y,f,s]  <<- TRUE
                        }
                        #DEBUG
                        print(paste("plt.polmk",plt.polmk[t, y,f,s], t, y, f, s))
                    }
                }
            }
        }
    }
}

#Funktciia vy`chisleniia znacheniia operatora L, bez peremnozheniia na matrichnuiu chast`
LV<-function(y,f, s, t){
    #Vy`chislenie znachenii` funktcii veroiatnosti skachkov ceny` na polshaga i shag psi1,2, s koe`ffitcientami beta1,2
    psi1res = 1/(1+exp(-obMPdf$beta1*obMPdf$FF[f]))
    psi2res = 1/(1+exp(-obMPdf$beta2*obMPdf$FF[f]))
    #Vy`chislenie matozhidaniia izmeneniia srednei` ceny`, plt.lj1,plt.lj2 - intensivnosti skachkov ceny`
    Edp = obMPdf$lambdaJ1 * (obMPdf$deltaTick / 2) * (2 * psi1res - 1) + obMPdf$lambdaJ2 * obMPdf$deltaTick * (2 * psi2res - 1)
    #Vy`chislenie operatora vozdei`stviia spreda na funktciiu vladeniia, plt.ro - matritca perehodov sostoianii` spreda
    Ls = 0
    for (j in seq( 1, nrow(obMPdf$roS)))
    {
        Ls =Ls+ (w[t+1, y,f, j] - w[t+1, y,f,s]) * obMPdf$roS[s,j]
    }
    #lambdaS - intensivnost` skachkov spreda
    Ls = obMPdf$lambdaS * Ls
    #Vy`chislenie matozhidaniia srednekvadratichnogo izmeneniia ceny`
    Edpp = 0.25 * obMPdf$lambdaJ1 + obMPdf$lambdaJ2
    
    gv = -10000000
    thmax = 1
    
    #if (obMPdf$SS[s] == obMPdf$deltaTick) thmax = 0
    if (s==1) thmax = 0
    gvtemp = 0
    #Vy`chislenie znachenii` veroiatnosti vziatiia limitny`kh orderov v ocheredi zaiavok h(f)
    #plt.ch - koe`ffitcient v formule dlia veroiatnosti h(f)
    
    hresp =  1/(1+exp(-obMPdf$dzeta0+obMPdf$dzeta1*obMPdf$FF[f]))
    hresm =  1/(1+exp(-obMPdf$dzeta0-obMPdf$dzeta1*obMPdf$FF[f]))
    
    #Vy`chislenie slagaemy`kh ga i gb v vy`razhenii dlia operatora L, thmax - maksimal`noe znachenie, kotoroe prinimaet
    # politika dlia limitny`kh orderov - 1
    for (i in 0:thmax)
    {
        for (k in 0:thmax)
        {
            
          #print(t+1, t, f,s)  
          gvtemp =(i * obMPdf$lambdaMA + (1 - i) * obMPdf$lambdaMA * hresp) * 
                        (w[t+1,min(y + 1, obMPdf$NY), f, s] - w[t+1,y,f,s] + 
                             obMPdf$SS[s] * obMPdf$deltaTick/2 - obMPdf$deltaTick*i)+
                    (k*obMPdf$lambdaMB + (1 - k) * obMPdf$lambdaMB * hresm) * 
                        (w[t+1,max(y - 1, 1), f,s]-w[t+1,y,f,s] +  
                             obMPdf$SS[s] * obMPdf$deltaTick/ 2 - obMPdf$deltaTick* k)
            
            #Zanesenie znacheniia 0 ili 1 v politiku limitny`kh orderov
            if (gvtemp > gv)
            {
                gv = gvtemp
                plt.thmkb[t, y,f,s] <<- i 
                plt.thmka[t,y,f,s] <<- k
                
            }
        }
    }
    #Vy`chislenie znacheniia operatora L (bez umnozheniia na matrichnuiu chast`)
    #plt.dt- shag vremeni, plt.gamma - mera riska
    lv = w[t+1,y,f,s] + obMPdf$deltat *( obMPdf$YY[y] * Edp +  Ls - obMPdf$gamma * obMPdf$YY[y]^2* Edpp + gv)


    return(lv)
}


#Vy`chislenie matrichnoi` chasti vy`razheniia operatora L
SolveLMatrix<-function(){
    uu<-c(1,-2,1, rep(0,obMPdf$NF-2))
    uu<-rep(uu,obMPdf$NF+1)
    uu<-uu[-1]
    uu<-uu[1:obMPdf$NF^2]
    D2<-matrix(uu, ncol=obMPdf$NF, nrow=obMPdf$NF, byrow=TRUE)
    D2<-D2/obMPdf$deltaF^2
    
    uun<-c(0,-1,1, rep(0,obMPdf$NF-2))
    uun<-rep(uun,obMPdf$NF+1)
    uun<-uun[-1]
    uun<-uun[1:(((obMPdf$NF-1)/2)*obMPdf$NF)]#obMPdf$NF^2]
    
    uup<-c(-1,1,0, rep(0,obMPdf$NF-2))
    uup<-rep(uup,obMPdf$NF+1)
    uup<-uup[-1]
    uup<-uup[(((obMPdf$NF-1)/2)*obMPdf$NF+1):(obMPdf$NF^2)]
    uu<-c(uun,uup)
    
    D1<-matrix(uu, ncol=obMPdf$NF, nrow=obMPdf$NF, byrow=TRUE)
    D1<-D1/obMPdf$deltaF
    
    #Differentcial`ny`e matritcy` D1,2 i matritca identichnosti I.
    #D1 = matrix(data=0,nrow=obMPdf$NF, ncol= obMPdf$NF)
    #D2 = matrix(data=0,nrow=obMPdf$NF, ncol= obMPdf$NF)
    I = diag(obMPdf$NF)
    LM = matrix(nrow=obMPdf$NF, ncol= obMPdf$NF)
    
    LM = I - 0.5*obMPdf$deltat * obMPdf$sigmaF^2 * D2 - obMPdf$deltat * obMPdf$alfaF * obMPdf$FF* D1
    return(base::solve(LM))
}
