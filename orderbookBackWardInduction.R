#' Функция решения уравнения HJB-QVI методом обратной индукции
#' plt - структура, в которой определены основные переменные и политики
# plt.T - число временных точек расчета, plt.S - число значений спреда (=3)
# plt.F- число точек расчета дисбаланса объема, plt.Y - количество значений открытой позиции
# plt.dF - шаг величины дисбаланса объемов, plt.Fmax - модуль максимального значения дисбаланса
# plt.ticksize - минимальный шаг цены, plt.comiss - биржевая комиссия
# plt.w - массив значения численной функции владения
# plt.polmk - булевый массив, определяющий, какая политика будет использована при текущих значениях [t,y,f,s]
# если true - лимитные ордера, false - маркет ордера
# plt.thtkq - массив объемов маркет ордеров при действии политики маркет ордеров
# plt.thmka, plt.thmkb - массив значений 0 (выставление на лучшую цену) или 1 (выставление на шаг лучше лучшей цены)
# при действии политики лимитных ордеров
# maxlot - абсолютное максимальное значение открытой позиции

SolveBackwardInduction<-function( maxlot=3)
{   
    # Массив значений функции владения.
    #Двигаемся вниз по временной сетке
    for (t in seq(obMPdf$TFrame , 0, by=-1))
    {
        
        
        #Двигаемся по сетке значения спреда
        for (s in obMPdf$SS)
        {
            # Определяем массив векторов оператора L (массив- по всем значениям открытой позиции,
            #вектор оператора - по всем значениям дисбаланса)
            L <- matrix(data=0,nrow=obMPdf$NY, ncol=obMPdf$NF)
            
            #Двигаемся по сетке открытых значений
            for (y in obMPdf$YY)
            {
                #Двигаемся по сетке дисбаланса объемов
                for (f in obMPdf$FF)
                {
                    #Первый шаг - вычисление функции владения w в конечный момент времени T
                    if (t == obMPdf$TFrame) 
                        w[match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = -abs(y) * (s * obMPdf$deltaTick / 2 + obMPdf$eps)
                    else
                    {
                        #В остальные моменты времени находим значения векторов L (пока без умножения на 
                        # дифференциальные матрицы в первой части выражения для L)
                        
                        L[match(y,obMPdf$YY), match(f,obMPdf$FF)] = LV(y, f, s, t)
                    }
                }
                if (t < obMPdf$TFrame)
                {
                    #Перемножение матричной части и векторов L, полученных выше, в результате получаем
                    # полностью рассчитанные вектора L. plt.rmatrix - матричная часть
                    L[match(y,obMPdf$YY),]<-plt.Lmatrix %*%  L[match(y,obMPdf$YY),]
                }
            }
            #Вычисление выражения M*L для определения политики маркет ордеров
            if (t <obMPdf$TFrame)
            {
                #Двигаемся по сетке открытой позиции
                for (y in obMPdf$YY)
                {
                    #Двигаемся по сетке дисбаланса объемов
                    for (f in obMPdf$FF)
                    {
                        #Максимальное значение контрактов, допустимое в маркет ордере на данном шаге
                        dzmax = min(obMPdf$NY-match(y,obMPdf$YY), maxlot)
                        ML = -1000000
                        #Двигаемся по сетке объема маркет ордера
                        for (dz in seq(max(-match(y,obMPdf$YY), -maxlot),dzmax,by=1))
                        {
                            #Вычисление оператора M*L для каждого значения объема маркет ордера
                            if (L[max(match(y,obMPdf$YY) + dz,1), match(f,obMPdf$FF)] - abs(dz) * (s * obMPdf$deltaTick / 2 + obMPdf$eps) > ML)
                            {
                                ML = L[max(match(y,obMPdf$YY) + dz,1), match(f,obMPdf$FF)] - abs(dz) * (s  * obMPdf$deltaTick / 2 + obMPdf$eps)
                                #Занесение в политику маркет ордеров значения объема
                                plt.thtkq[t, match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = dz
                            }
                        }
                        #Если оператор M*L больше оператора L при всех исходных параметрах, выбирается политика
                        #маркет ордеров
                        if (ML >  L[match(y,obMPdf$YY), match(f,obMPdf$FF)])
                        {
                            #Значению функции владения w присваивается значение оператора M*L
                            w[match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = ML
                            plt.polmk[t, match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = FALSE
                        }
                        # Иначе - политика лимитных ордеров
                        else
                        {
                            #Значению функции владения присваивается значение оператора L
                            w[match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = L[match(y,obMPdf$YY), match(f,obMPdf$FF)]
                            plt.polmk[t, match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)]  = TRUE
                        }
                    }
                }
            }
        }
    }
}

#Функция вычисления значения оператора L, без перемножения на матричную часть
LV<-function(y,f, s, t){
    #Вычисление значений функции вероятности скачков цены на полшага и шаг psi1,2, с коэффициентами beta1,2
    psi1res = 1/(1+exp(-obMPdf$beta1*f))
    psi2res = 1/(1+exp(-obMPdf$beta2*f))
    #Вычисление матожидания изменения средней цены, plt.lj1,plt.lj2 - интенсивности скачков цены
    Edp = obMPdf$lambdaJ1 * (obMPdf$deltaTick / 2) * (2 * psi1res - 1) + obMPdf$lambdaJ2 * obMPdf$deltaTick * (2 * psi2res - 1)
    #Вычисление оператора воздействия спреда на функцию владения, plt.ro - матрица переходов состояний спреда
    Ls = 0
    for (j in seq( 1, nrow(obMPdf$roS)))
    {
        Ls =Ls+ (w[match(y, obMPdf$YY),match(f, obMPdf$FF), j] - w[match(y,obMPdf$YY),match(f,obMPdf$FF), match(s,obMPdf$SS)]) * obMPdf$roS[match(s, obMPdf$SS), j]
    }
    #lambdaS - интенсивность скачков спреда
    Ls = obMPdf$lambdaS * Ls
    #Вычисление матожидания среднеквадратичного изменения цены
    Edpp = 0.25 * obMPdf$lambdaJ1 + obMPdf$lambdaJ2
    
    gv = -10000000
    thmax = 1
    
    if (match(s, obMPdf$SS) == 1) thmax = 0
    gvtemp = 0
    #Вычисление значений вероятности взятия лимитных ордеров в очереди заявок h(f)
    #plt.ch - коэффициент в формуле для вероятности h(f)
    hresp =  1/(1+exp(-(obMPdf$dzeta0+obMPdf$dzeta1*f)))
    hresm =  1/(1+exp(-(obMPdf$dzeta0+obMPdf$dzeta1*(-f))))
    
    #Вычисление слагаемых ga и gb в выражении для оператора L, thmax - максимальное значение, которое принимает
    # политика для лимитных ордеров - 1
    for (i in 0:thmax)
    {
        for (k in 0:thmax)
        {
            
            gvtemp = (i * obMPdf$lambdaMA + (1 - i) * obMPdf$lambdaMA * hresp) * 
                (w[min(match(y, obMPdf$YY) + 1, obMPdf$NY), match(f, obMPdf$FF), match(s, obMPdf$SS)] - 
                     w[match(y,obMPdf$YY),match(f, obMPdf$FF),match(s,obMPdf$SS)] + s * obMPdf$deltaTick/2 - obMPdf$deltaTick*i)+
                (k*obMPdf$lambdaMB + (1 - k) * obMPdf$lambdaMB * hresm) * 
                (w[max(match(y,obMPdf$YY) - 1, 1), match(f,obMPdf$FF),match(s,obMPdf$SS)]-w[match(y,obMPdf$YY),match(f,obMPdf$FF),match(s,obMPdf$SS)] + 
                     s * obMPdf$deltaTick/ 2 - obMPdf$deltaTick* k)
            #Занесение значения 0 или 1 в политику лимитных ордеров
            if (gvtemp > gv)
            {
                gv = gvtemp
                plt.thmkb[t, match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = i 
                plt.thmka[t, match(y,obMPdf$YY), match(f,obMPdf$FF),match(s,obMPdf$SS)] = k
            }
        }
    }
    #Вычисление значения оператора L (без умножения на матричную часть)
    #plt.dt- шаг времени, plt.gamma - мера риска
    lv = w[match(y,obMPdf$YY),match(f,obMPdf$FF),match(s,obMPdf$SS)] + obMPdf$deltat * y * Edp + obMPdf$deltat * Ls - 
        obMPdf$deltat * obMPdf$gamma * y^2* Edpp + obMPdf$deltat * gv
    
    return(lv)
}


#Вычисление матричной части выражения оператора L
SolveLMatrix<-function()
{
    #Дифференциальные матрицы D1,2 и матрица идентичности I.
    D1 = matrix(data=0,nrow=obMPdf$NF, ncol= obMPdf$NF)
    D2 = matrix(data=0,nrow=obMPdf$NF, ncol= obMPdf$NF)
    I = diag(obMPdf$NF)
    LM = matrix(nrow=obMPdf$NF, ncol= obMPdf$NF)
    #Заполняем матрицы на сетке F x F
    for (i in 1:obMPdf$NF)
    {
        k = 1
        if (i <= obMPdf$NF/ 2) 
            k = i
        else 
            k = i - 1
        D1[i, k] = -1 / obMPdf$deltaF
        D1[i, k + 1] = 1 /obMPdf$deltaF
        if (i == 1)
        {
            D2[i, i + 1] = 2 / obMPdf$deltaF^2
        }
        else if (i == obMPdf$NF)
        {
            D2[i, i - 1] = 2 / obMPdf$deltaF^2
        }
        else
        {
            D2[i, i - 1] = 1 / obMPdf$deltaF^2
            D2[i, i + 1] = 1 / obMPdf$deltaF^2
        }
        D2[i, i] = -2 / obMPdf$deltaF^2
        
        #Вычисляем значения матричной части выражения оператора L
        #cft[1] - значение sigmaF из уравнения Орнштейна-Уленбека для Ft,
        #cft[0] - значение alfaF
        for (j in 1:obMPdf$NF)
        {
            LM[i, j] = I[i, j] - 0.5*obMPdf$deltat * obMPdf$sigmaF^2 * D2[i, j] - obMPdf$deltat * obMPdf$alfaF *obMPdf$FF[i]  * D1[i, j]
        }
    }
    #Инвертируем матрицу, используя стороннюю библиотеку alglib
    return(base::solve(LM))
    
}
