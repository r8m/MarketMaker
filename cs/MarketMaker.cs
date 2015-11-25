/*
Суть такова: по каждому новому бидаску из нужного диапазона глубины считаем текущие занчения
ct - время с открытия позиции (0 если позиций нет)
cy - размер открытой позиция
cf - текущий дисбаланс
cs - текущий спред

Торговые политики расчитаны для каждого значения t, y, f, s и на каждый параметр есть предельные значения (мин/макс). Если текущие значения сt, сy, сf, сs выходят за границы, то берем предельные (блок расчета ti, yi, fi, si). Рыночные и предельные параметры лежат в файле marketparams.csv с одной записью с заголовком:

obMPdf.dfdate - дата расчета
obMPdf.symbol - символ

obMPdf.lambdaS - spread jump intensivity расчетная величина
obMPdf.alfaF - Mean reversion parameter F расчетная величина
obMPdf.sigmaF - Volatility paramter F расчетная величина
obMPdf.lambdaJ1 - Price jump intensivity расчетная величина
obMPdf.lambdaJ2 - Price jump intensivity расчетная величина
obMPdf.beta1 - rob. distribution parameters of directions of mid-price jumps расчетная величина
obMPdf.beta2 - rob. distribution parameters of directions of mid-price jumps расчетная величина
obMPdf.lambdaMA - market order jump intensivity at ask side расчетная величина
obMPdf.lambdaMB -  market order jump intensivity at bid side расчетная величина
obMPdf.dzeta0 - Limit order fill rates расчетная величина
obMPdf.dzeta1- Limit order fill rates расчетная величина

obMPdf.TFrame - рабочее временное окно
obMPdf.deltat - шаг времени
obMPdf.NT  - количество шагов времени в окне
obMPdf.MY - абсолютное максимальное значение открытой позиции       
obMPdf.deltaY - шаг изменения позиции
obMPdf.NY - общее количество шагов позиции
obMPdf.MF - абсолютное максимальное значение дисбаланса объемов 
obMPdf.deltaF - шаг изменения дисбаланса объемов 
obMPdf.NF - общее количество шагов дисбаланса объемов 
obMPdf.deltaTick - шаг цены
obMPdf.eps - комиссия за сделку
obMPdf.gamma - Invenory penalization (Risk)
obMPdf.dzetamax - Max market order size in lot
obMPdf.SMax - максимальное значение спреда   
obMPdf.NS - общее количество шагов изменения спреда

Торговые политики лежат в файле politics.csv следующего формата:
politics - номер политики
t - индекс масива значений времени
y - индекс масива значений открытой позиции
f - индекс масива значений дисбаланса
s - индекс масива значений спрела
PLT - флаг политики (true - ММ (лимитные ордера) /false - Control(рыночные ордера))
MMAQTY - ping Ask Флаг(0 или 1)
MMBQTY - ping Bid Флаг(0 или 1)
TQTY - количество контрактов для политики рыночных ордеров Control
TV - значение времени
YV - значение открытой позиции
FV - значение дисбаланса
SV - значение спреда

Оба файла marketparams.csv, politics.csv расчитаны отдельно  и загружаются при инициализации системы.

На каждый бидаск по расчитанным ti, yi, fi, si выбирается политика из массива политик и формируются ордера на исполнение.
При следующем бидаске, если ордер уже исполнился, то будет уже работать новая политика.
Те неисполненные ордера, что ушли глубоко в стакан (>5 например) я снимаю дополнительным обработчиком и тоже по каждому бидаску.



*/
public class MarketMakerHandlerOnBidAskNoR : AddedItemHandler<BidAsk>
    {
        private Strategy strategy; // параметры стратегии (символ, размер лота)
        private int obDepth; // какую глубину стакана будем просматривать
        private OrderBookContext orderBook;
        private ObservableQueue<Signal> signalQueue;
        private TradingDataContext tradingData;
        private Logger logger;
        private double cBidPrice;
        private double cBidVolume;
        private double cOfferPrice;
        private double cOfferVolume;
        private double cBidOfferSpread;

        private OpenPositionLimitSettings oplimits; // ограничения по открытию новых позиций
        private DateTime timenow;
        private DateTime timeopen;
        private double ct;  // время с открытия позиции
        private double cy;  // размер открытой позиция
        private double cf; // текущий дисбаланс
        private double cs; // текущий спред
        private double strategyInitialAmount;
        

        private HashSet<MarketMakerPolitic> politics; // политика на каждый сt, cy, cf, cs
        private OrderbookMarketParameters marketparams; // рыночные параметры для стратегии

        public MarketMakerHandlerOnBidAskNoR(Strategy strategy,
            TradingDataContext tradingData,
            OrderBookContext orderBook,
            ObservableQueue<Signal> signalQueue,
            Logger logger)
            : base(tradingData.Get<ObservableCollection<BidAsk>>())
        {
            this.strategy = strategy;
            this.orderBook = orderBook;
            this.signalQueue = signalQueue;
            this.tradingData = tradingData;
            this.logger = logger;
            this.strategyInitialAmount = this.strategy.Amount;
			
            this.oplimits = this.tradingData.Get<IEnumerable<OpenPositionLimitSettings>>().SingleOrDefault(s => s.StrategyId == this.strategy.Id);
			
            this.obDepth = AppSettings.GetValue<int>("OrderBookDepth");
			
            this.politics = new HashSet<MarketMakerPolitic>(new PoliticCollectionFactory(AppSettings.GetStringValue("politicsFileName")).Make());
			
            this.marketparams = new OrderBookMarketParametersFactory(AppSettings.GetStringValue("marketparamsFileName")).Make().First();
		
        }
	  
	  
	  
	  public override void OnItemAdded(BidAsk item)
        {
        
            // Если пришел бидаск глубже, чем нужно то пропускаем
			if ((item.Row > this.obDepth - 1) || (this.strategy.Symbol != item.Symbol))
                return;

            this.cBidPrice = item.Bid;
            this.cOfferPrice = item.Ask;
            this.cOfferVolume = item.AskSize;
            this.cBidVolume = item.BidSize;
            this.cBidOfferSpread = this.cOfferPrice - this.cBidPrice;

            if (this.cOfferVolume == 0 || this.cBidVolume == 0 || this.cBidPrice >= cOfferPrice)
                return;

            this.timenow = BrokerDateTime.Make(DateTime.Now);
            this.cy = this.tradingData.GetAmount(this.strategy);
            if (this.cy == 0)
            {
                this.ct = 0;
            }
            else
            {
                this.ct =(this.timenow - this.timeopen).TotalMilliseconds / 1000;
            }

            this.cf = Math.Round(Math.Log(this.cBidVolume) - Math.Log(this.cOfferVolume), 4);
            this.cs = this.cBidOfferSpread;


            // Проверка на лимиты (настройка ручная)
			// SPREAD MIN LIMIT
            if (this.cs < this.oplimits.spreadMin*this.marketparams.deltaTick && this.cy == 0)
                return;
            // SPREAD MAX LIMIT
            if (this.cs > this.oplimits.spreadMax * this.marketparams.deltaTick && this.cy == 0)
                return;
            // DISBALANCE MIN LIMIT
            if (Math.Abs(this.cf) < this.oplimits.logFMin && this.cy == 0)
                return;
            // DISBALANCE MAX LIMIT
            if (Math.Abs(this.cf) > this.oplimits.logFMax && this.cy == 0)
                return;

            // Получение политики
			IEnumerable<double> cParams = new List<double> { ct, cy, cf, cs };
            double ti = Math.Floor(Math.Max(Math.Min(this.ct, marketparams.TFrame - marketparams.deltat), 0) / marketparams.deltat) + 1;
            double yi = Math.Floor(Math.Max(Math.Min(this.cy, marketparams.MY), -marketparams.MY) / marketparams.deltaY) + marketparams.MY / marketparams.deltaY + 1;
            double fi = Math.Floor(Math.Max(Math.Min(this.cf, marketparams.MF), -marketparams.MF) / marketparams.deltaF) + marketparams.MF / marketparams.deltaF + 1;
            double si = Math.Floor(Math.Max(Math.Min(this.cs, marketparams.SMax), marketparams.deltaTick) / marketparams.deltaTick);

            MarketMakerPolitic cPolitic = this.politics.Where(p => p.t == (int)ti
                                                                           && p.y == (int)yi
                                                                           && p.f == (int)fi
                                                                           && p.s == (int)si).First();
            if (cPolitic == null)
                return;
			
			//Обработка политики
            if (cPolitic.plt == true)
            {
                this.strategy.Amount = this.strategyInitialAmount;
                if (this.cy == 0)
                {
                    this.timeopen = this.timenow;
                }
				// Market Making policy
                if (cPolitic.pingAsk == 0 && cPolitic.pingBid == 0)
                {
                    
                    Signal signalBuy = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Buy,
                                       OrderType.Limit,
                                       this.cBidPrice,
                                       0,
                                       this.cBidPrice);

                    Signal signalSell = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Sell,
                                       OrderType.Limit,
                                       this.cOfferPrice,
                                       0,
                                       this.cOfferPrice);

                    this.signalQueue.Enqueue(signalBuy);
                    this.signalQueue.Enqueue(signalSell);
                }

                // Pinging Ask Side policy
				if (cPolitic.pingAsk == 1 && cPolitic.pingBid == 0)
                {
                    Signal signalBuy = new Signal(this.strategy,
                                        BrokerDateTime.Make(DateTime.Now),
                                        TradeAction.Buy,
                                        OrderType.Limit,
                                        this.cBidPrice,
                                        0,
                                        this.cBidPrice);

                    Signal signalSell = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Sell,
                                       OrderType.Limit,
                                       this.cOfferPrice - marketparams.deltaTick,
                                       0,
                                       this.cOfferPrice - marketparams.deltaTick);

                    this.signalQueue.Enqueue(signalBuy);
                    this.signalQueue.Enqueue(signalSell);
                }

                // Pinging Bid Side policy
                if (cPolitic.pingAsk == 0 && cPolitic.pingBid == 1)
                {
                    Signal signalBuy = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Buy,
                                       OrderType.Limit,
                                       this.cBidPrice + marketparams.deltaTick,
                                       0,
                                       this.cBidPrice + marketparams.deltaTick);
                    Signal signalSell = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Sell,
                                       OrderType.Limit,
                                       this.cOfferPrice,
                                       0,
                                       this.cOfferPrice);

                    this.signalQueue.Enqueue(signalBuy);
                    this.signalQueue.Enqueue(signalSell);
                }

                // Pinging Bid & Ask Sides policy
                if (cPolitic.pingAsk == 1 && cPolitic.pingBid == 1)
                {
                    Signal signalBuy = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Buy,
                                       OrderType.Limit,
                                       this.cBidPrice + marketparams.deltaTick,
                                       0,
                                       this.cBidPrice + marketparams.deltaTick);
                    Signal signalSell = new Signal(this.strategy,
                                       BrokerDateTime.Make(DateTime.Now),
                                       TradeAction.Sell,
                                       OrderType.Limit,
                                       this.cOfferPrice - marketparams.deltaTick,
                                       0,
                                       this.cOfferPrice - marketparams.deltaTick);
                    this.signalQueue.Enqueue(signalBuy);
                    this.signalQueue.Enqueue(signalSell);

                }
            }

           // Control policy
           if (cPolitic.plt == false)
            {
                if (this.cy == 0)
                {
                    this.timeopen = this.timenow;
                }

                this.strategy.Amount = Math.Abs(cPolitic.takeQty);
                TradeAction action = TradeAction.Buy;
                if (cPolitic.takeQty > 0)
                {
                    action = TradeAction.Buy;
                }
                else if (cPolitic.takeQty < 0)
                {

                    action = TradeAction.Sell;
                }
                else
                    return;

                double price = (this.cBidPrice + this.cOfferPrice) / 2;
                Signal signalControl = new Signal(this.strategy,
                                      BrokerDateTime.Make(DateTime.Now),
                                      action,
                                      OrderType.Market,
                                      price,
                                      0,
                                      0);
                this.signalQueue.Enqueue(signalControl);
                
            }
        }