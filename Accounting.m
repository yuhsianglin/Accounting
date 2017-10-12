(* :Title: /Users/zachlin/Dropbox/FF/Accounting/US/Accounting.m *)
(* :Notebook:  *)

(* :Author: Yu-Hsiang Lin *)
(* :Date: Thu, Aug 17, 2017 *)

(* :Package Version: 7.1 *)
(* :Mathematica Version: 9.0 *)

(* :Context: Accounting` *)

(*
:Requirements:


*)

(*
:Summary:

	Create my own accounting software.
*)

(*
:Keywords:


*)

(*
:Discussion:

Version 1.0:
	The first version.

Version 2.0:
	Add financial statements.

Version 3.0:
	Classify the type of an account at the moment you add it.
	(* This version abandoned. *)

Version 4.0:
	Continued from Version 2.0.

Version 4.1:
	Rearrange accounts.

Version 4.2:
	Tackle adding new account that did not exist before.

Version 5.0:
	Stop attaching version number to the file name.
	Adding "Allownace for Abroad" to cash flow calculation.

Version 5.1:
	Adapt for US version. (Modify cash accounts.)
	Add PrintMoney[].

Version 5.2:
	Include changes of Retained Earnings in Statement of Changes of Equity.

Version 5.3:
	Build Templates. Turned out it should be defined in 1507.m, so nothing changed here.

Version 6.0: (Abandoned)
	Modified to TW, based on US.
	Generalized PrintMoney[] to arbitrary decimal digits.
	Added system variable DecimalDigits.
	Somehow in making comparative balance sheet,
					amount =
						Extract[
							PreviousBalanceSheet,
							{pos[[1]], pos[[2]], pos[[3]] + 3, 1}
						];
		we only need list depth like {1, 2, 3, 4} instead of {1, 2, 3, 4, 5}.
	Also changed operatingIncreaseCashAccountList and operatingDecreaseCashAccountList.

Version 7.0:
	Finally updated the data form.

Version 7.1:
	Add fbap.
*)



(* -------------------------------------------- *)
(* -------------------------------------------- *)

BeginPackage["Accounting`"];



(* -------------------------------------------- *)

(* Functions to be exported *)

FunctionExportedList =
{
	SetYearAndMonth, CreateAccount, SetAccount,
	AddTransaction,
	PrintAccount, PrintAllAccounts,
	GetAccountBalance,
	GetFinancialStatements,
	ExportBalanceSheet,
	PrintMoney,
	PrintAllStatementData
};



(* -------------------------------------------- *)
(* -------------------------------------------- *)

Begin["`Private`"];



(* -------------------------------------------- *)
(* Objects *)
(* -------------------------------------------- *)

(*
	Account[
		"Title", Abbreviation, "Type", Balance,
		(* Transactions *)
			{
				{{Year, Month, Day}, "transaction name", {Debit amount, Credit amount}},
				...
			}
	]
	
	Transaction[
		{Year, Month, Day}, "transaction name",
		(* Debit side list *)
			{{Abbreviation, Amount}, ...},
		(* Credit side list *)
			{{Abbreviation, Amount}, ...}
	]
*)



(* -------------------------------------------- *)
(* System variables *)
(* -------------------------------------------- *)

AccountList = {};

YearAndMonth;

LastDayInMonthList =
	{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

AssetsList = {};
LiabilitiesList = {};
EquityList = {};
RevenuesList = {};
ExpensesList = {};

NetIncome;
IncomeStatementData;
IncomeStatement;

REAccountObject;
EquityStatementData;
EquityStatement;

BalanceSheetData;
BalanceSheet;

PreviousBalanceSheetData;
TemporaryBalanceSheetObject;
ComparativeBalanceSheetData;
ComparativeBalanceSheet;

CashFlowsStatementData;
CashFlowsStatement;

(* For US version, use 2 *)
DecimalDigits = 2;



(* -------------------------------------------- *)
(* System operations *)
(* -------------------------------------------- *)

GetPositionInAccountList[abbreviation_]:=
	Position[AccountList, abbreviation][[1, 1]];



(* -------------------------------------------- *)

(* This is for convenience *)

GetPositionInAccountListByTitle[title_]:=
	Position[AccountList, title][[1, 1]];



(* -------------------------------------------- *)

GetAccountType[abbreviation_]:=
	AccountList[[ GetPositionInAccountList[abbreviation], 3 ]];



(* -------------------------------------------- *)

DebitOrCredit[abbreviation_]:=
	Switch[
		GetAccountType[abbreviation],
		"Assets", "Debit",
		"Liabilities", "Credit",
		"Equity", "Credit",
		"Revenues", "Credit",
		"Expenses", "Debit"
	];



(* -------------------------------------------- *)

GetAccountBalance[abbreviation_]:=
	AccountList[[ GetPositionInAccountList[abbreviation], 4 ]];



(* -------------------------------------------- *)

GetInitialBalance[abbreviation_]:=
Module[
	{pos, initialBalance},
	
	pos = GetPositionInAccountList[abbreviation];
	
	If[
		DebitOrCredit[abbreviation] == "Debit",
		
		initialBalance = AccountList[[ pos, 5, 1, 3, 1]];,
		
		(* DebitOrCredit[abbreviation] == "Credit" *)
		
		initialBalance = AccountList[[ pos, 5, 1, 3, 2]];
	];
	
	
	
	initialBalance
];



(* -------------------------------------------- *)

(* Income statement *)

GenerateIncomeStatementData[]:=
Module[
	{totalRevenues, totalExpenses, index},
	
	totalRevenues = 0;
	
	Do[
		(
			If[
				AccountList[[index, 3]] == "Revenues",
				
				(
					totalRevenues = totalRevenues + AccountList[[index, 4]];
					
					RevenuesList = Insert[RevenuesList, AccountList[[index]], -1];
				)
			];
		),
		
		{index, 1, Length[AccountList]}
	];
	
	
	
	totalExpenses = 0;
	
	Do[
		(
			If[
				AccountList[[index, 3]] == "Expenses",
				
				(
					totalExpenses = totalExpenses + AccountList[[index, 4]];
					
					ExpensesList = Insert[ExpensesList, AccountList[[index]], -1];
				)
			];
		),
		
		{index, 1, Length[AccountList]}
	];
	
	NetIncome = totalRevenues - totalExpenses;
	
	
	
	IncomeStatementData =
		{
			Table[
				{RevenuesList[[index, 1]], RevenuesList[[index, 4]]},
				
				{index, 1, Length[RevenuesList]}
			],
			
			totalRevenues,
			
			Table[
				{ExpensesList[[index, 1]], ExpensesList[[index, 4]]},
				
				{index, 1, Length[ExpensesList]}
			],
			
			totalExpenses,
			
			NetIncome
		};
];



GenerateIncomeStatement[]:=
Module[
	{index},
	
	IncomeStatement =
		Grid[
		Join[
			{
				{Item["Zach", Alignment -> Left]},
				{Item["Income Statement", Alignment -> Left]},
				{Item[
					"For the Month Ended " <>
					DateString[
						Join[YearAndMonth, {LastDayInMonthList[[  YearAndMonth[[2]]  ]]}],
						{"MonthName", " ", "DayShort", ", ", "Year"}
					],
					
					Alignment -> Left
				]},
				{},
				{Item["Revenues", Alignment -> Left]}
			},
			
			Table[
				{"", Item[IncomeStatementData[[1, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[IncomeStatementData[[1, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[  IncomeStatementData[[1]]  ]}
			],
			
			{
				{"", "", Item["Total Revenues", Alignment -> Left], "", "", Item[PrintMoney[IncomeStatementData[[2]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Expenses", Alignment -> Left]}
			},
			
			Table[
				{"", Item[IncomeStatementData[[3, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[IncomeStatementData[[3, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[ExpensesList]}
			],
			
			{
				{"", "", Item["Total Expenses", Alignment -> Left], "", "", Item[PrintMoney[IncomeStatementData[[4]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Net Income", Alignment -> Left], "", "", "", "", Item[PrintMoney[IncomeStatementData[[5]], DecimalDigits], Alignment -> "."]}
			}
		],
			
			Frame -> All
		];
	
	IncomeStatement
];



(* -------------------------------------------- *)

(* Statement of Changes in Equity *)

(* I explicitly used titles "Capital" and "Retained Earnings" here *)

GenerateEquityStatementData[]:=
Module[
	{
		capitalAccountObject, initialCapital,
		initialRE,
		initialTotalEquity,
		addList, lessList, index,
		totalAdd, totalLess,
		finalTotalEquity
	},
	
	capitalAccountObject = AccountList[[  GetPositionInAccountListByTitle["Capital"]  ]];
	initialCapital = capitalAccountObject[[5, 1, 3, 2]];
	
	REAccountObject = AccountList[[  GetPositionInAccountListByTitle["Retained Earnings"]  ]];
	initialRE = REAccountObject[[5, 1, 3, 2]];
	
	initialTotalEquity = initialCapital + initialRE;
	
	
	
	(* -------------------------------------------- *)
	
	addList = {};
	lessList = {};
	
	Do[
		If[
			(* It's an "Add" transaction *)
			capitalAccountObject[[5, index, 3, 2]] != 0,
			
			addList =
				Insert[
					addList,
					{
						capitalAccountObject[[5, index, 2]],
						capitalAccountObject[[5, index, 3, 2]]
					},
					-1
				],
			
			(* It's an "Less" transaction *)
			lessList =
				Insert[
					lessList,
					{
						capitalAccountObject[[5, index, 2]],
						capitalAccountObject[[5, index, 3, 1]]
					},
					-1
				]
		],
		
		{index, 2, Length[  capitalAccountObject[[5]]  ]}   (* skip the first entry (initial balance) *)
	];
	
	(* Include the transaction (or adjustment) in Retained Earnings *)
	
	Do[
		If[
			(* It's an "Add" transaction *)
			REAccountObject[[5, index, 3, 2]] != 0,
			
			addList =
				Insert[
					addList,
					{
						REAccountObject[[5, index, 2]],
						REAccountObject[[5, index, 3, 2]]
					},
					-1
				],
			
			(* It's an "Less" transaction *)
			lessList =
				Insert[
					lessList,
					{
						REAccountObject[[5, index, 2]],
						REAccountObject[[5, index, 3, 1]]
					},
					-1
				]
		],
		
		{index, 2, Length[  REAccountObject[[5]]  ]}   (* skip the first entry (initial balance) *)
	];
	
	totalAdd =
		NetIncome +
		Sum[
			addList[[index, 2]],
			
			{index, 1, Length[addList]}
		];
	
	totalLess =
		Sum[
			lessList[[index, 2]],
			
			{index, 1, Length[lessList]}
		];
	
	
	
	(* -------------------------------------------- *)
	
	(* Add the closing entry to Retained Earnings *)
	
	REAccountObject[[5]] =
		Insert[
			REAccountObject[[5]],
			{
				Join[
					YearAndMonth, {LastDayInMonthList[[  YearAndMonth[[2]]  ]]}],
					"Closing",
					0,
					{0, NetIncome}
			},
			-1
		];
	
	REAccountObject[[4]] = REAccountObject[[4]] + NetIncome;
	
	EquityList = {capitalAccountObject, REAccountObject};
	
	finalTotalEquity = capitalAccountObject[[4]] + REAccountObject[[4]];
	
	
	
	(* -------------------------------------------- *)
	
	EquityStatementData =
		{
			{initialCapital, initialRE},
			initialTotalEquity,
			
			Join[
				{{"Net Income", NetIncome}},
				
				addList
			],
			totalAdd,
			
			lessList,
			totalLess,
			
			{capitalAccountObject[[4]], REAccountObject[[4]]},
			finalTotalEquity
		};
];



GenerateEquityStatement[]:=
Module[
	{},
	
	EquityStatement =
		Grid[
		Join[
			{
				{Item["Zach", Alignment -> Left]},
				{Item["Statement of Changes in Equity", Alignment -> Left]},
				{Item[
					"For the Month Ended " <>
					DateString[
						Join[YearAndMonth, {LastDayInMonthList[[  YearAndMonth[[2]]  ]]}],
						{"MonthName", " ", "DayShort", ", ", "Year"}
					],
					
					Alignment -> Left
				]},
				{},
				{Item[
					"Equity " <>
					DateString[
						Join[YearAndMonth, {1}],
						{"MonthName", " ", "DayShort"}
					],
					
					Alignment -> Left
				]},
				{"", Item["Capital", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[1, 1]], DecimalDigits], Alignment -> "."]},
				{"", Item["Retained Earnings", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[1, 2]], DecimalDigits], Alignment -> "."]},
				{"", "", Item["Total Equity", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[2]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Add:", Alignment -> Left]}
			},
			
			Table[
				{"", Item[EquityStatementData[[3, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[3, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[EquityStatementData[[3]]]}
			],
			
			{
				{"", "", Item["Total Add", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[4]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Less:", Alignment -> Left]}
			},
			
			Table[
				{"", Item[EquityStatementData[[5, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[5, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[EquityStatementData[[5]]]}
			],
			
			{
				{"", "", Item["Total Less", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[6]], DecimalDigits], Alignment -> "."]},
				{},
				{Item[
					"Equity " <>
					DateString[
						Join[YearAndMonth, {LastDayInMonthList[[  YearAndMonth[[2]]  ]]}],
						{"MonthName", " ", "DayShort"}
					],
					
					Alignment -> Left
				]},
				{"", Item["Capital", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[7, 1]], DecimalDigits], Alignment -> "."]},
				{"", Item["Retained Earnings", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[7, 2]], DecimalDigits], Alignment -> "."]},
				{"", "", Item["Total Equity", Alignment -> Left], "", "", Item[PrintMoney[EquityStatementData[[8]], DecimalDigits], Alignment -> "."]}
			}
		],
			
			Frame -> All
		];
	
	EquityStatement
];



(* -------------------------------------------- *)

(* Balance Sheet *)

GenerateBalanceSheetData[]:=
Module[
	{totalAssets, index, totalLiabilities},
	
	totalAssets = 0;
	
	Do[
		(
			If[
				AccountList[[index, 3]] == "Assets",
				
				(
					totalAssets = totalAssets + AccountList[[index, 4]];
					
					AssetsList = Insert[AssetsList, AccountList[[index]], -1];
				)
			];
		),
		
		{index, 1, Length[AccountList]}
	];
	
	
	
	totalLiabilities = 0;
	
	Do[
		(
			If[
				AccountList[[index, 3]] == "Liabilities",
				
				(
					totalLiabilities = totalLiabilities + AccountList[[index, 4]];
					
					LiabilitiesList = Insert[LiabilitiesList, AccountList[[index]], -1];
				)
			];
		),
		
		{index, 1, Length[AccountList]}
	];
	
	
	
	finalTotalEquity =
		Sum[
			EquityList[[index, 4]],
			
			{index, 1, Length[EquityList]}
		];
	
	
	
	BalanceSheetData =
		{
			Table[
				{AssetsList[[index, 1]], AssetsList[[index, 4]]},
				
				{index, 1, Length[AssetsList]}
			],
			
			totalAssets,
			
			Table[
				{LiabilitiesList[[index, 1]], LiabilitiesList[[index, 4]]},
				
				{index, 1, Length[LiabilitiesList]}
			],
			
			totalLiabilities,
			
			Table[
				{EquityList[[index, 1]], EquityList[[index, 4]]},
				
				{index, 1, Length[EquityList]}
			],
			
			finalTotalEquity,
			
			totalLiabilities + finalTotalEquity
		};
];



GenerateBalanceSheet[]:=
Module[
	{},
	
	BalanceSheet =
		Grid[
		Join[
			{
				{Item["Zach", Alignment -> Left]},
				{Item["Balance Sheet", Alignment -> Left]},
				{Item[
					DateString[
						Join[YearAndMonth, {LastDayInMonthList[[  YearAndMonth[[2]]  ]]}],
						{"MonthName", " ", "DayShort", ", ", "Year"}
					],
					
					Alignment -> Left
				]},
				{},
				{Item["Assets", Alignment -> Left]}
			},
			
			Table[
				{"", Item[BalanceSheetData[[1, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[1, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[BalanceSheetData[[1]]]}
			],
			
			{
				{"", "", Item["Total Assets", Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[2]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Liabilities", Alignment -> Left]}
			},
			
			Table[
				{"", Item[BalanceSheetData[[3, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[3, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[BalanceSheetData[[3]]]}
			],
			
			{
				{"", "", Item["Total Liabilities", Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[4]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Equity", Alignment -> Left]}
			},
			
			Table[
				{"", Item[BalanceSheetData[[5, index, 1]], Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[5, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[BalanceSheetData[[5]]]}
			],
			
			{
				{"", "", Item["Total Equity", Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[6]], DecimalDigits], Alignment -> "."]},
				{},
				{"", "", Item["Total Liabilities and Equity", Alignment -> Left], "", "", Item[PrintMoney[BalanceSheetData[[7]], DecimalDigits], Alignment -> "."]}
			}
		],
			
			Frame -> All
		];
	
	BalanceSheet
];



(* -------------------------------------------- *)

(* Comparative Balance Sheet *)

GenerateComparativeBalanceSheetData[]:=
Module[
	{
		filename,
		
		assetsPairList,
		liabilitiesPairList,
		equityPairList,
		index, pos, amount,
		
		cashAccountsList,
		currentCashAccountsList,
		
		totalAssetsPairList,
		totalLiabilitiesPairList,
		totalEquityPairList
	},
	
	(* Get the balance sheet of the previous month *)
	
	If[
		YearAndMonth[[2]] == 1,
		
		filename =
			"BalanceSheet" <>
			ToString[IntegerDigits[YearAndMonth[[1]] - 1, 10, 2][[1]]] <>
			ToString[IntegerDigits[YearAndMonth[[1]] - 1, 10, 2][[2]]] <>
			"12.mx",
		
		(* YearAndMonth[[2]] != 1 *)
		
		filename =
			"BalanceSheet" <>
			ToString[IntegerDigits[YearAndMonth[[1]], 10, 2][[1]]] <>
			ToString[IntegerDigits[YearAndMonth[[1]], 10, 2][[2]]] <>
			ToString[IntegerDigits[YearAndMonth[[2]] - 1, 10, 2][[1]]] <>
			ToString[IntegerDigits[YearAndMonth[[2]] - 1, 10, 2][[2]]] <>
			".mx"
	];
	
	Get[filename];	(* This reads in a symbol called "TemporaryBalanceSheetObject" *)
	
	PreviousBalanceSheet = TemporaryBalanceSheetObject;
	
	
	
	(* Pair the corresponding entries *)
	
	assetsPairList = {};
	
	Do[
		(
			pos =
				Position[
					PreviousBalanceSheet,
					AssetsList[[index, 1]]
				];
			
			If[
				pos == {},	(* No such account exists before *)
				
				amount = 0,
				
				(* pos != {} *)
				
				(
					pos = pos[[1]];
					
					amount =
						PreviousBalanceSheet[[pos[[1]], pos[[2]], 2]];
				)
			];
			
			assetsPairList =
				Insert[
					assetsPairList,
					{
						AssetsList[[index, 1]],
						amount,
						AssetsList[[index, 4]],
						AssetsList[[index, 4]] - amount
					},
					-1
				];
		),
		
		{index, 1, Length[AssetsList]}
	];
	
	cashAccountsList =
		{
			"Reservoir",
			"Daily Account",
			"Education",
			"Transformation",
			"Saving Account",
			"Financial Freedom Account",
			"Foundation",
			"Rolling Stone Account"
		};
	
	currentCashAccountsList =
		{
			"Daily Account",
			"Education",
			"Transformation",
			"Saving Account",
			"Foundation"
		};
	
	totalAssetsPairList =
		{
			{
				"Total Assets",
				Sum[
					assetsPairList[[index, 2]],
					
					{index, 1, Length[assetsPairList]}
				],
				Sum[
					assetsPairList[[index, 3]],
					
					{index, 1, Length[assetsPairList]}
				],
				Sum[
					assetsPairList[[index, 4]],
					
					{index, 1, Length[assetsPairList]}
				]
			},
			
			{
				"Total Cash",
				Sum[
					Extract[
						assetsPairList,
						{
							Position[
								assetsPairList,
								cashAccountsList[[index]]
							][[1]][[1]],
							2
						}
					],
					
					{index, 1, Length[cashAccountsList]}
				],
				Sum[
					Extract[
						assetsPairList,
						{
							Position[
								assetsPairList,
								cashAccountsList[[index]]
							][[1]][[1]],
							3
						}
					],
					
					{index, 1, Length[cashAccountsList]}
				],
				Sum[
					Extract[
						assetsPairList,
						{
							Position[
								assetsPairList,
								cashAccountsList[[index]]
							][[1]][[1]],
							4
						}
					],
					
					{index, 1, Length[cashAccountsList]}
				]
			},
			
			{
				"Current Cash (Budget Accounts)",
				Sum[
					Extract[
						assetsPairList,
						{
							Position[
								assetsPairList,
								currentCashAccountsList[[index]]
							][[1]][[1]],
							2
						}
					],
					
					{index, 1, Length[currentCashAccountsList]}
				],
				Sum[
					Extract[
						assetsPairList,
						{
							Position[
								assetsPairList,
								currentCashAccountsList[[index]]
							][[1]][[1]],
							3
						}
					],
					
					{index, 1, Length[currentCashAccountsList]}
				],
				Sum[
					Extract[
						assetsPairList,
						{
							Position[
								assetsPairList,
								currentCashAccountsList[[index]]
							][[1]][[1]],
							4
						}
					],
					
					{index, 1, Length[currentCashAccountsList]}
				]
			}
		};
	
	
	
	liabilitiesPairList = {};
	
	Do[
		(
			pos =
				Position[
					PreviousBalanceSheet,
					LiabilitiesList[[index, 1]]
				];
			
			If[
				pos == {},	(* No such account exists before *)
				
				amount = 0,
				
				(* pos != {} *)
				
				(
					pos = pos[[1]];
					
					amount =
						PreviousBalanceSheet[[pos[[1]], pos[[2]], 2]];
				)
			];
			
			liabilitiesPairList =
				Insert[
					liabilitiesPairList,
					{
						LiabilitiesList[[index, 1]],
						amount,
						LiabilitiesList[[index, 4]],
						LiabilitiesList[[index, 4]] - amount
					},
					-1
				];
		),
		
		{index, 1, Length[LiabilitiesList]}
	];
	
	totalLiabilitiesPairList =
		{
			{
				"Total Liabilities",
				Sum[
					liabilitiesPairList[[index, 2]],
					
					{index, 1, Length[liabilitiesPairList]}
				],
				Sum[
					liabilitiesPairList[[index, 3]],
					
					{index, 1, Length[liabilitiesPairList]}
				],
				Sum[
					liabilitiesPairList[[index, 4]],
					
					{index, 1, Length[liabilitiesPairList]}
				]
			}
		};
	
	
	
	equityPairList = {};
	
	Do[
		(
			pos =
				Position[
					PreviousBalanceSheet,
					EquityList[[index, 1]]
				];
			
			If[
				pos == {},	(* No such account exists before *)
				
				amount = 0,
				
				(* pos != {} *)
				
				(
					pos = pos[[1]];
					
					amount =
						PreviousBalanceSheet[[pos[[1]], pos[[2]], 2]];
				)
			];
			
			equityPairList =
				Insert[
					equityPairList,
					{
						EquityList[[index, 1]],
						amount,
						EquityList[[index, 4]],
						EquityList[[index, 4]] - amount
					},
					-1
				];
		),
		
		{index, 1, Length[EquityList]}
	];
	
	totalEquityPairList =
		{
			{
				"Total Equity",
				Sum[
					equityPairList[[index, 2]],
					
					{index, 1, Length[equityPairList]}
				],
				Sum[
					equityPairList[[index, 3]],
					
					{index, 1, Length[equityPairList]}
				],
				Sum[
					equityPairList[[index, 4]],
					
					{index, 1, Length[equityPairList]}
				]
			}
		};
	
	totalEquityPairList =
		Insert[
			totalEquityPairList,
			{
				"Total Liabilities and Equity",
				totalLiabilitiesPairList[[1, 2]] + totalEquityPairList[[1, 2]],
				totalLiabilitiesPairList[[1, 3]] + totalEquityPairList[[1, 3]],
				totalLiabilitiesPairList[[1, 4]] + totalEquityPairList[[1, 4]]
			},
			-1
		];
	
	ComparativeBalanceSheetData =
		{
			assetsPairList,
			totalAssetsPairList,
			
			liabilitiesPairList,
			totalLiabilitiesPairList,
			
			equityPairList,
			totalEquityPairList
		};
];



GenerateComparativeBalanceSheet[]:=
Module[
	{},
	
	ComparativeBalanceSheet =
		Grid[
		Join[
			{
				{Item["Zach", Alignment -> Left]},
				{Item["Comparative Balance Sheet", Alignment -> Left]},
				{Item["End of Month", Alignment -> Left]},
				{},
				{
					"",
					"",
					"",
					Item[
						DateString[
							If[
								YearAndMonth[[2]] == 1,
								{YearAndMonth[[1]] - 1, 12},
								{YearAndMonth[[1]], YearAndMonth[[2]] - 1}
							],
							{"MonthName", ", ", "Year"}
						],
						Alignment -> Left
					],
					Item[
						DateString[
							YearAndMonth,
							{"MonthName", ", ", "Year"}
						],
						Alignment -> Left
					],
					Item["Change in Account Balance (Amount Increased)", Alignment -> Left]
				},
				{Item["Assets", Alignment -> Left]}
			},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[1, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[1, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[1, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[1, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 1, Length[ComparativeBalanceSheetData[[1]]]}
			],
			
			{{}},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[2, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[2, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[2, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[2, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 1, 1}
			],
			
			{{}},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[2, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[2, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[2, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[2, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 2, 3}
			],
			
			{
				{},
				{Item["Liabilities", Alignment -> Left]}
			},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[3, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[3, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[3, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[3, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 1, Length[ComparativeBalanceSheetData[[3]]]}
			],
			
			{{}},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[4, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[4, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[4, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[4, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 1, 1}
			],
			
			{
				{},
				{Item["Equity", Alignment -> Left]}
			},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[5, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[5, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[5, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[5, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 1, Length[ComparativeBalanceSheetData[[5]]]}
			],
			
			{{}},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[6, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[6, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[6, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[6, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 1, 1}
			],
			
			{{}},
			
			Table[
				{
					"",
					Item[ComparativeBalanceSheetData[[6, index, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[ComparativeBalanceSheetData[[6, index, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[6, index, 3]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[ComparativeBalanceSheetData[[6, index, 4]], DecimalDigits], Alignment -> "."]
				},
				
				{index, 2, 2}
			]
		],
			
			Frame -> All
		];
	
	ComparativeBalanceSheet
];



(* -------------------------------------------- *)

(* Statement of Cash Flows *)

(* I explicitly used titles "Machine", "Capital", "Retained Earnings" here *)

GenerateCashFlowsStatementData[]:=
Module[
	{
		operatingDecreaseCashAccountList, operatingIncreaseCashAccountList,
		operatingDecreaseCashList, operatingIncreaseCashList,
		index, pos, amount,
		operatingCashAdjustment, operatingNetCash,
		
		machineAccountObject, investingIncreaseCashList,
		investingNetCash,
		
		capitalAccountObject, financingIncreaseCashList,
		REAccountObject,
		financingNetCash,
		
		totalNetCash, initialCash, finalCash
	},
	
	(* -------------------------------------------- *)
	
	(* Cash flows from operating activities *)
	
	operatingDecreaseCashAccountList =
		{
			"Accounts Receivable",
			"WFB Credit Card Deposit"
			(*"Prepaid Starbucks",*)
			(*"Prepaid Tuition",*)
			(*"Prepaid Giving",*)
			(*"Accumulated Depreciation---Machine"*)
			(*"Prepaid Easy Card"*)
		};
	
	operatingIncreaseCashAccountList =
		{
			"Accounts Payable",
			"WFB Payable",
			"PNC Payable",
			"Unearned Revenue"
		};
	
	
	
	operatingDecreaseCashList = {};
	
	Do[
		(
			pos =
				Position[
					ComparativeBalanceSheetData,
					operatingDecreaseCashAccountList[[index]]
				];
			
			If[
				pos == {},	(* No such account exists *)
				
				amount = 0,
				
				(* pos != {} *)
				
				(
					pos = pos[[1]];
					
					amount = - ComparativeBalanceSheetData[[pos[[1]], pos[[2]], 4]];
				)
			];
			
			operatingDecreaseCashList =
				Insert[
					operatingDecreaseCashList,
					{
						operatingDecreaseCashAccountList[[index]],
						amount
					},
					-1
				];
		),
		
		{index, 1, Length[operatingDecreaseCashAccountList]}
	];
	
	
	
	operatingIncreaseCashList = {};
	
	Do[
		(
			pos =
				Position[
					ComparativeBalanceSheetData,
					operatingIncreaseCashAccountList[[index]]
				];
			
			If[
				pos == {},	(* No such account exists *)
				
				amount = 0,
				
				(* pos != {} *)
				
				(
					pos = pos[[1]];
					
					amount = ComparativeBalanceSheetData[[pos[[1]], pos[[2]], 4]];
				)
			];
			
			operatingIncreaseCashList =
				Insert[
					operatingIncreaseCashList,
					{
						operatingIncreaseCashAccountList[[index]],
						amount
					},
					-1
				];
		),
		
		{index, 1, Length[operatingIncreaseCashAccountList]}
	];
	
	
	
	operatingCashAdjustment =
		Sum[operatingDecreaseCashList[[index, 2]], {index, 1, Length[operatingDecreaseCashList]}] +
		Sum[operatingIncreaseCashList[[index, 2]], {index, 1, Length[operatingIncreaseCashList]}];
	
	operatingNetCash =
		NetIncome + operatingCashAdjustment;
	
	
(*
	(* -------------------------------------------- *)
	
	(* Cash flows from investing activities *)
	
	machineAccountObject =
		AccountList[[  GetPositionInAccountListByTitle["Machine"]  ]];
	
	investingIncreaseCashList =
		Table[
			{
				machineAccountObject[[5, index, 2]],
				-machineAccountObject[[5, index, 3, 1]] + machineAccountObject[[5, index, 3, 2]]
			},
			
			{index, 2, Length[machineAccountObject[[5]]]}
		];
	
	investingNetCash =
		Sum[investingIncreaseCashList[[index, 2]], {index, 1, Length[investingIncreaseCashList]}];
	(* For empty List, this gives 0 *)
*)
	


	(* -------------------------------------------- *)
	
	(* Cash flows from investing activities *)

	investingIncreaseCashList = {};

	investingNetCash =
		Sum[investingIncreaseCashList[[index, 2]], {index, 1, Length[investingIncreaseCashList]}];
	(* For empty List, this gives 0 *)


	
	(* -------------------------------------------- *)
	
	(* Cash flows from financing activities *)
	
	capitalAccountObject =
		AccountList[[  GetPositionInAccountListByTitle["Capital"]  ]];
	
	financingIncreaseCashList =
		Table[
			{
				capitalAccountObject[[5, index, 2]],
				-capitalAccountObject[[5, index, 3, 1]] + capitalAccountObject[[5, index, 3, 2]]
			},
			
			{index, 2, Length[capitalAccountObject[[5]]]}
		];
	
	
	
	(* Include Retained Earnings *)
	
	REAccountObject =
		AccountList[[  GetPositionInAccountListByTitle["Retained Earnings"]  ]];
	
	financingIncreaseCashList =
	Join[
		financingIncreaseCashList,
		
		Table[
			{
				REAccountObject[[5, index, 2]],
				-REAccountObject[[5, index, 3, 1]] + REAccountObject[[5, index, 3, 2]]
			},
			
			{index, 2, Length[REAccountObject[[5]]]}
		]
	];
	
	
	
	financingNetCash =
		Sum[financingIncreaseCashList[[index, 2]], {index, 1, Length[financingIncreaseCashList]}];
	(* For empty List, this gives 0 *)
	
	
	
	(* -------------------------------------------- *)
	
	(* Total cash flows *)
	
	initialCash =
		ComparativeBalanceSheetData[[2, 2, 2]];
	
	totalNetCash =
		operatingNetCash + investingNetCash + financingNetCash;
	
	finalCash =
		initialCash + totalNetCash;
	
	
	
	CashFlowsStatementData =
		{
			NetIncome,
			Join[
				operatingDecreaseCashList,
				operatingIncreaseCashList
			],
			operatingCashAdjustment,
			operatingNetCash,
			
			investingIncreaseCashList,
			investingNetCash,
			
			financingIncreaseCashList,
			financingNetCash,
			
			totalNetCash,
			initialCash,
			finalCash
		};
];



GenerateCashFlowsStatement[]:=
Module[
	{},
	
	CashFlowsStatement =
		Grid[
		Join[
			{
				{Item["Zach", Alignment -> Left]},
				{Item["Statement of Cash Flows", Alignment -> Left]},
				{Item[
					"For the Month Ended " <>
					DateString[
						Join[YearAndMonth, {LastDayInMonthList[[  YearAndMonth[[2]]  ]]}],
						{"MonthName", " ", "DayShort", ", ", "Year"}
					],
					
					Alignment -> Left
				]},
				{},
				{Item["Cash Flows from Operating Activities", Alignment -> Left]},
				{},
				{"", Item["Net Income", Alignment -> Left], "", "", "", Item[PrintMoney[CashFlowsStatementData[[1]], DecimalDigits], Alignment -> "."]},
				{},
				{"", Item["Adjustments to reconcile net income to net cash provided by operating activities:", Alignment -> Left]}
			},
			
			Table[
				{"", "", Item[CashFlowsStatementData[[2, index, 1]], Alignment -> Left], "", Item[PrintMoney[CashFlowsStatementData[[2, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[CashFlowsStatementData[[2]]] - 1}
			],
			
			{
				{
					"",
					"",
					Item[CashFlowsStatementData[[2, -1, 1]], Alignment -> Left],
					"",
					Item[PrintMoney[CashFlowsStatementData[[2, -1, 2]], DecimalDigits], Alignment -> "."],
					Item[PrintMoney[CashFlowsStatementData[[3]], DecimalDigits], Alignment -> "."]
				},
				{},
				{"", Item["Net cash provided by operating activities", Alignment -> Left], "", "", "", Item[PrintMoney[CashFlowsStatementData[[4]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Cash flows from investing activities", Alignment -> Left]},
				{}
			},
			
			Table[
				{"", "", Item[CashFlowsStatementData[[5, index, 1]], Alignment -> Left], "", Item[PrintMoney[CashFlowsStatementData[[5, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[CashFlowsStatementData[[5]]]}
			],
			
			{
				{},
				{"", Item["Net cash provided by investing activities", Alignment -> Left], "", "", "", Item[PrintMoney[CashFlowsStatementData[[6]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Cash flows from financing activities", Alignment -> Left]},
				{}
			},
			
			Table[
				{"", "", Item[CashFlowsStatementData[[7, index, 1]], Alignment -> Left], "", Item[PrintMoney[CashFlowsStatementData[[7, index, 2]], DecimalDigits], Alignment -> "."]},
				
				{index, 1, Length[CashFlowsStatementData[[7]]]}
			],
			
			{
				{},
				{"", Item["Net cash provided by financing activities", Alignment -> Left], "", "", "", Item[PrintMoney[CashFlowsStatementData[[8]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Net increase in cash", Alignment -> Left], "", "", "", "", Item[PrintMoney[CashFlowsStatementData[[9]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Cash at beginning of period", Alignment -> Left], "", "", "", "", Item[PrintMoney[CashFlowsStatementData[[10]], DecimalDigits], Alignment -> "."]},
				{},
				{Item["Cash at end of period", Alignment -> Left], "", "", "", "", Item[PrintMoney[CashFlowsStatementData[[11]], DecimalDigits], Alignment -> "."]}
			}
		],
			
			Frame -> All
		];
	
	CashFlowsStatement
];



(* -------------------------------------------- *)
(* Public operations *)
(* -------------------------------------------- *)

SetYearAndMonth[year_, month_]:=
Module[
	{},
	
	YearAndMonth = {year, month};
	
	{DateString[YearAndMonth, "Year"], DateString[YearAndMonth, "MonthNameShort"]}
];



(* -------------------------------------------- *)

CreateAccount[title_, abbreviation_, type_]:=
Module[
	{},
	
	AccountList =
		Insert[
			AccountList,
			Account[title, abbreviation, type, 0, {}],
			-1
		];
	
	AccountList
];



(* -------------------------------------------- *)

(* SetAccount[] should only be used to initialize the an account. *)

SetAccount[day_, title_, {abbreviation_, balance_}]:=
Module[
	{pos},
	
	pos = GetPositionInAccountList[abbreviation];
	
	AccountList[[pos, 4]] = balance;
	
	If[
		DebitOrCredit[abbreviation] == "Debit",
		
		AccountList[[pos, 5]] =
			Insert[
				AccountList[[pos, 5]],
				{
					Join[YearAndMonth, {day}],
					title,
					{balance, 0}
				},
				-1
			],
		
		(* DebitOrCredit[abbreviation] == "Credit" *)
		
		AccountList[[pos, 5]] =
			Insert[
				AccountList[[pos, 5]],
				{
					Join[YearAndMonth, {1}],
					title,
					{0, balance}
				},
				-1
			]
	];
	
	AccountList
];



(* -------------------------------------------- *)

AddTransaction[day_, title_, debitList_, creditList_]:=
Module[
	{pos, index},
	
	(* Check balance *)
	
	If[
		Sum[
			debitList[[index, 2]],
			
			{index, 1, Length[debitList]}
		] ==
			Sum[
				creditList[[index, 2]],
				
				{index, 1, Length[creditList]}
			],
	
	(
	
	(* Debit side *)
	
	Do[
		(
			pos = GetPositionInAccountList[  debitList[[index, 1]]  ];
			
			
			
			(* Calculate new balance *)
			
			If[
				DebitOrCredit[  AccountList[[pos]]  ] == "Debit",
				
				AccountList[[pos, 4]] = AccountList[[pos, 4]] + debitList[[index, 2]];,
				
				(* DebitOrCredit[  AccountList[[pos]]  ] == "Credit" *)
				
				AccountList[[pos, 4]] = AccountList[[pos, 4]] - debitList[[index, 2]];
			];
			
			
			
			(* Add transaction entry into the account *)
			
			AccountList[[pos, 5]] =
			Insert[
				AccountList[[pos, 5]],
				{
					Join[YearAndMonth, {day}],
					title,
					{debitList[[index, 2]], 0}
				},
				-1
			];
		),
		
		{index, 1, Length[debitList]}
	];
	
	
	
	(* Credit side *)
	
	Do[
		(
			pos = GetPositionInAccountList[  creditList[[index, 1]]  ];
			
			
			
			(* Calculate new balance *)
			
			If[
				DebitOrCredit[  AccountList[[pos]]  ] == "Debit",
				
				AccountList[[pos, 4]] = AccountList[[pos, 4]] - creditList[[index, 2]];,
				
				(* DebitOrCredit[  AccountList[[pos]]  ] == "Credit" *)
				
				AccountList[[pos, 4]] = AccountList[[pos, 4]] + creditList[[index, 2]];
			];
			
			
			
			(* Add transaction entry into the account *)
			
			AccountList[[pos, 5]] =
			Insert[
				AccountList[[pos, 5]],
				{
					Join[YearAndMonth, {day}],
					title,
					{0, creditList[[index, 2]]}
				},
				-1
			];
		),
		
		{index, 1, Length[creditList]}
	];
	
	),
	
	(* If debit/credit unbalanced *)
	
	Print[
		"\n",
		"Error: Unbalanced transaction."
	];
	];	(* End If[] *)
	
	
	AccountList
];



(* -------------------------------------------- *)

PrintAccount[abbreviation_]:=
Module[
	{accountObject, balance, output, index},
	
	accountObject = AccountList[[  GetPositionInAccountList[abbreviation]  ]];
	
	balance = 0;
	
	If[
		DebitOrCredit[abbreviation] == "Debit",
		
		output = 
			Grid[
			Join[
				{
					{Item[accountObject[[1]], Alignment -> Left], Item[accountObject[[3]], Alignment -> Left]},
					{Item[DateString[YearAndMonth, "Year"], Alignment -> Left], Item[DateString[YearAndMonth, "MonthNameShort"], Alignment -> Left]},
					{Item["Date", Alignment -> Left], Item["Explanation", Alignment -> Left], Item["Debit", Alignment -> Left], Item["Credit", Alignment -> Left], Item["Balance", Alignment -> Left]}
				},
				
				Table[
					{
						Item[DateString[  accountObject[[5, index, 1]], "DayShort"  ], Alignment -> Left],
						Item[accountObject[[5, index, 2]], Alignment -> Left],
						Item[PrintMoney[accountObject[[5, index, 3, 1]], DecimalDigits], Alignment -> "."],
						Item[PrintMoney[accountObject[[5, index, 3, 2]], DecimalDigits], Alignment -> "."],
						Item[PrintMoney[balance = balance + accountObject[[5, index, 3, 1]] - accountObject[[5, index, 3, 2]], DecimalDigits], Alignment -> "."]
					},
					
					{index, 1, Length[accountObject[[5]]]}
				]
			],
			
			Frame -> All
			];,
		
		(* DebitOrCredit[abbreviation] == "Debit" *)
		
		output = 
			Grid[
			Join[
				{
					{Item[accountObject[[1]], Alignment -> Left], Item[accountObject[[3]], Alignment -> Left]},
					{Item[DateString[YearAndMonth, "Year"], Alignment -> Left], Item[DateString[YearAndMonth, "MonthNameShort"], Alignment -> Left]},
					{Item["Date", Alignment -> Left], Item["Explanation", Alignment -> Left], Item["Debit", Alignment -> Left], Item["Credit", Alignment -> Left], Item["Balance", Alignment -> Left]}
				},
				
				Table[
					{
						Item[DateString[  accountObject[[5, index, 1]], "DayShort"  ], Alignment -> Left],
						Item[accountObject[[5, index, 2]], Alignment -> Left],
						Item[PrintMoney[accountObject[[5, index, 3, 1]], DecimalDigits], Alignment -> "."],
						Item[PrintMoney[accountObject[[5, index, 3, 2]], DecimalDigits], Alignment -> "."],
						Item[PrintMoney[balance = balance - accountObject[[5, index, 3, 1]] + accountObject[[5, index, 3, 2]], DecimalDigits], Alignment -> "."]
					},
					
					{index, 1, Length[accountObject[[5]]]}
				]
			],
			
			Frame -> All
			];
	];
	
	
	
	Print[output];
];



(* -------------------------------------------- *)

PrintAllAccounts[]:=
Module[
	{index},
	
	Do[
		PrintAccount[AccountList[[index, 2]]],
		
		{index, 1, Length[AccountList]}
	];
];



(* -------------------------------------------- *)

(* Money number display *)

PrintMoney[n_, decimalDigits_]:=
	NumberForm[n, {Floor[N[Log[10, n]]] + 1 + decimalDigits, decimalDigits}, DigitBlock -> 3];



(* -------------------------------------------- *)

GetFinancialStatements[]:=
Module[
	{},
	
	GenerateIncomeStatementData[];
	Print[GenerateIncomeStatement[]];
	
	GenerateEquityStatementData[];
	Print[GenerateEquityStatement[]];
	
	GenerateBalanceSheetData[];
	Print[GenerateBalanceSheet[]];
	
	GenerateComparativeBalanceSheetData[];
	Print[GenerateComparativeBalanceSheet[]];
	
	GenerateCashFlowsStatementData[];
	Print[GenerateCashFlowsStatement[]];
];



(* -------------------------------------------- *)

(* Just for reference *)

PrintAllStatementData[]:=
Module[
	{},
	
	Print["\nIncomeStatementData = \n", IncomeStatementData];
	Print["\nEquityStatementData = \n", EquityStatementData];
	Print["\nBalanceSheetData = \n", BalanceSheetData];
	Print["\nComparativeBalanceSheetData = \n", ComparativeBalanceSheetData];
	Print["\nCashFlowsStatementData = \n", CashFlowsStatementData];
];



(* -------------------------------------------- *)

(* Save balance sheet for the Comparative Balance Sheets next month *)

ExportBalanceSheet[]:=
Module[
	{fileName},
	
	TemporaryBalanceSheetObject = BalanceSheetData;
	
	fileName =
		"BalanceSheet" <>
		ToString[IntegerDigits[YearAndMonth[[1]], 10, 2][[1]]] <>
		ToString[IntegerDigits[YearAndMonth[[1]], 10, 2][[2]]] <>
		ToString[IntegerDigits[YearAndMonth[[2]], 10, 2][[1]]] <>
		ToString[IntegerDigits[YearAndMonth[[2]], 10, 2][[2]]] <>
		".mx";
	
	DumpSave[fileName, TemporaryBalanceSheetObject];
];



(* -------------------------------------------- *)
(* -------------------------------------------- *)

End[];



Apply[Protect, FunctionExportedList];

EndPackage[];
