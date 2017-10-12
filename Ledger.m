(* :Title: /Users/zachlin/Dropbox/FF/Accounting/US/1708.m *)
(* :Notebook:  *)

(* :Author: Yu-Hsiang Lin *)
(* :Date: *)

(* :Script Version: *)
(* :Mathematica Version: 9.0 *)

(* :Context: Global` *)

(*
:Requirements:

Accounting.m
*)

(*
:Summary:

Ledger script.
*)

(*
:Keywords:


*)

(*
:Discussion:

Version 2.0:
	Start with version 2.0.

Version 3.0:
	Classify the type of an account at the moment you add it.
	(This version abandoned.)

Version 4.0:
	Continued from Version 2.0.

Version 4.1:
	Rearrange accounts.

Version 4.2:
	Tackle adding new account that did not exist before.

20160209:
	From now on the Account Receivable uses Res, not SA.
*)



(* -------------------------------------------- *)
(* Load package *)
(* -------------------------------------------- *)

Needs["Accounting`"];



(* -------------------------------------------- *)
(* Initialize *)
(* -------------------------------------------- *)

SetYearAndMonth[2017, 8];

CreateAccount["Reservoir", res, "Assets"];
SetAccount[1, "Initial balance", {res, 5000}];

CreateAccount["Loan Revenue", rev, "Revenues"];
SetAccount[1, "Initial balance", {rev, 0}];

CreateAccount["Rent Expense", rent, "Expenses"];
SetAccount[1, "Initial balance", {rent, 0}];

CreateAccount["Daily Account", da, "Assets"];
SetAccount[1, "Initial balance", {da, 0}];

CreateAccount["Daily Expense", de, "Expenses"];
SetAccount[1, "Initial balance", {de, 0}];

CreateAccount["Education", edu, "Assets"];
SetAccount[1, "Initial balance", {edu, 0}];

CreateAccount["Tuition", tuition, "Expenses"];
SetAccount[1, "Initial balance", {tuition, 0}];

CreateAccount["Transformation", trans, "Assets"];
SetAccount[1, "Initial balance", {trans, 0}];

CreateAccount["Play Expense", play, "Expenses"];
SetAccount[1, "Initial balance", {play, 0}];

CreateAccount["Saving Account", sa, "Assets"];
SetAccount[1, "Initial balance", {sa, 0}];

CreateAccount["Accounts Receivable", ar, "Assets"];
SetAccount[1, "Initial balance", {ar, 0}];

CreateAccount["Financial Freedom Account", ffa, "Assets"];
SetAccount[1, "Initial balance", {ffa, 0}];

CreateAccount["Foundation", found, "Assets"];
SetAccount[1, "Initial balance", {found, 0}];

CreateAccount["Giving Expense", giving, "Expenses"];
SetAccount[1, "Initial balance", {giving, 0}];

CreateAccount["Rolling Stone Account", rsa, "Assets"];
SetAccount[1, "Initial balance", {rsa, 0}];

CreateAccount["WFB Credit Card Deposit", wfbar, "Assets"];
SetAccount[1, "Initial balance", {wfbar, 0}];

CreateAccount["Accounts Payable", ap, "Liabilities"];
SetAccount[1, "Initial balance", {ap, 0}];

CreateAccount["WFB Payable", wfbap, "Liabilities"];
SetAccount[1, "Initial balance", {wfbap, 0}];

CreateAccount["PNC Payable", pncap, "Liabilities"];
SetAccount[1, "Initial balance", {pncap, 0}];

CreateAccount["Unearned Revenue", unearned, "Liabilities"];
SetAccount[1, "Initial balance", {unearned, 0}];

CreateAccount["Capital", capital, "Equity"];
SetAccount[1, "Initial balance", {capital, 5000}];

CreateAccount["Retained Earnings", re, "Equity"];
SetAccount[1, "Initial balance", {re, 0}];

CreateAccount["Extraordinary Gain or Loss", extraordinary, "Expenses"];
SetAccount[1, "Initial balance", {extraordinary, 0}];



(* -------------------------------------------- *)
(* Add transactions *)
(* -------------------------------------------- *)

(* food, with da *)

food[date_, amount_]:=
AddTransaction[
date,
ToString[Unevaluated[
food
]],
{{
de, amount
}}, {{
da, amount
}}];


(* -------------------------------------------- *)

(* food, with wfbap *)

foodwfb[date_, amount_]:=
AddTransaction[
date,
ToString[Unevaluated[
food wfbap
]],
{{
de, amount
},{res, amount}}, {{
da, amount
},{wfbap, amount}}];



(* -------------------------------------------- *)

(* food, with pncap *)

foodpnc[date_, amount_]:=
AddTransaction[
date,
ToString[Unevaluated[
food pncap
]],
{{
de, amount
},{res, amount}}, {{
da, amount
},{pncap, amount}}];



(* -------------------------------------------- *)

(* laundry *)

laundry[date_, amount_]:=
AddTransaction[
date,
ToString[Unevaluated[
laundry
]],
{{
de, amount
}}, {{
da, amount
}}];



(* -------------------------------------------- *)
(* -------------------------------------------- *)

food[1, 13.2];




(*****************************************************
AddTransaction[
,
ToString[Unevaluated[

]],
{{

}}, {{

}}];
*********************************************************)











(* If this is the first monthly ledger and there is no previous balance sheet, manually put it here *)

(* ================

(* -------------------------------------------- *)
(* Read in PreviousBalanceSheet *)
(* -------------------------------------------- *)

balanceSheet1707 =
	{{{"Reservoir",5000},{"Daily Account",0},{"Education",0},{"Transformation",0},{"Saving Account",0},{"Accounts Receivable",0},{"Financial Freedom Account",0},{"Foundation",0},{"Rolling Stone Account",0},{"WFB Credit Card Deposit",0}},5000,{{"Accounts Payable", 0},{"WFB Payable",0},{"PNC Payable",0},{"Unearned Revenue",0}},0,{{"Capital",5000},{"Retained Earnings",0}},5000,5000};



Accounting`Private`TemporaryBalanceSheetObject = balanceSheet1707;

DumpSave["BalanceSheet1707.mx", Accounting`Private`TemporaryBalanceSheetObject];

================ *)















(* -------------------------------------------- *)
(* Print *)
(* -------------------------------------------- *)

Print[
	"\n",
	"-- Ledger --", "\n"
];

PrintAllAccounts[];



Print[
	"\n",
	"-- Financial Statements --", "\n"
];

GetFinancialStatements[];



(* -------------------------------------------- *)

(* Estimate the allowance per day *)

estimatedAllowance = N[GetAccountBalance[da] / (31 - 28) - 70 / 31];

Which[
	estimatedAllowance > 0,
	Print[
		"\n",
		"Estimated allowance per day = ",
		PrintMoney[estimatedAllowance, 0]
	],
	
	estimatedAllowance < 0,
	Print[
		"\n",
		"Estimated allowance per day = -",
		PrintMoney[-estimatedAllowance, 0]
	],
	
	estimatedAllowance == 0,
	Print[
		"\n",
		"Estimated allowance per day = 0."
	]
];



(* -------------------------------------------- *)

(* Export the balance sheet *)

ExportBalanceSheet[];



(* -------------------------------------------- *)

(* Just for reference *)

PrintAllStatementData[];


