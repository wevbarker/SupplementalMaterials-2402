VerifyCovariance[InputLightconePropagator_]:=Module[{Expr=InputLightconePropagator},
	Expr//=Total;
	Expr//=ToNewCanonical;
	Diagnostic@Expr;	
	SummaryOfResults=SummariseResults[
		"something",
		LocalWaveOperator,
		LocalPropagator,
		LocalSourceConstraints,
		LocalSpectrum,
		LocalMasslessSpectrum,
		LocalOverallUnitarity,
		LocalSummaryOfTheory];
	Print@SummaryOfResults;
	Quit[];
];