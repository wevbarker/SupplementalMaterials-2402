(* ::Package:: *)

(*====================================*)
(*  CalibrateCaseLinWeylZeroTorsion  *)
(*====================================*)

CalibrateCaseLinWeylZeroTorsion[CaseNumber_,CaseRules_List]:=Module[{
	LinearisedLagrangianLinWeyl,
	Couplings={lLambda,lR1,lR2,lR3,lR4,lR5,lC1,lXi,lNu,lT1,lT2,lT3,lPhi0}},
	
	(*To fit with notation in CriticalCasesLinWeyl*)
	CaseNumberAdded=CaseNumber+13;

	Subsection@("Case "<>ToString@CaseNumberAdded<>" i.e. zero-torsion case "<>ToString@UnitarityInfoLinWeylZeroTorsion[[CaseNumber,1]]);
	Comment@("Now for a new theory. Here is the full nonlinear Lagrangian for Case "<>ToString@CaseNumberAdded<>". Cases 14-29 as defined by FIG. 1 in Lin, PHYS. REV. D 104, 024034 (2021). Cases 30-52 as in TABLE III:");	

	(*Diagnostic*)
	Comment@("Here are the expected details of this case:");
	UnitarityInfoLinWeylZeroTorsionPrint[CaseNumber];

	LinearisedLagrangianLinWeyl=LineariseLagrangianLinWeyl[NonlinearLagrangianLinWeylZeroTorsion/.CaseRules];
	DisplayExpression@CollectTensors@ToCanonical[LinearisedLagrangianLinWeyl];
	
	(*Here we perform restriction on A*)
	Comment@"Here we expand A to linear order in terms of Weyl vector and tetrad perturbation, as required in the zero-torsion restriction.";
	LinearisedLagrangianLinWeyl=LinearisedLagrangianLinWeyl/.WeylTestAExpandToLinear;
	LinearisedLagrangianLinWeyl//=xAct`PSALTer`Private`ToNewCanonical;
	
	Comment@"This is the resultant linearised Lagrangian that will be fed into PSALTer.";
	DisplayExpression@CollectTensors@ToCanonical[LinearisedLagrangianLinWeyl];
		
	(*Diagnostic line follows*)
	Print@"Hi there, I'm sitting between Zhiyuan's code and Will's code!";
	
	ParticleSpectrum[
		LinearisedLagrangianLinWeyl,
		ClassName->"WeylGaugeTheory",
		TheoryName->("WeylCase"<>ToString@CaseNumberAdded),
		Method->"Easy",
		MaxLaurentDepth->3
	];

	Comment@"So, that's the end of the PSALTer output for this theory. N.B. No unitary conditions in the paper for these cases; here is the unitary information from the paper:";
	UnitarityInfoLinWeylZeroTorsionPrint[CaseNumber];
	Comment@"Okay, that concludes the analysis of this theory.";
	];
