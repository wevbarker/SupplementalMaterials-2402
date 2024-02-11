(*========================*)
(*  PartanenTulkkiTheory  *)
(*========================*)

Section@"Partanen-Tulkki gravity";

Comment@"We now examine the theory proposed in arXiv:2310.01460.";
LinearisedLagrangian=xAct`PSALTer`TensorTheory`Coupling1*(
	(CD[-m]@LinearMetric[-r,-n]-CD[-n]@LinearMetric[-r,-m])*
	(CD[m]@LinearMetric[r,n]-CD[n]@LinearMetric[r,m])
);
DisplayExpression[LinearisedLagrangian,EqnLabel->"PartanenTulkkiTheory"];
LinearisedLagrangian//=ToCanonical;
LinearisedLagrangian//=CollectTensors;
ParticleSpectrum[
	LinearisedLagrangian,
	ClassName->"TensorTheory",
	TheoryName->"PartanenTulkkiTheory",
	Method->"Easy",
	MaxLaurentDepth->3
];
Comment@{"What to say about",Cref@"PartanenTulkkiTheory","."};
