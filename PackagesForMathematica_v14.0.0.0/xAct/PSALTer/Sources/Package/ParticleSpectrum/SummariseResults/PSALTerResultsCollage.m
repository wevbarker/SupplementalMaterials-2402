(*=========================*)
(*  PSALTerResultsCollage  *)
(*=========================*)

BuildPackage@"ParticleSpectrum/SummariseResults/PSALTerResultsCollage/Vectorize.m";
BuildPackage@"ParticleSpectrum/SummariseResults/PSALTerResultsCollage/GraphicsCollage.m";

PSALTerResultsCollage[
		TheSummaryOfTheory_,
		TheWaveOperator_,
		ThePropagator_,
		TheSourceConstraints_,
		TheSpectrum_,
		TheMasslessSpectrum_,
		TheOverallUnitarity_]:=Module[{
	MatricesGroup=Join[TheWaveOperator,ThePropagator,{TheSourceConstraints},{TheSummaryOfTheory}],
	SpectrumGroup=TheSpectrum~Join~TheMasslessSpectrum,
	FinalElement,
	MaxWidth,
	FinalGraphic},
	
	MatricesGroup=Vectorize/@MatricesGroup;
	SpectrumGroup=Vectorize/@SpectrumGroup;

	(*Print@(((Times@@(ImageDimensions@#))&)/@MatricesGroup);
	Print@((((ImageDimensions@#))&)/@MatricesGroup);*)
	FinalElement=MatricesGroup[[-1]];
	MatricesGroup//=(#~Drop~(-1))&;
	MatricesGroup//=(#~DeleteCases~(_?(((Times@@(ImageDimensions@#))>0.5*10^6)&)))&;
	MatricesGroup//=(#~Join~{FinalElement})&;
	MaxWidth=Max@((First/@(ImageDimensions/@(MatricesGroup~Join~SpectrumGroup)))~Join~{500});

	MatricesGroup=GraphicsCollage[MatricesGroup,MaxWidth];
	SpectrumGroup=GraphicsCollage[SpectrumGroup,MaxWidth];

	FinalGraphic={
			MakeLabel@"PSALTer results panel",
			MakeLabel@"Wave operator and propagator",
			MatricesGroup,
			MakeLabel@"Massive and massless spectra",
			SpectrumGroup,
			MakeLabel@"Unitarity conditions",
			TheOverallUnitarity
	};

	FinalGraphic//=Column[#,
			Alignment->{Left,Center},
			Background->PanelColor,
			Frame->True,
			Spacings->{1,1}]&;
FinalGraphic];
