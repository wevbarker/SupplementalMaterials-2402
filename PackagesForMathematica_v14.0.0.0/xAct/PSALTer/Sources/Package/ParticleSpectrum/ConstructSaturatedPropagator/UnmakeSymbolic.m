(*==================*)
(*  UnmakeSymbolic  *)
(*==================*)

BuildPackage@"ParticleSpectrum/ConstructSaturatedPropagator/UnmakeSymbolic/InitialExpand.m";
BuildPackage@"ParticleSpectrum/ConstructSaturatedPropagator/UnmakeSymbolic/GradualExpandSubTask.m";
BuildPackage@"ParticleSpectrum/ConstructSaturatedPropagator/UnmakeSymbolic/ConsolidateUnmakeSymbolic.m";
BuildPackage@"ParticleSpectrum/ConstructSaturatedPropagator/UnmakeSymbolic/ConsolidateFinalElement.m";

UnmakeSymbolic[InverseSymbolicMatrix_,
	DeterminantSymbolic_,
	ReduceFirstIntermediateSymbols_,
	FirstIntermediateSymbolsToSecondIntermediateSymbols_,
	SecondIntermediateSymbolsToCouplingConstants_,
	CouplingAssumptions_]:=Module[{
		RankOfMatrix,
		SubTaskFileNames,
		GraduallyExpandedSubTasks,
		InverseMatrix,
		CombinedRules,
		TheInverseSymbolicMatrix},

	LocalPropagator=" ** UnmakeSymbolic...";

	RankOfMatrix=Length@InverseSymbolicMatrix;

	MatrixElementFileNames=Table[0,{i,RankOfMatrix+1},{j,RankOfMatrix+1}];
	TheInverseSymbolicMatrix=InverseSymbolicMatrix~PadRight~{RankOfMatrix+1,RankOfMatrix+1};
	TheInverseSymbolicMatrix[[RankOfMatrix+1,RankOfMatrix+1]]=Evaluate@(DeterminantSymbolic);

	Table[
		If[j>=i,
			MatrixElement={CouplingAssumptions,Evaluate@(TheInverseSymbolicMatrix[[i,j]])},
			MatrixElement={CouplingAssumptions,0}
		];
		MatrixElementFileName=FileNameJoin@{$WorkingDirectory,
				"tmp",
				"MatrixElement"<>ToString@i<>"By"<>ToString@j<>".mx"};
		MatrixElementFileNames[[i,j]]=MatrixElementFileName;
		DumpSave[MatrixElementFileName,MatrixElement];
		MatrixElement=0;
		MatrixElementFileName="";,
	{i,RankOfMatrix+1},{j,RankOfMatrix+1}];

	Diagnostic@MatrixElementFileNames;

	CombinedRules={ReduceFirstIntermediateSymbols,
			FirstIntermediateSymbolsToSecondIntermediateSymbols,
			SecondIntermediateSymbolsToCouplingConstants};

	LocalPropagator=" ** InitialExpand...";
	SubTaskFileNames=MapThread[
		(xAct`PSALTer`Private`PSALTerParallelSubmit@(InitialExpand[#1,#2]))&,
		{Map[(CombinedRules)&,MatrixElementFileNames,{2}],
		MatrixElementFileNames},2];
	SubTaskFileNames=MonitorParallel@SubTaskFileNames;
	Diagnostic@SubTaskFileNames;

	LocalPropagator=" ** GradualExpandSubTask...";
	MonitorParallel@Map[
		(xAct`PSALTer`Private`PSALTerParallelSubmit@(GradualExpandSubTask[#1]))&,
		SubTaskFileNames,{4}];

	LocalPropagator=" ** ConsolidateUnmakeSymbolic...";
	InverseMatrix=Map[
		(xAct`PSALTer`Private`PSALTerParallelSubmit@(ConsolidateUnmakeSymbolic[#1]))&,
		SubTaskFileNames,{2}];
	InverseMatrix=MonitorParallel@InverseMatrix;
	Diagnostic@InverseMatrix;

	InverseMatrix=InverseMatrix[[1;;RankOfMatrix,1;;RankOfMatrix]]/InverseMatrix[[RankOfMatrix+1,RankOfMatrix+1]];

	LocalPropagator=" ** ConsolidateFinalElement...";
	InverseMatrix=Map[
		(xAct`PSALTer`Private`PSALTerParallelSubmit@(ConsolidateFinalElement[#1]))&,
		Map[{CouplingAssumptions,#}&,InverseMatrix,{2}],{2}];
	InverseMatrix=MonitorParallel@InverseMatrix;
	Diagnostic@InverseMatrix;

	LocalPropagator=" ** Conjugate...";
	Table[
		If[j<i,
			InverseMatrix[[i,j]]=Assuming[
					CouplingAssumptions,
					Conjugate@Evaluate@(InverseMatrix[[j,i]])]
		],
	{i,RankOfMatrix},
	{j,RankOfMatrix}];

	LocalPropagator=" ** ConsolidateFinalElement...";
	InverseMatrix=Map[
		(xAct`PSALTer`Private`PSALTerParallelSubmit@(ConsolidateFinalElement[#1]))&,
		Map[{CouplingAssumptions,#}&,InverseMatrix,{2}],{2}];
	InverseMatrix=MonitorParallel@InverseMatrix;
	Diagnostic@InverseMatrix;

InverseMatrix];
