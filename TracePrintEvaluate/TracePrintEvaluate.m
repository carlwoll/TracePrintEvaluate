BeginPackage["TracePrintEvaluate`"];
	
TracePrintEvaluate::usage = "TracePrintEvaluate[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"form\", \"TI\"]\)] prints an in/out toggler for \
expressions which match \!\(\*StyleBox[\"form\", \"TI\"]\). 
TracePrintEvaluate[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"s\", \"TI\"]\)] includes all evaluations which use \
transformation rules associated with the symbol \!\(\*StyleBox[\"s\", \"TI\"]\)."
	
ToggleSelector::usage = "ToggleSelector[{evaluated, unevaluated..}, initial] toggles starting with initial"	

Begin["`Private`"];
	
Options[TracePrintEvaluate]=Join[
	FilterRules[Options[TracePrint], Except[TraceInternal|TraceAction]],
	{
		"TraceIn" -> None, (* Side effect function applied to input *)
		"TraceOut" -> None, (* Side effect function applied to output *)
		"Timing" -> False, (* Whether to show evaluation time *)
		"Toggler" -> 1, (* Which toggler state to show initially *)
		"StripDynamics" -> True, (* Whether to convert dynamic content to literals in the TPE output *)
		"TraceTag" -> Automatic, (* Tag to use for TPE output *)
		TraceInternal -> True
	}
];
	
SetAttributes[TracePrintEvaluate, {HoldAll, ReadProtected}];

TracePrintEvaluate[expr_, form_, opts:OptionsPattern[]] := Module[
	{
	res, index=0, values, timing, start, outAction,
	toggle = Replace[OptionValue["Toggler"], Except[_Integer]->1],
	showtime = TrueQ@OptionValue["Timing"],
	in = OptionValue["TraceIn"],
	out = OptionValue["TraceOut"],
	purge = TrueQ@OptionValue["StripDynamics"],
	tag = Replace[OptionValue["TraceTag"], Except[_String] :> "TPE-"<>DateString[]],
	stylesheet = CurrentValue[{StyleDefinitions, "TPE2"}] =!= {}
	},
		
	(* What to show if a step hasn't finished evaluating *)
	values /: MakeBoxes[_values, fmt_] := "...";
	timing /: MakeBoxes[_timing, fmt_] := "...";
	outAction /: MakeBoxes[_outAction, fmt_] := "...";
		
	(* Set CellTags for the evaluation cell so that printed cells have the tag *)
	SetOptions[EvaluationCell[], CellTags->tag];
	If[!purge,
		$TraceVariables[tag] = {values, timing, start, outAction};
		ClearAttributes[{values, timing, start, outAction}, Temporary]
	];
		
	Block[{stack = {}},
		With[
			{
			tprint = With[{v = push @ index++},
				Print @ TPESelector[
					{
					indented[TraceLevel[]-1, Defer@@#], (* input *)
					visibleForm@values[v], (* output *)
					If[showtime, timing[v], Nothing],
					If[in=!=None, visibleForm@in[#], Nothing],
					If[out=!=None, visibleForm@outAction[v], Nothing]
					},
					toggle,
					"Stylesheet" -> stylesheet
				];
				start[v] = AbsoluteTime[]
			]&,
			tset = Function[Null, 
				With[{v=pop[]},
					timing[v] = AbsoluteTime[]-start[v];
					values[v] = #2;
					(* Perform additional TraceAction if specified *)
					If[out=!=None, outAction[v] = out[#1,#2]]
				],
				SequenceHold
			],
			tsopts = FilterRules[{opts}, Options@TraceScan]
			},

			(* Main trace code *)				
			res = TraceScan[
				tprint,
				expr,
				form,
				tset,
				tsopts,
				TraceInternal->OptionValue[TraceInternal]
			];

			If[purge,			
				(* Clear evaluation cell tag *)
				SetOptions[EvaluationCell[], CellTags->{}];

				(* Select all TracePrintEvaluate cells, and clear tags *)
				NotebookFind[
					EvaluationNotebook[],
					tag,
					All,
					CellTags,
					AutoScroll->False
				];

				FrontEndExecute @ FrontEnd`SelectionRemoveCellTags[
					EvaluationNotebook[],
					tag
				];

				FrontEndExecute @ FrontEnd`NotebookDynamicToLiteral[
					NotebookSelection @ EvaluationNotebook[]
				];

				(* Clear module variables now that their values have been 
				 * burned into the TracePrint cells *)
				Clear[timing, values, start, outAction];
			];
			
			res
		]
	]
]

Options[TPESelector] = {"Stylesheet"->True}
TPESelector /: MakeBoxes[TPESelector[{in_, out__}, init_Integer:1, OptionsPattern[]], fmt_] := With[
	{
	name = "TPE" <> ToString@Length[Hold[in, out]],
	inboxes = MakeBoxes[in, fmt],
	outboxes = ToBoxes /@ Hold[out],
	df = If[TrueQ @ OptionValue[TPESelector, "Stylesheet"], Sequence@@{}, DisplayFunction -> TPEDisplayFunction[Length[{in, out}]]]
	},

	Replace[
		RotateLeft[Prepend[outboxes, inboxes], init-1],
		Hold[x__] :> DynamicBox @ TemplateBox[{x}, name, df]
	]
]

TPEDisplayFunction[n_] := With[{toggles = Table[i -> TagBox[Slot[i], event[i]], {i, n}]},
	Function @ TogglerBox[1, toggles, ImageSize->Automatic]
]

event[i_] := EventHandlerTag[{
	{"MouseClicked", 2} :> Block[{System`TemplateArgBox = #&}, CopyToClipboard @ Cell[StripBoxes @ BoxData[Slot[i]], "Input"]]
}]

(* A lightweight stack *)	
push[x_]:=Last[stack={stack,x}]
pop[]:=(stack=#;#2)&@@stack
	
(* Use an explicit RowBox instead of Row so that StripBoxes will 
 * strip the indenting *)
indented /: MakeBoxes[indented[n_, e_], fmt_] := RowBox[{
	ToBoxes[Indent[n]],
	MakeBoxes[e,fmt]
}]

(* The output should show Sequence objects, and I replace "" with "\"\""
 * so that empty strings are visible and selectable *)
SetAttributes[visibleForm,SequenceHold]
visibleForm /: MakeBoxes[visibleForm[""], fmt_] := StyleBox[
	"\"\"",
	ShowStringCharacters->True
]
visibleForm /: MakeBoxes[visibleForm[u_], fmt_] := MakeBoxes[u,fmt]

(* add TPE styles to evaluation notebook stylesheet *)

styles = Table[
	Cell[StyleData["TPE"<>ToString[n]],
		CellContext->Cell,
		TemplateBoxOptions->{DisplayFunction->TPEDisplayFunction[n]}
	],
	{n, 7}
]

If[CurrentValue[EvaluationNotebook[], {StyleDefinitions, "TPE2"}] === {},
	SetOptions[
		EvaluationNotebook[],
		StyleDefinitions -> Replace[CurrentValue[EvaluationNotebook[], StyleDefinitions],
			{
			Notebook[oldcells_, r__] :> Notebook[Join[oldcells, styles], r],
			other_ :> Notebook[Prepend[styles, Cell[StyleDefinitions->other]]]
			}
		]
	]
]
	
End[]

EndPackage[]