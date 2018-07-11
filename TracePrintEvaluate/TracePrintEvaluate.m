BeginPackage["TracePrintEvaluate`"];
	
TracePrintEvaluate::usage = "TracePrintEvaluate[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"form\", \"TI\"]\)] prints an in/out toggler for \
expressions which match \!\(\*StyleBox[\"form\", \"TI\"]\). 
TracePrintEvaluate[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"s\", \"TI\"]\)] includes all evaluations which use \
transformation rules associated with the symbol \!\(\*StyleBox[\"s\", \"TI\"]\)."
	
ToggleSelector::usage = "ToggleSelector[{evaluated, unevaluated..}] is like FlipView, \
showing different elements when toggled, but also copies the contents when right-click is used"

Begin["`Private`"];
	
Options[TracePrintEvaluate] = SortBy[
	Join[
		FilterRules[Options[TracePrint], Except[TraceInternal|TraceAction]],
		{
			"TraceIndent" -> " ",
			"TraceIn" -> None, (* Side effect function applied to input *)
			"TraceOut" -> None, (* Side effect function applied to input and output *)
			"Timing" -> False, (* Whether to show evaluation time *)
			"ToggleElements" -> Automatic, (* Which pieces of information to display in the toggler *)
			TraceInternal -> True,
			SaveDefinitions -> True
		}
	],
	ToString[#[[1]], OutputForm]&
];
	
SetAttributes[TracePrintEvaluate, {HoldAll, ReadProtected}];

TracePrintEvaluate[expr_, form_, opts:OptionsPattern[]] := Module[
	{
	res, index=0, values, timing, start, outAction,
	display = lookup[OptionValue["ToggleElements"]],
	showtime = TrueQ@OptionValue["Timing"],
	in = OptionValue["TraceIn"],
	out = OptionValue["TraceOut"],
	stylesheet = CurrentValue[{StyleDefinitions, "ToggleSelector"}] =!= {},
	indent = Replace[OptionValue["TraceIndent"], Except[_String] -> " "]
	},

	display = DeleteCases[display, Alternatives @@ lookup@Pick[{"Timing", "TraceIn", "TraceOut"}, {!showtime, in===None, out===None}]];
	If[display === {}, display = {1, 2}];

	Block[{stack = {}},
		With[
			{
			tprint = With[{v = push @ index++},
				Print @ ToggleSelector[
					{
					indented[TraceLevel[]-2, indent, Defer@@#], (* input *)
					visibleForm @ Dynamic @ values[v], (* output *)
					If[showtime, Dynamic @ timing[v], Null],
					If[in=!=None, visibleForm @ in[#], Null],
					If[out=!=None, visibleForm @ Dynamic @ outAction[v], Null]
					}[[display]],
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
			r = DynamicModule[{vv, tt, oo},
				values = vv;
				timing = tt;
				outAction = oo;
				
				(* What to show if a step hasn't finished evaluating *)
				vv /: MakeBoxes[_vv, fmt_] := "\[Ellipsis]";
				tt /: MakeBoxes[_tt, fmt_] := "\[Ellipsis]";
				oo /: MakeBoxes[_oo, fmt_] := "\[Ellipsis]";
			
				res = TraceScan[
					tprint,
					expr,
					form,
					tset,
					tsopts,
					TraceInternal->OptionValue[TraceInternal]
				];
				
				RawBoxes @ "\[Bullet]",

				Initialization :> (values = vv; timing = tt; outAction = oo)
			];

			If[TrueQ @ OptionValue[SaveDefinitions],
				CellPrint @ Cell[
					BoxData @ ToBoxes @ res,
					"PersistentOutput",
					CellMargins -> CurrentValue[{StyleDefinitions, "Output", CellMargins}],
					CellLabel -> "Out["<>ToString[$Line]<>"]=",
					CellDingbat -> Cell @ BoxData @ ToBoxes @ r
				];
				res;,
				
				res
			]
		]
	]
]

elementAssociation = <|"In" -> 1, "Out" -> 2, "Timing" -> 3, "TraceIn" -> 4, "TraceOut" -> 5|>
lookup[All] := Values[elementAssociation]
lookup[Automatic] := lookup[All]
lookup[n_List] := DeleteMissing @ Lookup[n][elementAssociation]
lookup[n_] := lookup[{n}]

Options[ToggleSelector] = {"Stylesheet" -> False}

ToggleSelector /: MakeBoxes[ToggleSelector[{a__}, OptionsPattern[]], fmt_] := TemplateBox[
	BoxForm`ListMakeBoxes[{a}, fmt],
	"ToggleSelector",
	If[TrueQ @ OptionValue[ToggleSelector, "Stylesheet"],
		Sequence @@ {},
		DisplayFunction -> $OverlayDisplayFunction
	]
]

$OverlayDisplayFunction = DynamicModuleBox[{i = 1, s = {TemplateSlotSequence[1]}}, 
	TagBox[
		OverlayBox[{TemplateSlotSequence[1]}, {i}, i], 
			EventHandlerTag[{
			{"MouseClicked", 1} :> {i = Mod[i + 1, Length[s], 1]},
			{"MouseClicked", 2} :> {
				Block[{System`TemplateArgBox = #&},
					CopyToClipboard @ Cell[
						StripBoxes @ BoxData @ If[MatchQ[s[[i]],_DynamicBox], s[[i,1]], s[[i]]],
						"Input"
					]
				]
			}
		}]
	]
]&

(* A lightweight stack *)	
push[x_]:=Last[stack={stack,x}]
pop[]:=(stack=#;#2)&@@stack
	
(* Use an explicit RowBox instead of Row so that StripBoxes will 
 * strip the indenting *)
indented /: MakeBoxes[indented[n_, i_String, e_], fmt_] := With[{boxes = MakeBoxes[e, fmt]},
	If[n < 1,
		boxes,
		RowBox[{
			StringRepeat[i, n],
			"\[InvisibleSpace]",
			boxes
		}]
	]
]

(* The output should show Sequence objects, and I replace "" with "\"\""
 * so that empty strings are visible and selectable *)
SetAttributes[visibleForm, {SequenceHold}]
visibleForm /: MakeBoxes[visibleForm[""], fmt_] := StyleBox[
	"\"\"",
	ShowStringCharacters->True
]
visibleForm /: MakeBoxes[visibleForm[u_], fmt_] := MakeBoxes[u,fmt]

(* add TPE styles to evaluation notebook stylesheet *)

styles = Cell[StyleData["ToggleSelector"],
	TemplateBoxOptions->{
		DisplayFunction->$OverlayDisplayFunction
	}
]

If[CurrentValue[EvaluationNotebook[], {StyleDefinitions, "ToggleSelector"}] === {},
	SetOptions[
		EvaluationNotebook[],
		StyleDefinitions -> Replace[CurrentValue[EvaluationNotebook[], StyleDefinitions],
			{
			Notebook[oldcells_, r__] :> Notebook[Join[oldcells, {styles}], r],
			other_ :> Notebook[Prepend[styles, Cell[StyleData[StyleDefinitions->other]]], StyleDefinitions -> "PrivateStylesheetFormatting.nb"]
			}
		]
	]
]

End[]

EndPackage[]