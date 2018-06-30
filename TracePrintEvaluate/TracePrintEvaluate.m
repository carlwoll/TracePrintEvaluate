BeginPackage["TracePrintEvaluate`"];
	
TracePrintEvaluate::usage = "TracePrintEvaluate[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"form\", \"TI\"]\)] prints an in/out toggler for \
expressions which match \!\(\*StyleBox[\"form\", \"TI\"]\). 
TracePrintEvaluate[\!\(\*StyleBox[\"expr\", \"TI\"]\), \!\(\*StyleBox[\"s\", \"TI\"]\)] includes all evaluations which use \
transformation rules associated with the symbol \!\(\*StyleBox[\"s\", \"TI\"]\)."
	
ToggleSelector::usage = "ToggleSelector[{evaluated, unevaluated..}, initial] toggles starting with initial"

Begin["`Private`"];
	
Options[TracePrintEvaluate] = SortBy[
	Join[
		FilterRules[Options[TracePrint], Except[TraceInternal|TraceAction]],
		{
			"TraceIndent" -> " ",
			"TraceIn" -> None, (* Side effect function applied to input *)
			"TraceOut" -> None, (* Side effect function applied to input and output *)
			"Timing" -> False, (* Whether to show evaluation time *)
			"Toggler" -> 1, (* Which toggler state to show initially *)
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
	toggle = Replace[OptionValue["Toggler"], Except[_Integer]->1],
	showtime = TrueQ@OptionValue["Timing"],
	in = OptionValue["TraceIn"],
	out = OptionValue["TraceOut"],
	stylesheet = CurrentValue[{StyleDefinitions, "TPE2"}] =!= {},
	indent = Replace[OptionValue["TraceIndent"], Except[_String] -> " "]
	},

	Block[{stack = {}},
		With[
			{
			tprint = With[{v = push @ index++},
				Print @ ToggleSelector[
					{
					indented[TraceLevel[]-2, indent, Defer@@#], (* input *)
					visibleForm @ Dynamic @ values[v], (* output *)
					If[showtime, Dynamic @ timing[v], Nothing],
					If[in=!=None, visibleForm @ in[#], Nothing],
					If[out=!=None, visibleForm @ Dynamic @ outAction[v], Nothing]
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
				
				"\[Bullet]",
				Initialization :> (values = vv; timing = tt; outAction = oo)
			];

			If[TrueQ @ OptionValue[SaveDefinitions],
				CellPrint @ Cell[
					BoxData @ ToBoxes @ res,
					"PersistentOutput",
					CellLabel -> "Out["<>ToString[$Line]<>"]=",
					CellDingbat -> Cell @ BoxData @ ToBoxes @ r
				];
				res;,
				
				res
			]
		]
	]
]

Options[ToggleSelector] = {"Stylesheet"->True}
ToggleSelector /: MakeBoxes[ToggleSelector[{in_, out__}, init_Integer:1, OptionsPattern[]], fmt_] := With[
	{
	name = "TPE" <> ToString@Length[Hold[in, out]],
	inboxes = MakeBoxes[in, fmt],
	outboxes = ToBoxes /@ Hold[out],
	df = If[TrueQ @ OptionValue[ToggleSelector, "Stylesheet"], Sequence@@{}, DisplayFunction -> TPEDisplayFunction[Length[{in, out}]]]
	},

	Replace[
		RotateLeft[Prepend[outboxes, inboxes], init-1],
		Hold[x__] :> TemplateBox[{x}, name, df]
	]
]

TPEDisplayFunction[n_] := With[{toggles = Table[i -> TagBox[Slot[i], event[i]], {i, n}]},
	Function @ TogglerBox[1, toggles, ImageSize->Automatic]
]

event[i_] := EventHandlerTag[{
	{"MouseClicked", 2} :> Block[{System`TemplateArgBox = #&}, CopyToClipboard @ Cell[StripBoxes @ BoxData[Replace[Slot[i], DynamicBox[x_]:>x]], "Input"]]
}]

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

styles = Append[
	Table[
		Cell[StyleData["TPE"<>ToString[n]],
			CellContext->Cell,
			TemplateBoxOptions->{DisplayFunction->TPEDisplayFunction[n]}
		],
		{n, 7}
	],
	Cell[StyleData["PersistentOutput", StyleDefinitions->StyleData["Output"]]]
]

If[CurrentValue[EvaluationNotebook[], {StyleDefinitions, "TPE2"}] === {},
	SetOptions[
		EvaluationNotebook[],
		StyleDefinitions -> Replace[CurrentValue[EvaluationNotebook[], StyleDefinitions],
			{
			Notebook[oldcells_, r__] :> Notebook[Join[oldcells, styles], r],
			other_ :> Notebook[Prepend[styles, Cell[StyleData[StyleDefinitions->other]]], StyleDefinitions -> "PrivateStylesheetFormatting.nb"]
			}
		]
	]
]

End[]

EndPackage[]