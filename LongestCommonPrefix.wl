(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


LongestCommonPrefix // ClearAll;


(* ::Subsection:: *)
(*Messages*)


(* ::Text:: *)
(*Messages are printed using ResourceFunctionMessage:*)


LongestCommonPrefix::argm =
"`1` called with `2` arguments; `3` or more arguments are expected.";

LongestCommonPrefix::mismatch =
"Unable to compare strings with lists.";

LongestCommonPrefix::list = 
"List expected at position `2` in `1`.";

LongestCommonPrefix::seq = 
"A list of strings or a list of lists is expected at position `2` in `1`.";

LongestCommonPrefix::bdmtd = 
"Value of option Method -> `1` is not Automatic, \"BinarySearch\", or \"LinearSearch\".";


(* ::Subsection:: *)
(*Options*)


LongestCommonPrefix // Options = {
    IgnoreCase -> False,
    Method     -> Automatic
};


(* ::Subsection:: *)
(*Main definition*)


(* ::Text:: *)
(*Argument patterns:*)


$string = _String? StringQ;
$list   = _List;
$seq    = $string|$list;


list[ t_ ] := { t .. };


(* ::Text:: *)
(*For a single sequence, the longest common prefix is simply itself:*)


LongestCommonPrefix[ { single: $seq }, OptionsPattern[ ] ] := 
  single;


(* ::Text:: *)
(*Compare strings:*)


LongestCommonPrefix[ strings: list @ $string, opts: OptionsPattern[ ] ] :=
  Module[ { prefix },
      prefix = longestStringPrefix[ strings,
                                    OptionValue @ IgnoreCase,
                                    OptionValue @ Method
               ];
      prefix /; StringQ @ prefix
  ];


(* ::Text:: *)
(*Compare lists:*)


LongestCommonPrefix[ lists: list @ $list, opts: OptionsPattern[ ] ] :=
  Module[ { prefix },
      prefix = longestListPrefix[ lists, OptionValue @ Method ];
      prefix /; ListQ @ prefix
  ];


(* ::Text:: *)
(*Interpret multiple arguments as a single list:*)


LongestCommonPrefix[ a: $seq, b: $seq .., opts: OptionsPattern[ ] ] :=
  LongestCommonPrefix[ { a, b }, opts ];


(* ::Text:: *)
(*Use Values if given an association:*)


LongestCommonPrefix[ a_Association? AssociationQ, opts: OptionsPattern[ ] ] :=
  With[ { list = Values @ a },
      LongestCommonPrefix[ list, opts ] /; MatchQ[ list, { $seq .. } ]
  ];


(* ::Subsubsection:: *)
(*Error handling*)


(* ::Text:: *)
(*Wrong number of arguments:*)


LongestCommonPrefix[ ] :=
  Null /; message[ "argm", LongestCommonPrefix, 0, 1 ];


(* ::Text:: *)
(*Argument is not a list:*)


LongestCommonPrefix[ a: Except[ $list ] ] :=
  Null /; message[ "list", HoldForm @ LongestCommonPrefix @ a, 1 ];


(* ::Text:: *)
(*Check list for incompatible elements:*)


LongestCommonPrefix[ list_List ] :=
  Null /; checkListArgs @ list;


(* ::Subsection:: *)
(*Dependencies*)


(* ::Subsubsection:: *)
(*String functions*)


(* ::Subsubsubsection::Closed:: *)
(*longestStringPrefix*)


longestStringPrefix // ClearAll;


longestStringPrefix[ strings_, ignoreCase_, Automatic ] :=
  With[ { method = autoStringMethod[ strings, ignoreCase ] },
      longestStringPrefix[ strings, ignoreCase, method ] /; 
        MatchQ[ method, "BinarySearch"|"LinearSearch" ]
  ];


longestStringPrefix[ strings_, ignoreCase_, "BinarySearch" ] :=
  longestStringBinary[ strings, ignoreCase ];


longestStringPrefix[ strings_, ignoreCase_, "LinearSearch" ] :=
  longestStringLinear[ strings, ignoreCase ];


longestStringPrefix[ _, _, method_ ] :=
  message[ "bdmtd", method ];


(* ::Subsubsubsection::Closed:: *)
(*autoStringMethod*)


autoStringMethod // ClearAll;


autoStringMethod[ strings_, ignoreCase_ ] :=
  With[ { case = If[ TrueQ @ ignoreCase, ToLowerCase, Identity ] },
      If[ SameQ @@ case @ StringTake[ Take[ strings, UpTo @ 5 ], UpTo @ 5 ],
          "BinarySearch",
          "LinearSearch"
      ]
  ];


(* ::Subsubsubsection::Closed:: *)
(*longestStringLinear*)


(* ::Text:: *)
(*Linear search for longest string prefix:*)


longestStringLinear // ClearAll;


longestStringLinear[ strings_, ignoreCase_ ] :=
  Module[ { ignore, first, n = 0, max },
      ignore = TrueQ @ ignoreCase;
      first = First @ strings;
      max = Min @ StringLength @ strings;
      If[ max === 0, Throw[ "", $tag ] ];

      While[ And @@ StringStartsQ[ strings,
                                   StringTake[ first, ++n ],
                                   IgnoreCase -> ignore
                    ],
             If[ n >= max, Throw[ StringTake[ first, max ], $tag ] ]
      ];
      
      StringTake[ first, n - 1 ]
  ] ~Catch~ $tag;


(* ::Subsubsubsection::Closed:: *)
(*longestStringBinary*)


(* ::Text:: *)
(*Binary search for longest string prefix:*)


longestStringBinary // ClearAll;


longestStringBinary[ strings_, ignoreCase_ ] :=
    Module[ { low, mid, hi, case },
        low = 0;
        hi = Min @ StringLength @ strings;
        mid = low + Round[ (hi - low)/2 ];
        case = If[ TrueQ @ ignoreCase, ToLowerCase, Identity ];
        If[ hi > 0 && ! SameQ @@ case @ StringTake[ strings, 1 ], 
            Throw[ "", $tag ]
        ];
        While[ low <= mid <= hi
               ,
               Which[ 
                   low === mid === hi,
                     Throw[ StringTake[ First @ strings, mid ], $tag ]
                   ,
                   SameQ @@ case @ StringTake[ strings, mid ],
                     low = mid;
                     mid = low + Ceiling[ (hi - low)/2 ]
                   ,
                   True,
                     hi = mid - 1;
                     mid = low + Floor[ (hi - low)/2 ]
               ]
        ]
    ] ~Catch~ $tag;


(* ::Subsubsection:: *)
(*List functions*)


(* ::Subsubsubsection::Closed:: *)
(*longestListPrefix*)


longestListPrefix // ClearAll;


longestListPrefix[ lists_, Automatic ] :=
  With[ { method = autoListMethod @ lists },
      longestListPrefix[ lists, method ] /;
        MatchQ[ method, "BinarySearch"|"LinearSearch" ]
  ];


longestListPrefix[ lists_, "BinarySearch" ] :=
  longestListBinary @ lists;


longestListPrefix[ lists_, "LinearSearch" ] :=
  longestListLinear @ lists;


longestListPrefix[ _, method_ ] :=
  message[ "bdmtd", method ];


(* ::Subsubsubsection::Closed:: *)
(*autoListMethod*)


autoListMethod // ClearAll;


autoListMethod[ lists_ ] :=
  If[ SameQ @@ Take[ Take[ lists, UpTo @ 5 ], UpTo @ 5 ],
      "BinarySearch",
      "LinearSearch"
  ];


(* ::Subsubsubsection::Closed:: *)
(*longestListLinear*)


(* ::Text:: *)
(*Linear search for longest list prefix:*)


longestListLinear // ClearAll;


longestListLinear[ lists_ ] :=
  Module[ { first, n = 0, max },
      first = First @ lists;
      max = Min @ Map[ Length, lists ];
      If[ max === 0, Throw[ { }, $tag ] ];

      While[ SameQ @@ lists[[ All, ;; ++n ]],
             If[ n >= max, Throw[ first[[ ;; max ]], $tag ] ]
      ];

      first[[ ;; n-1 ]]
  ] ~Catch~ $tag;


(* ::Subsubsubsection::Closed:: *)
(*longestListBinary*)


(* ::Text:: *)
(*Binary search for longest list prefix:*)


longestListBinary // ClearAll;


longestListBinary[ lists_ ] :=
  Module[ { low, mid, hi },
      low = 0;
      hi = Min[ Length /@ lists ];
      If[ hi > 0 && ! SameQ @@ lists[[ All, 1 ]], Throw[ { }, $tag ] ];
      mid = low + Round[ (hi - low)/2 ];
      While[ low <= mid <= hi
             ,
             Which[
                 low === mid === hi, 
                     Throw[ Take[ First @ lists, mid ], $tag ]
                 ,
                 SameQ @@ lists[[ All, ;; mid ]],
                     low = mid;
                     mid = low + Ceiling[ (hi - low)/2 ]
                 ,
                 True,
                     hi = mid - 1;
                     mid = low + Floor[ (hi - low) / 2 ]
             ]
      ]
  ] ~Catch~ $tag;


(* ::Subsubsection:: *)
(*Utilities*)


(* ::Subsubsubsection::Closed:: *)
(*message*)


(* ::Text:: *)
(*Print message as a resource function:*)


message // ClearAll;


message[ tag_, args___ ] := (
    ResourceFunction[ "ResourceFunctionMessage" ][ 
        MessageName[ LongestCommonPrefix, tag ], 
        args 
    ];
    False
);


(* ::Subsubsubsection::Closed:: *)
(*checkListArgs*)


(* ::Text:: *)
(*Check a list for argument validity errors:*)


checkListArgs // ClearAll;


checkListArgs[ { OrderlessPatternSequence[ $string, $list, ___ ] } ]:=
  message[ "mismatch" ];


checkListArgs[ { } ]:=
  message[ "seq", HoldForm @ LongestCommonPrefix @ { }, 1 ];


checkListArgs[ a: { ___, Except[ $seq ], ___ } ]:=
  message[ "seq", HoldForm @ LongestCommonPrefix @ a, 1 ];


(* ::Section::Closed:: *)
(*Tests*)


$verificationTests = HoldComplete[
    VerificationTest[
        LongestCommonPrefix @ { "the cat in the hat", "the cat insists" },
        "the cat in"
    ],
    VerificationTest[
        LongestCommonPrefix @ {
            "the cat in the hat",
            "the cat insists",
            "the catapult is inferior to the trebuchet"
        },
        "the cat"
    ],
    VerificationTest[ LongestCommonPrefix @ { { 1, 2, 3 }, { 1, 2, x } }, { 1, 2 } ],
    VerificationTest[
        LongestCommonPrefix @ { { a, b, c }, { a, b, 1, 2 }, { a, b, "hello" } },
        { a, b }
    ],
    VerificationTest[
        LongestCommonPrefix @ { Fibonacci @ Range[ 3, 10 ], Prime @ Range[ 10 ] },
        { 2, 3, 5 }
    ],
    VerificationTest[
        LongestCommonPrefix[
            "the cat in the hat",
            "the cat insists",
            "the catastrophe"
        ],
        "the cat"
    ],
    VerificationTest[ LongestCommonPrefix[ { 1, 2, 3 }, { 1, 2, x } ], { 1, 2 } ],
    VerificationTest[
        LongestCommonPrefix @ <|
            "a" -> { 1, 2, 3 },
            "b" -> { 1, 2, x }
        |>,
        { 1, 2 }
    ],
    VerificationTest[ LongestCommonPrefix[ "Testing", "TEST STRING" ], "T" ],
    VerificationTest[
        LongestCommonPrefix[ "Testing", "TEST STRING", IgnoreCase -> True ],
        "Test"
    ],
    VerificationTest[
        LongestCommonPrefix[
            "TEST STRING",
            "Testing",
            "tested",
            IgnoreCase -> True
        ],
        "TEST"
    ],
    VerificationTest[
        LongestCommonPrefix[
            "tested",
            "TEST STRING",
            "Testing",
            IgnoreCase -> True
        ],
        "test"
    ],
    VerificationTest[
        LongestCommonPrefix[
            "Testing",
            "tested",
            "TEST STRING",
            IgnoreCase -> True
        ],
        "Test"
    ],
    VerificationTest[
        LongestCommonPrefix[ { "abc", "abx" }, Method -> "LinearSearch" ],
        "ab"
    ],
    VerificationTest[
        LongestCommonPrefix[ { "abc", "abx" }, Method -> "BinarySearch" ],
        "ab"
    ],
    VerificationTest[
        LongestCommonPrefix @ { "the cat in the hat" },
        "the cat in the hat"
    ],
    VerificationTest[ LongestCommonPrefix @ { Range[ 5 ] }, { 1, 2, 3, 4, 5 } ],
    VerificationTest[ LongestCommonPrefix @ { "this one", "another one" }, "" ],
    VerificationTest[ LongestCommonPrefix @ { { a, b, c }, { 1, 2, 3 } }, { } ],
    VerificationTest[
        LongestCommonPrefix @ { "", "the cat in the hat", "the cat insists", "the catastrophe" },
        ""
    ],
    VerificationTest[ LongestCommonPrefix @ { { 1, 2, 3 }, { 1, 2, x }, { } }, { } ],
    VerificationTest[
        LongestCommonSubsequence[ { 1, 2, 3, 4, 5, 6 }, { 1, 2, 3, x, 5, 6 } ],
        { 1, 2, 3 }
    ],
    VerificationTest[
        LongestCommonPrefix[ { 1, 2, 3, 4, 5, 6 }, { 1, 2, 3, x, 5, 6 } ],
        { 1, 2, 3 }
    ],
    VerificationTest[
        LongestCommonSubsequence[ { 1, 2, 3, 4, 5, 6 }, { 1, 2, x, 4, 5, 6 } ],
        { 4, 5, 6 }
    ],
    VerificationTest[
        LongestCommonPrefix[ { 1, 2, 3, 4, 5, 6 }, { 1, 2, x, 4, 5, 6 } ],
        { 1, 2 }
    ],
    VerificationTest[ LongestCommonPrefix @ { { } }, { } ],
    VerificationTest[ LongestCommonPrefix @ { "" }, "" ],
    VerificationTest[
        LongestCommonPrefix[ { "testing", "TEST STRING" }, IgnoreCase -> True ],
        "test"
    ],
    VerificationTest[
        LongestCommonPrefix[ { "TEST STRING", "testing" }, IgnoreCase -> True ],
        "TEST"
    ],
    VerificationTest[
        LongestCommonPrefix[
            { { "A", "B", "C" }, { "a", "b", "x" } },
            IgnoreCase -> True
        ],
        { }
    ],
    VerificationTest[
        LongestCommonPrefix[ { "ABC", "abx" }, IgnoreCase -> True ],
        "AB"
    ]
];


(* ::Section:: *)
(*Metadata*)


<|
    "Name" -> "LongestCommonPrefix",
    "Function" -> LongestCommonPrefix,
    "ResourceType" -> "Function",
    "Description" -> "Find the longest common contiguous prefix of a set of strings or lists (development branch)",
    "Version" -> "1.1.0",
    "Keywords" -> {
        "prefix",
        "common start",
        "unique rest",
        "drop same",
        "string drop same"
    },
    "Categories" -> { "Core Language & Structure", "Strings & Text" },
    "ContributorInformation" -> <|
        "PublisherID" -> "Wolfram",
        "DisplayName" -> "Wolfram Research",
        "ContributedBy" -> "Richard Hennigan (Wolfram Research)"
    |>,
    "RelatedSymbols" -> {
        "StringTake",
        "StringStartsQ",
        "TakeWhile",
        "LongestCommonSubsequence",
        "LongestCommonSequence",
        "SequenceAlignment",
        "NeedlemanWunschSimilarity",
        "Subsequences"
    },
    "SeeAlso" -> {
        "StringPrepend",
        "PrefixQ",
        "AllSameAs",
        "DropWhile",
        "DropTrailingWhile"
    },
    "VerificationTests" -> $verificationTests
|>
