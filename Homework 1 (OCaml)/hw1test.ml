let my_subset_test0 = subset [1;2] [1;2;3]
let my_subset_test1 = subset [4;1;4] [1;3;4]
let my_subset_test2 = not (subset [7;2;3] [1;9;4])

let my_equal_sets_test0 = equal_sets [1;3] [3;1]
let my_equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])
let my_equal_sets_test2 = equal_sets [0;1;2] [0;1;2]

let my_set_union_test0 = equal_sets (set_union [1] [1;5;6]) [1;5;6]
let my_set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3;4]) [1;2;3;4]
let my_set_union_test2 = equal_sets (set_union [1] [2]) [1;2]

let my_set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [7;1;1] [1;3;3]) [1]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4;5] [3;1;2;4;7;8;9]) [4;3;2;1]

let my_set_diff_test0 = equal_sets (set_diff [1;9;0] [4;5;3;2]) [1;9;0]
let my_set_diff_test1 = equal_sets (set_diff [0;5] [0;5]) []
let my_set_diff_test2 = equal_sets (set_diff [7;5;2] []) [2;5;7]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 4) 1000000000000 = 0

(* An example grammar for a small subset of Awk.  *)
type simpleNTs =
  | Okay | Sure | No | Yes

let simpleG =
  Okay,
  [Yes, [];
   No, [T"kk"];
   Sure, [N Yes];
   Sure, [N No];
   Okay, [N Sure; N Okay]]

let my_filter_reachable_test0 =
  filter_reachable simpleG = simpleG
