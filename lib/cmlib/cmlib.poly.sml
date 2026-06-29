(* Poly/ML build script for cmlib (subset) *)
(* Usage: poly --use lib/cmlib/cmlib.poly.sml *)

(* Suspensions — portable implementation *)
use "lib/cmlib/susp.sig";
use "lib/cmlib/susp.sml";

(* FromString / Bytestring — needed by Stream *)
use "lib/cmlib/from-string.sig";
use "lib/cmlib/from-string.sml";
use "lib/cmlib/bytestring.sig";
use "lib/cmlib/bytestring.sml";

(* Ordered types *)
use "lib/cmlib/ordered.sig";
use "lib/cmlib/ordered.sml";

(* Splay tree core *)
use "lib/cmlib/splay-tree.sml";

(* Dict and Set *)
use "lib/cmlib/dict.sig";
use "lib/cmlib/dict-splay.sml";
use "lib/cmlib/set.sig";
use "lib/cmlib/set-splay.sml";

(* Sort *)
use "lib/cmlib/sort.sig";
use "lib/cmlib/quicksort.sml";

(* Stream *)
use "lib/cmlib/stream.sig";
use "lib/cmlib/stream.sml";
