structure Benchmark: sig
              val repeat: int -> (unit -> 'a) -> unit
              val bench: int -> (unit -> 'a) -> LargeInt.int
              val benchmark: string -> int -> (unit -> 'a) -> unit
              val benchset: string -> int -> (string * (unit -> 'a)) list -> unit
          end
=
struct
    fun repeat 0 f = ()
      | repeat n f =  (f ();repeat (n - 1) f)

    fun bench n f = let
        val startTime = Time.now ()
        val _ = repeat n f
        val endTime = Time.now ()
    in
        Time.toMilliseconds (Time.-(endTime, startTime))
    end

    fun benchmark name n f = let
        val time = bench n f
    in
        print (name ^ "\n");
        print (" Time:\n");
        print ("    [Total] " ^ (LargeInt.toString time) ^ " ms/" ^ (Int.toString n) ^ "calls\n");
        print ("  [Average] " ^ (Real.toString((Real.fromLargeInt time) / (Real.fromInt n))) ^ " ms/call\n")
    end

    fun nChars n char = CharArray.vector(CharArray.array(n, char))

    fun toWidth width str = let
        val len = String.size str
    in
        if len < width
        then str ^ (nChars (width - len) #" ")
        else str
    end

    fun histLine width base value =
        (nChars (Int.fromLarge(width * value div base)) #"*") ^ "\n"

    fun benchset name n fs = let
        val res = List.map (fn (label, f) => (label, bench n f)) fs
        val max = List.foldl (fn ((_, time), m) => LargeInt.max(time, m)) 0 res
        val maxLen = List.foldl (fn ((label, _), m) => Int.max(String.size label,  m)) 0 fs
    in
        print "name:\n";
        print ((nChars ((String.size " ") + maxLen) #"-") ^ "+" ^ (nChars ((String.size "|") +  50) #"-") ^ "\n");
        app (fn (label, time) => print(" " ^ (toWidth maxLen label) ^ "|" ^(histLine (50:LargeInt.int) max time))) res;
        print ((nChars ((String.size " ") + maxLen) #"-") ^ "+" ^ (nChars ((String.size "|") +  50) #"-") ^ "\n")
    end

end
