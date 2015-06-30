open Core_bench.Std



let pow2 n =
  let rec doit acc n =
    if n<=0 then acc else doit (Z.add acc acc) (n-1)
  in
  doit Z.one n

let p2 = Z.of_int 2
let p30 = pow2 30
let p62 = pow2 62
let p300 = pow2 300
let p120 = pow2 120
let p121 = pow2 121
let maxi = Z.of_int max_int
let mini = Z.of_int min_int
let maxi32 = Z.of_int32 Int32.max_int
let mini32 = Z.of_int32 Int32.min_int
let maxi64 = Z.of_int64 Int64.max_int
let mini64 = Z.of_int64 Int64.min_int
let maxni = Z.of_nativeint Nativeint.max_int
let minni = Z.of_nativeint Nativeint.min_int

let bench_odd_p x =
  Bench.Test.create_group ~name:(Z.to_string x)
    [
      Bench.Test.create ~name:"Z.testbit 0" (fun () ->
          ignore (Z.tstbit x 0));
      Bench.Test.create ~name:"Z.odd_p" (fun () ->
          ignore (Z.is_odd x));
    ]

let main () =
  let open Core.Std in
  Command.run (Bench.make_command [
      (bench_odd_p p30);
      (bench_odd_p p120);
      (bench_odd_p (Z.neg p120));
    ])

let () = main ()
